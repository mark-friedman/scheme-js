/**
 * Runs the real-code workload under every available Scheme implementation.
 *
 * ## Why this is worth doing
 *
 * The obvious use is comparison: how far behind are we on real code rather than
 * on eight microbenchmarks. The less obvious and more valuable use is as a
 * **validity check on the benchmark itself**. If a program is relatively
 * expensive for us *and* relatively expensive for Gambit and Racket, the
 * benchmark is measuring something intrinsic to the program. If it is expensive
 * only for us, the benchmark is measuring our implementation -- which is useful
 * too, but is a different claim, and one that a single-implementation number
 * cannot distinguish.
 *
 * ## Workload
 *
 * The project's own Scheme test files. Most are portable R7RS; the handful that
 * reach for JavaScript interop or `garbage-collect` are excluded by name rather
 * than silently skipped.
 *
 * Each file is timed *inside* the Scheme program, with R7RS `current-jiffy`, and
 * the body is repeated so the measurement is well clear of clock granularity.
 * Timing the process instead does not work here: these files do one to three
 * milliseconds of work against a twenty-millisecond process launch, so
 * subtracting a measured startup clamped every result to zero.
 *
 * Racket needs its R7RS language to participate: `raco pkg install r7rs`.
 * Without it, Racket is reported as unavailable rather than silently omitted.
 *
 * Usage: node benchmarks/compare_macro.js [--runs N] [--files N]
 */

import { execFileSync } from 'child_process';
import fs from 'fs';
import os from 'os';
import path from 'path';
import { fileURLToPath } from 'url';

import { createInterpreter } from '../src/core/interpreter/index.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { parse } from '../src/core/interpreter/reader.js';

const PROJECT_ROOT = path.join(path.dirname(fileURLToPath(import.meta.url)), '..');
const TEST_DIR = path.join(PROJECT_ROOT, 'tests', 'core', 'scheme');

const args = process.argv.slice(2);
const valueOf = (f, d) => { const i = args.indexOf(f); return i >= 0 ? args[i + 1] : d; };
const RUNS = parseInt(valueOf('--runs', '3'), 10);
const FILE_LIMIT = parseInt(valueOf('--files', '999'), 10);

/**
 * Files excluded from the portable workload, with the reason. Listed rather
 * than detected, so an exclusion is a decision someone made and can revisit.
 */
const EXCLUDED = {
  'test.scm': 'the harness itself, loaded with every file',
  'dynamic_wind_interop_tests.scm': 'JavaScript interop',
  'js_interop_tests.scm': 'JavaScript interop',
  'promise_tests.scm': 'JavaScript promises',
  'tco_tests.scm': 'needs a host GC hook, and its million-iteration loop would dominate any total'
};

/** Portable no-op definitions for the hooks our harness reports through. */
const SHIM = `(define (native-report-test-result a b c d) #f)
(define (native-log-title t) #f)
(define (native-report-test-skip a b) #f)
`;

/** How many times each file's body is repeated inside one timed region. */
const REPEATS = 100;

/**
 * Wraps a workload body in a portable timing harness.
 *
 * The body is placed in a thunk and run `REPEATS` times inside one timed
 * region, then the per-iteration time is printed. `current-jiffy` and
 * `jiffies-per-second` are R7RS, so the same source works under every
 * implementation that claims R7RS support.
 *
 * @param {string} harness - The test harness source.
 * @param {string} body - The workload file's source.
 * @returns {string} A self-timing Scheme program.
 */
function timedProgram(harness, body) {
  // The trailing `#t` matters: a lambda body consisting only of definitions is
  // illegal in R7RS, and several of these files are definitions throughout.
  return `${harness}
(define (bench-body)
${body}
#t)
(define jps (jiffies-per-second))
(define start (current-jiffy))
(let loop ((i 0))
  (if (< i ${REPEATS})
      (begin (bench-body) (loop (+ i 1)))))
(define elapsed (- (current-jiffy) start))
(display "ELAPSED_MS ")
(display (/ (* 1000.0 elapsed) (* jps ${REPEATS})))
(newline)
`;
}

const IMPLEMENTATIONS = [
  {
    name: 'gambit-gsi', label: 'Gambit gsi (interpreter)', command: 'gsi', extension: '.scm',
    wrap: (body) => body,
    args: (file) => [file]
  },
  {
    name: 'racket', label: 'Racket CS (compiled)', command: 'racket', extension: '.rkt',
    wrap: (body) => `#lang r7rs\n(import (scheme base) (scheme write) (scheme char)\n        (scheme inexact) (scheme time) (scheme complex))\n${body}`,
    args: (file) => [file]
  }
];

/**
 * Checks whether a command exists on PATH.
 * @param {string} command - Executable name.
 * @returns {boolean} True if found.
 */
function isAvailable(command) {
  try { execFileSync('which', [command], { stdio: 'pipe' }); return true; } catch { return false; }
}

/**
 * Times a command, returning milliseconds, or null if it failed.
 * @param {Object} impl - The implementation entry.
 * @param {string} file - Path to the program.
 * @returns {{ms: number|null, error: string|null}} The timing.
 */
function timeCommand(impl, file) {
  try {
    const out = execFileSync(impl.command, impl.args(file), {
      stdio: 'pipe', timeout: 180000, encoding: 'utf8'
    });
    const match = out.match(/ELAPSED_MS ([0-9.eE+-]+)/);
    if (!match) return { ms: null, error: 'program produced no timing' };
    return { ms: parseFloat(match[1]), error: null };
  } catch (e) {
    const detail = (e.stderr ? String(e.stderr) : e.message).split('\n')[0];
    return { ms: null, error: detail.slice(0, 110) };
  }
}

/**
 * Writes a program for one implementation and times it, subtracting that
 * implementation's measured startup cost.
 * @param {Object} impl - Implementation entry.
 * @param {string} body - Scheme source.
 * @param {number} startupMs - Measured startup for this implementation.
 * @param {string} dir - Scratch directory.
 * @param {string} tag - Filename stem.
 * @returns {{ms: number|null, error: string|null}} Evaluation time.
 */
function runProgram(impl, body, dir, tag) {
  const file = path.join(dir, tag + impl.extension);
  fs.writeFileSync(file, impl.wrap(body));
  const times = [];
  let error = null;
  for (let i = 0; i < RUNS; i++) {
    const r = timeCommand(impl, file);
    if (r.ms === null) { error = r.error; break; }
    times.push(r.ms);
  }
  if (times.length === 0) return { ms: null, error };
  times.sort((a, b) => a - b);
  return { ms: times[Math.floor(times.length / 2)], error: null };
}

/**
 * Runs one workload file under our own implementation, in process.
 * @param {string} harness - Harness source to load first.
 * @param {string} body - The test file's source.
 * @returns {{ms: number|null, error: string|null}} Evaluation time.
 */
function runOurs(harness, body) {
  const times = [];
  for (let i = 0; i < RUNS; i++) {
    const { interpreter, env } = createInterpreter();
    const saved = { log: console.log, error: console.error, write: process.stdout.write };
    console.log = console.error = () => {};
    process.stdout.write = () => true;
    try {
      // Bootstrapping is outside the timed region, matching the startup
      // subtraction applied to the external implementations.
      for (const name of ['macros', 'equality', 'cxr', 'numbers', 'list', 'control']) {
        for (const form of parse(fs.readFileSync(`${PROJECT_ROOT}/src/core/scheme/${name}.scm`, 'utf8'))) {
          interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
        }
      }
      for (const form of parse(harness)) {
        interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
      }
      // Parsed *and analyzed* before the timed region. The other
      // implementations run compiled code inside their loop, so leaving
      // analysis inside ours would charge us for work they do once -- an
      // earlier version did exactly that and made us look several times worse
      // than we are.
      const asts = parse(body).map((form) => analyze(form));
      const start = performance.now();
      for (let rep = 0; rep < REPEATS; rep++) {
        for (const ast of asts) {
          interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
        }
      }
      times.push((performance.now() - start) / REPEATS);
    } catch (e) {
      Object.assign(console, { log: saved.log, error: saved.error });
      process.stdout.write = saved.write;
      return { ms: null, error: e.message.slice(0, 110) };
    } finally {
      Object.assign(console, { log: saved.log, error: saved.error });
      process.stdout.write = saved.write;
    }
  }
  times.sort((a, b) => a - b);
  return { ms: times[Math.floor(times.length / 2)], error: null };
}

function main() {
  const harness = SHIM + fs.readFileSync(path.join(TEST_DIR, 'test.scm'), 'utf8');
  const names = fs.readdirSync(TEST_DIR)
    .filter((f) => f.endsWith('.scm') && EXCLUDED[f] === undefined)
    .sort().slice(0, FILE_LIMIT);

  const available = IMPLEMENTATIONS.filter((i) => isAvailable(i.command));
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'macro-compare-'));

  console.log('='.repeat(84));
  console.log('Real-code workload across Scheme implementations');
  console.log('='.repeat(84));
  console.log(`${names.length} of the project's own test files, median of ${RUNS} runs`);
  console.log(`Excluded: ${Object.entries(EXCLUDED).map(([f, why]) => `${f} (${why})`).join('; ')}`);
  console.log('');

  // A trivial self-timing program confirms the implementation can run this
  // workload's shape, and reports its clock resolution. Resolution matters
  // here: these files do tens to hundreds of microseconds of work, and an
  // implementation whose clock ticks every millisecond cannot measure that
  // however many times the body is repeated.
  for (const impl of available) {
    const probe = runProgram(impl, timedProgram('', '(define probe 1)'), dir, 'probe');
    impl.usable = probe.error === null;
    if (!impl.usable) {
      console.log(`  ${impl.label}: cannot run the workload -- ${probe.error}`);
      continue;
    }
    const clockFile = path.join(dir, 'clock' + impl.extension);
    fs.writeFileSync(clockFile, impl.wrap('(display "ELAPSED_MS ")(display (jiffies-per-second))(newline)'));
    const jps = timeCommand(impl, clockFile).ms;
    impl.resolutionUs = jps ? (1e6 / jps) / REPEATS : null;
    console.log(`  ${impl.label}: available, clock ${jps ? jps.toLocaleString() : '?'} jiffies/s ` +
      `(effective resolution ${impl.resolutionUs ? impl.resolutionUs.toFixed(2) : '?'} us per iteration)`);
  }
  for (const impl of IMPLEMENTATIONS.filter((i) => !available.includes(i))) {
    console.log(`  ${impl.label}: not on PATH, skipped`);
  }
  console.log('');

  const rows = [];
  for (const name of names) {
    const body = fs.readFileSync(path.join(TEST_DIR, name), 'utf8');
    const row = { file: name, ours: runOurs(harness, body) };
    for (const impl of available) {
      row[impl.name] = impl.usable
        ? runProgram(impl, timedProgram(harness, body), dir, name.replace('.scm', ''))
        : { ms: null, error: 'implementation cannot run this workload' };
    }
    rows.push(row);
    process.stdout.write('.');
  }
  console.log('\n');

  const header = ['file', 'scheme-js-4', ...available.map((i) => i.name), ...available.map((i) => 'vs ' + i.name)];
  console.log('| ' + header.join(' | ') + ' |');
  console.log('|' + header.map(() => '---').join('|') + '|');

  const ratios = Object.fromEntries(available.map((i) => [i.name, []]));
  for (const row of rows) {
    // Reported in microseconds: with the body repeated, a file's work is tens
    // to hundreds of microseconds, and milliseconds to one decimal would round
    // most of the suite to zero.
    const us = (ms) => ms === null ? 'ERROR' : (1000 * ms).toFixed(0) + ' us';
    const cells = [row.file, us(row.ours.ms)];
    for (const impl of available) cells.push(us(row[impl.name].ms));
    for (const impl of available) {
      const theirs = row[impl.name].ms;
      // 5 microseconds per iteration is the floor below which the clock, not
      // the program, is what is being measured.
      if (row.ours.ms !== null && theirs !== null && theirs > 0.005 && row.ours.ms > 0.005) {
        const ratio = row.ours.ms / theirs;
        ratios[impl.name].push(ratio);
        cells.push(ratio.toFixed(1) + 'x');
      } else {
        cells.push('—');
      }
    }
    console.log('| ' + cells.join(' | ') + ' |');
  }

  console.log('');
  for (const impl of available) {
    const list = ratios[impl.name];
    if (list.length === 0) { console.log(`vs ${impl.label}: no comparable files`); continue; }
    const geo = Math.exp(list.reduce((a, r) => a + Math.log(r), 0) / list.length);
    const sorted = [...list].sort((a, b) => a - b);
    console.log(`vs ${impl.label}: geometric mean ${geo.toFixed(1)}x slower ` +
      `(range ${sorted[0].toFixed(1)}x to ${sorted[sorted.length - 1].toFixed(1)}x, ` +
      `${list.length} comparable files)`);
    // A figure built from one- and two-tick readings is arithmetic, not
    // measurement, and saying so is cheaper than someone later quoting it.
    if (impl.resolutionUs !== null && impl.resolutionUs > 5) {
      console.log(`  LOW CONFIDENCE: this clock resolves ${impl.resolutionUs.toFixed(0)} us per ` +
        `iteration, and most files here do less than 300 us of work, so many readings are only a ` +
        `few ticks. Treat the figure as an order of magnitude.`);
    }
  }

  console.log('');
  console.log('For contrast, on the eight microbenchmarks we measured 7-14x slower than gsi.');
  console.log('A large divergence between those figures would mean the microbenchmarks are');
  console.log('measuring something the real workload does not.');

  fs.rmSync(dir, { recursive: true, force: true });

  console.log('');
  console.log('--- JSON Results ---');
  console.log(JSON.stringify({ files: names.length, runs: RUNS, rows, ratios }, null, 2));
}

main();
