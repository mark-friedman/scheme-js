/**
 * Runs the canonical R7RS suite under every available Scheme implementation.
 *
 * ## Why this exists
 *
 * Two reasons, and the second is the more valuable one.
 *
 * The obvious reason is standing: how far behind Gambit and Racket are we, on
 * programs nobody here chose. The less obvious reason is that cross-
 * implementation agreement is a **validity check on the benchmark itself**. If
 * a program is relatively expensive for us *and* relatively expensive for
 * Gambit and Racket, it is measuring something intrinsic to the program. If it
 * is expensive only for us, it is measuring our implementation -- also useful,
 * but a different claim, and one a single implementation's numbers cannot tell
 * apart. That distinction is exactly what the eight microbenchmarks got wrong:
 * they were accurate about our standing and inaccurate about which
 * optimizations help (R23).
 *
 * These programs make the check sharper than the project's own Scheme could,
 * because `ecraven/r7rs-benchmarks` publishes results for more than twenty
 * implementations. A Gambit or Racket number that disagrees with the published
 * one is evidence about *this harness*, before it is evidence about anything
 * else.
 *
 * ## Measurement
 *
 * Every implementation is timed inside the Scheme program by
 * `run-r7rs-benchmark`, using R7RS `current-jiffy`, on the same source. The
 * reference implementations run under upstream's own preludes, unmodified.
 *
 * Repetition counts are **calibrated per implementation** and results reported
 * per iteration. This matters more than it sounds: Racket's clock ticks a
 * thousand times a second, so a count giving this interpreter a second of work
 * gives Racket one or two ticks, and a two-tick measurement is not a
 * measurement. Three earlier comparisons in this project were wrong for
 * precisely this family of reasons (R24), so the clock is probed and
 * inadequate resolution is labelled rather than assumed away.
 *
 * Racket needs its R7RS language: `raco pkg install r7rs`.
 *
 * Usage: node benchmarks/compare_r7rs.js [--profile default|full]
 *                                        [--target SECONDS] [--only name,name]
 */

import { execFileSync } from 'child_process';
import fs from 'fs';
import os from 'os';
import path from 'path';

import { fileURLToPath } from 'url';

import { selectBenchmarks } from './r7rs/manifest.js';
import { R7RS_DIR, buildInput, parseCsvLine, calibrate } from './lib/r7rs_harness.js';

const WORKER = path.join(path.dirname(fileURLToPath(import.meta.url)), 'lib', 'r7rs_worker.js');

const args = process.argv.slice(2);
const valueOf = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i >= 0 ? args[i + 1] : fallback;
};

const PROFILE = valueOf('--profile', 'default');
const TARGET = parseFloat(valueOf('--target', '1.0'));
const ONLY = valueOf('--only', null);

/** Wall-clock budget for one measurement, in seconds. */
const BUDGET = parseFloat(valueOf('--budget', '180'));

/**
 * Reference implementations, each with the prelude upstream uses for it.
 *
 * The preludes are vendored unmodified in `r7rs/src/`. Substituting our own
 * would mean measuring a different program than the published results measure.
 * @type {Array<Object>}
 */
const REFERENCES = [
  {
    name: 'gambit-gsi',
    label: 'Gambit gsi (interpreter)',
    command: 'gsi',
    prelude: 'GambitC-prelude.scm',
    extension: '.scm'
  },
  {
    name: 'racket',
    label: 'Racket CS (compiled)',
    command: 'racket',
    prelude: 'Racket-prelude.scm',
    extension: '.rkt'
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
 * Asks an implementation how fine its clock is.
 *
 * Reported with every result. A time is only as meaningful as the clock behind
 * it, and two implementations here differ by three orders of magnitude.
 *
 * @param {Object} ref - A reference implementation entry.
 * @param {string} dir - Scratch directory.
 * @returns {number|null} Jiffies per second, or null if it could not be asked.
 */
function probeClock(ref, dir) {
  const prelude = fs.readFileSync(path.join(R7RS_DIR, 'src', ref.prelude), 'utf8');
  const file = path.join(dir, `clock${ref.extension}`);
  fs.writeFileSync(file, `${prelude}\n(import (scheme time) (scheme write))\n(write (jiffies-per-second))\n`);
  try {
    const out = execFileSync(ref.command, [file], { stdio: 'pipe', encoding: 'utf8', timeout: 60000 });
    const value = parseInt(out.trim(), 10);
    return Number.isFinite(value) ? value : null;
  } catch {
    return null;
  }
}

/**
 * Runs one benchmark under one reference implementation.
 * @param {Object} ref - A reference implementation entry.
 * @param {Object} bench - Manifest entry.
 * @param {number} count - Repetitions.
 * @param {string} dir - Scratch directory.
 * @returns {{seconds: (number|null), incorrect: boolean, error: (string|null)}} Outcome.
 */
function runReference(ref, bench, count, dir) {
  const prelude = fs.readFileSync(path.join(R7RS_DIR, 'src', ref.prelude), 'utf8');
  const common = fs.readFileSync(path.join(R7RS_DIR, 'src', 'common.scm'), 'utf8');
  const postlude = fs.readFileSync(path.join(R7RS_DIR, 'src', 'common-postlude.scm'), 'utf8');
  // The reference implementations get the program with its `(import ...)`
  // intact -- unlike us, they support it, and removing it would be a change to
  // the program under measurement.
  const program = fs.readFileSync(path.join(R7RS_DIR, 'src', `${bench.name}.scm`), 'utf8');
  const file = path.join(dir, `${bench.name}${ref.extension}`);
  fs.writeFileSync(file, [prelude, program, common, postlude].join('\n'));

  const inputFile = path.join(dir, `${bench.name}.input`);
  fs.writeFileSync(inputFile, buildInput(bench.name, bench.params, count));

  try {
    const out = execFileSync(ref.command, [file], {
      stdio: ['pipe', 'pipe', 'pipe'],
      encoding: 'utf8',
      timeout: 600000,
      cwd: R7RS_DIR,
      input: fs.readFileSync(inputFile, 'utf8')
    });
    const parsed = parseCsvLine(out);
    return { seconds: parsed.seconds, incorrect: parsed.incorrect, error: null };
  } catch (e) {
    return { seconds: null, incorrect: false, error: (e.message || 'failed').slice(0, 80) };
  }
}

/**
 * Measures one benchmark under one reference, calibrating the count first.
 * @param {Object} ref - A reference implementation entry.
 * @param {Object} bench - Manifest entry.
 * @param {string} dir - Scratch directory.
 * @returns {{seconds: (number|null), count: number, error: (string|null),
 *   incorrect: boolean}} Per-iteration seconds and supporting detail.
 */
function measureReference(ref, bench, dir) {
  const c = calibrate((count) => runReference(ref, bench, count, dir), TARGET);
  return {
    seconds: c.seconds,
    count: c.count,
    error: c.unmeasurable ? 'below clock resolution' : c.result.error,
    incorrect: c.result.incorrect
  };
}

/**
 * Runs one measurement of ours in a child process, under a wall-clock budget.
 *
 * The reference implementations already get a budget from `execFileSync`'s
 * timeout; ours needs the same treatment for the same reason, and a benchmark
 * that hangs should be reported as hanging rather than stalling the run.
 *
 * @param {Object} bench - Manifest entry.
 * @param {number} count - Repetitions.
 * @returns {{seconds: (number|null), incorrect: boolean, error: (string|null)}} Outcome.
 */
function runOurs(bench, count) {
  const request = JSON.stringify({
    name: bench.name, params: bench.params, count, useCompiler: false
  });
  try {
    return JSON.parse(execFileSync(process.execPath, [WORKER, request], {
      stdio: ['pipe', 'pipe', 'pipe'], encoding: 'utf8', timeout: BUDGET * 1000
    }));
  } catch (e) {
    const killed = e.killed || e.signal === 'SIGTERM';
    return {
      seconds: null, incorrect: false,
      error: killed ? `exceeded the ${BUDGET}s budget` : (e.message || 'failed').slice(0, 80)
    };
  }
}

/**
 * Measures one benchmark under this implementation, calibrating the count.
 * @param {Object} bench - Manifest entry.
 * @returns {{seconds: (number|null), count: number, error: (string|null),
 *   incorrect: boolean}} Per-iteration seconds and supporting detail.
 */
function measureOurs(bench) {
  const c = calibrate((count) => runOurs(bench, count), TARGET);
  return {
    seconds: c.seconds,
    count: c.count,
    error: c.unmeasurable ? 'below clock resolution' : c.result.error,
    incorrect: c.result.incorrect
  };
}

/**
 * Geometric mean of a list of ratios.
 * @param {number[]} values - Positive ratios.
 * @returns {number|null} The geometric mean, or null if empty.
 */
function geometricMean(values) {
  if (values.length === 0) return null;
  return Math.exp(values.reduce((acc, v) => acc + Math.log(v), 0) / values.length);
}

/**
 * Entry point.
 * @returns {void}
 */
function main() {
  let benchmarks = selectBenchmarks(PROFILE);
  if (ONLY) {
    const wanted = new Set(ONLY.split(','));
    benchmarks = benchmarks.filter((b) => wanted.has(b.name));
  }

  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-r7rs-compare-'));
  const available = REFERENCES.filter((r) => isAvailable(r.command));

  console.log(`Canonical R7RS suite across implementations -- profile '${PROFILE}', `
    + `${benchmarks.length} programs`);
  console.log('');
  console.log(`  scheme-js-4 (interpreter): clock 1,000 jiffies/s`);
  for (const ref of REFERENCES) {
    if (!available.includes(ref)) {
      console.log(`  ${ref.label}: NOT AVAILABLE (${ref.command} not on PATH)`);
      continue;
    }
    const clock = probeClock(ref, dir);
    console.log(`  ${ref.label}: clock ${clock ? clock.toLocaleString() : 'unknown'} jiffies/s`);
  }
  console.log('');

  const rows = [];
  for (const bench of benchmarks) {
    const ours = measureOurs(bench);
    const refs = {};
    for (const ref of available) refs[ref.name] = measureReference(ref, bench, dir);

    rows.push({ name: bench.name, workload: bench.workload, ours, refs });

    const cells = available.map((ref) => {
      const r = refs[ref.name];
      if (!r || r.seconds === null || ours.seconds === null) return '--'.padStart(9);
      return `${(ours.seconds / r.seconds).toFixed(1)}x`.padStart(9);
    });
    const problem = ours.error ? ours.error.slice(0, 40) : (ours.incorrect ? 'WRONG ANSWER' : '');
    console.log(`  ${bench.name.padEnd(12)} ${bench.workload.padEnd(13)}`
      + ` ${(ours.seconds === null ? '--' : (ours.seconds * 1000).toFixed(1) + ' ms').padStart(10)}`
      + cells.join('') + (problem ? '  ' + problem : ''));
  }

  console.log('');
  console.log('=== How far behind, by workload class ===');
  console.log('');
  console.log('Per class, never blended: there is no average Scheme program to weight these');
  console.log('against, and one number is what hid the overfitting of the earlier suite.');
  console.log('');

  const classes = [...new Set(rows.map((r) => r.workload))];
  for (const ref of available) {
    console.log(`  vs ${ref.label}:`);
    for (const workload of classes) {
      const ratios = rows
        .filter((r) => r.workload === workload && r.ours.seconds !== null
          && r.refs[ref.name] && r.refs[ref.name].seconds !== null)
        .map((r) => r.ours.seconds / r.refs[ref.name].seconds);
      if (ratios.length === 0) continue;
      const mean = geometricMean(ratios);
      const sorted = [...ratios].sort((a, b) => a - b);
      console.log(`    ${workload.padEnd(13)} ${mean.toFixed(1)}x slower`.padEnd(36)
        + ` (${sorted[0].toFixed(1)}x to ${sorted[sorted.length - 1].toFixed(1)}x,`
        + ` ${ratios.length} programs)`);
    }
    console.log('');
  }

  fs.rmSync(dir, { recursive: true, force: true });

  console.log('--- JSON Results ---');
  console.log(JSON.stringify({
    profile: PROFILE,
    target: TARGET,
    rows: rows.map((r) => ({
      name: r.name,
      workload: r.workload,
      oursSeconds: r.ours.seconds,
      oursCount: r.ours.count,
      references: Object.fromEntries(Object.entries(r.refs).map(([k, v]) =>
        [k, { seconds: v.seconds, count: v.count }]))
    }))
  }, null, 2));
}

main();
