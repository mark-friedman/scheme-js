/**
 * Cross-implementation benchmark harness.
 *
 * Runs the same portable programs from `benchmarks/programs/` under scheme-js-4,
 * Gambit (`gsi`) and Racket, so our numbers have external calibration rather than
 * only being comparable to our own past selves. Implementations that are not
 * installed are skipped rather than failing the run.
 *
 * Gambit's `gsi` is an interpreter and Racket CS is a compiler, which brackets
 * the design space usefully: matching gsi means we have stopped being slow for
 * avoidable reasons, and approaching Racket means the compiler is working.
 *
 * Usage:
 *   node benchmarks/compare_implementations.js [--profile quick|canonical] [--runs N]
 */

import { execFileSync } from 'child_process';
import fs from 'fs';
import os from 'os';
import path from 'path';

import { BENCHMARKS, sizeFor } from './programs/manifest.js';
import { PROGRAM_DIR, runBenchmark } from './lib/harness.js';

/**
 * External implementations we know how to drive. Each supplies a prelude and a
 * postlude wrapped around the shared program source, and a command to run it.
 */
const IMPLEMENTATIONS = [
  {
    name: 'gambit-gsi',
    description: 'Gambit interpreter',
    command: 'gsi',
    extension: '.scm',
    wrap: (size, source) => `(define bench-size ${size})
${source}
(let ((start (real-time)))
  (let ((result (bench-run)))
    (let ((elapsed (- (real-time) start)))
      (display "RESULT ") (write result) (newline)
      (display "MS ") (display (* 1000.0 elapsed)) (newline))))
`,
    args: (file) => [file]
  },
  {
    name: 'racket',
    description: 'Racket CS (compiled)',
    command: 'racket',
    extension: '.rkt',
    wrap: (size, source) => `#lang racket/base
(define bench-size ${size})
${source}
(let ((start (current-inexact-milliseconds)))
  (let ((result (bench-run)))
    (let ((elapsed (- (current-inexact-milliseconds) start)))
      (display "RESULT ") (write result) (newline)
      (display "MS ") (display elapsed) (newline))))
`,
    args: (file) => [file]
  }
];

/**
 * Checks whether a command exists on PATH.
 *
 * Uses `which` rather than a shell built-in: passing arguments through a shell
 * is both a deprecated Node pattern and needless exposure, and the command name
 * here would be interpolated into a shell string.
 *
 * @param {string} command - The executable name.
 * @returns {boolean} True if found.
 */
function isAvailable(command) {
  try {
    execFileSync('which', [command], { stdio: 'pipe' });
    return true;
  } catch {
    return false;
  }
}

/**
 * Runs one benchmark under one external implementation.
 * @param {Object} impl - An entry from IMPLEMENTATIONS.
 * @param {Object} bench - A manifest entry.
 * @param {number} size - Size to bind to `bench-size`.
 * @param {number} runs - Number of repetitions; the median is reported.
 * @returns {{median: (number|null), result: (string|null), error: (string|null)}}
 */
function runExternal(impl, bench, size, runs) {
  const source = fs.readFileSync(path.join(PROGRAM_DIR, bench.file), 'utf8');
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'schemebench-'));
  const file = path.join(dir, `${bench.name}${impl.extension}`);
  fs.writeFileSync(file, impl.wrap(size, source));

  const times = [];
  let result = null;
  let error = null;

  try {
    for (let i = 0; i < runs; i++) {
      const out = execFileSync(impl.command, impl.args(file), {
        encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'], timeout: 300000
      });
      const resultMatch = out.match(/RESULT (.*)/);
      const msMatch = out.match(/MS ([0-9.eE+-]+)/);
      if (!resultMatch || !msMatch) {
        throw new Error(`unparseable output: ${out.slice(0, 200)}`);
      }
      result = resultMatch[1].trim();
      times.push(parseFloat(msMatch[1]));
    }
  } catch (e) {
    error = (e.stderr ? String(e.stderr).split('\n')[0] : e.message).slice(0, 120);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }

  times.sort((a, b) => a - b);
  return {
    median: times.length > 0 ? times[Math.floor(times.length / 2)] : null,
    result,
    error
  };
}

/**
 * Normalizes a rendered result for cross-implementation comparison. Gambit and
 * Racket print pairs as `(200 . 200)` and booleans as `#t`/`#f`, which already
 * matches `renderResult`, but whitespace and exactness markers can differ.
 * @param {string|null} value - A rendered result.
 * @returns {string|null} A normalized form.
 */
function normalize(value) {
  if (value === null || value === undefined) return null;
  return String(value).replace(/\s+/g, ' ').trim();
}

async function main() {
  const args = process.argv.slice(2);
  const profileIndex = args.indexOf('--profile');
  const profile = profileIndex >= 0 ? args[profileIndex + 1] : 'quick';
  const runsIndex = args.indexOf('--runs');
  const runs = runsIndex >= 0 ? parseInt(args[runsIndex + 1], 10) : 3;

  const available = IMPLEMENTATIONS.filter(i => isAvailable(i.command));
  const missing = IMPLEMENTATIONS.filter(i => !available.includes(i));

  console.log('='.repeat(78));
  console.log(`Cross-implementation benchmarks  (profile: ${profile}, runs: ${runs})`);
  console.log('='.repeat(78));
  console.log(`Node ${process.version} on ${os.platform()}/${os.arch()}`);
  for (const impl of available) console.log(`  available: ${impl.name} (${impl.description})`);
  for (const impl of missing) console.log(`  SKIPPED:   ${impl.name} (${impl.command} not on PATH)`);
  console.log('');

  const results = {};

  for (const bench of BENCHMARKS) {
    const size = sizeFor(bench, profile);
    const row = { size, scheme_js_4: null, results: {} };

    process.stdout.write(`  ${bench.name} (size ${size}) `);

    const ours = runBenchmark(bench, size, runs);
    row.scheme_js_4 = ours.median;
    row.results['scheme-js-4'] = normalize(ours.result);
    if (ours.error) row.error = ours.error;
    process.stdout.write('.');

    for (const impl of available) {
      const r = runExternal(impl, bench, size, runs);
      row[impl.name] = r.median;
      row.results[impl.name] = normalize(r.result);
      if (r.error) row[`${impl.name}_error`] = r.error;
      process.stdout.write('.');
    }

    // Cross-implementation agreement is the real correctness check: we do not
    // need a hardcoded expected value if three implementations concur.
    const values = Object.values(row.results).filter(v => v !== null);
    row.agree = values.length > 1 && values.every(v => v === values[0]);

    results[bench.name] = row;
    console.log(` ${row.agree ? 'agree' : 'DISAGREE'}  ${JSON.stringify(row.results)}`);
  }

  console.log('');
  console.log('| Benchmark | size | scheme-js-4 | gambit-gsi | racket | vs gsi | vs racket |');
  console.log('|-----------|------|-------------|------------|--------|--------|-----------|');
  for (const [name, row] of Object.entries(results)) {
    const fmt = (v) => v === null || v === undefined ? '-' : `${v.toFixed(1)} ms`;
    const ratio = (v) => (v === null || v === undefined || !row.scheme_js_4 || v === 0)
      ? '-' : `${(row.scheme_js_4 / v).toFixed(0)}x`;
    console.log(
      `| ${name.padEnd(9)} | ${String(row.size).padEnd(4)} | ${fmt(row.scheme_js_4).padEnd(11)} ` +
      `| ${fmt(row['gambit-gsi']).padEnd(10)} | ${fmt(row.racket).padEnd(6)} ` +
      `| ${ratio(row['gambit-gsi']).padEnd(6)} | ${ratio(row.racket).padEnd(9)} |`
    );
  }

  const disagreements = Object.entries(results).filter(([, r]) => !r.agree);
  if (disagreements.length > 0) {
    console.log('');
    console.log('RESULT DISAGREEMENTS (investigate before trusting timings):');
    for (const [name, row] of disagreements) {
      console.log(`  ${name}: ${JSON.stringify(row.results)}`);
      for (const key of Object.keys(row)) {
        if (key.endsWith('_error') || key === 'error') console.log(`    ${key}: ${row[key]}`);
      }
    }
  }

  console.log('');
  console.log('--- JSON Results ---');
  console.log(JSON.stringify({ profile, runs, nodeVersion: process.version, results }, null, 2));
}

main().catch(err => {
  console.error('Cross-implementation comparison failed:', err);
  process.exit(1);
});
