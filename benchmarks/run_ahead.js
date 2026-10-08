/**
 * Runs the canonical R7RS suite compiled ahead of time, beside the compiled
 * tier.
 *
 * A program compiled ahead of time (`scripts/build_ahead.scm`) runs with no
 * interpreter: its captures and its moves of frames to the heap are finished
 * by the driver of the runtime's own (`runAhead` in
 * src/core/interpreter/unwind.js), where under the tier the interpreter
 * beneath finishes moves. Each benchmark is calibrated under the tier, as
 * `run_r7rs.js` calibrates it, then built ahead of time at the same count and
 * run in a process of its own that loads nothing but the runtime; the two
 * times per iteration are compared. A program the build refuses is reported
 * with the build's reasons.
 *
 * The program's input, which the canonical harness reads through a string
 * port, is written into the program as data (`assembleAhead`), since a
 * program compiled ahead of time has no reader.
 *
 * Usage:
 *   node benchmarks/run_ahead.js [--profile default|full] [--target SECONDS] [--only name,name]
 */

import { execFileSync } from 'child_process';
import fs from 'fs';
import os from 'os';
import path from 'path';
import { fileURLToPath } from 'url';

import { selectBenchmarks } from './r7rs/manifest.js';
import { calibrate, assembleAhead } from './lib/r7rs_harness.js';

const HERE = path.dirname(fileURLToPath(import.meta.url));
const ROOT = path.join(HERE, '..');
const R7RS_WORKER = path.join(HERE, 'lib', 'r7rs_worker.js');
const AHEAD_WORKER = path.join(HERE, 'lib', 'ahead_worker.js');

const args = process.argv.slice(2);
const valueOf = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i >= 0 ? args[i + 1] : fallback;
};
const PROFILE = valueOf('--profile', 'default');
const TARGET = parseFloat(valueOf('--target', '1.0'));
const ONLY = valueOf('--only', null);
const BUDGET_MS = 300000;

/**
 * One measurement under the compiled tier, in a child process.
 * @param {Object} bench - Manifest entry.
 * @param {number} count - Repetitions.
 * @returns {{seconds: (number|null)}}
 */
function underTier(bench, count) {
  const request = JSON.stringify({ name: bench.name, params: bench.params, count, useCompiler: true });
  return JSON.parse(execFileSync(process.execPath, ['--expose-gc', R7RS_WORKER, request],
    { timeout: BUDGET_MS, maxBuffer: 1 << 26 }).toString());
}

/**
 * Builds a benchmark ahead of time at a count and runs it.
 * @param {Object} bench - Manifest entry.
 * @param {number} count - Repetitions.
 * @param {string} dir - Where to write the program and its table.
 * @returns {{seconds: (number|null), incorrect: boolean, error: (string|null),
 *   refusals: string[], bytes: number}}
 */
function ahead(bench, count, dir) {
  const program = path.join(dir, `${bench.name}.scm`);
  const table = path.join(dir, `${bench.name}.js`);
  fs.writeFileSync(program, assembleAhead(bench.name, bench.params, count, 'scheme-js-4-ahead'));
  try {
    execFileSync(process.execPath, ['repl.js', '-I', 'scripts/lib', 'scripts/build_ahead.scm', program, table],
      { cwd: ROOT, stdio: ['ignore', 'pipe', 'pipe'], timeout: BUDGET_MS });
  } catch (e) {
    return { seconds: null, incorrect: false, error: null, refusals: String(e.stderr).trim().split('\n'), bytes: 0 };
  }
  const run = JSON.parse(execFileSync(process.execPath, ['--expose-gc', AHEAD_WORKER, table],
    { timeout: BUDGET_MS, maxBuffer: 1 << 26 }).toString());
  return { ...run, refusals: [], bytes: fs.statSync(table).size };
}

const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-ahead-bench-'));
const only = ONLY === null ? null : ONLY.split(',');
console.log('per iteration       tier         ahead     ahead/tier   table');
try {
  for (const bench of selectBenchmarks(PROFILE)) {
    if (only !== null && !only.includes(bench.name)) continue;
    const name = bench.name.padEnd(12);
    const tier = calibrate((count) => underTier(bench, count), TARGET);
    if (tier.seconds === null) {
      console.log(`${name} the tier did not finish: ${tier.result.error ?? 'incorrect'}`);
      continue;
    }
    const run = ahead(bench, tier.count, dir);
    if (run.refusals.length > 0) {
      console.log(`${name} refused: ${run.refusals.join('; ')}`);
      continue;
    }
    if (run.seconds === null) {
      console.log(`${name} ahead of time did not finish: ${run.error ?? 'incorrect'}`);
      continue;
    }
    const perAhead = run.seconds / tier.count;
    console.log(`${name} ${(tier.seconds * 1000).toFixed(3).padStart(10)} ms ${(perAhead * 1000).toFixed(3).padStart(10)} ms`
      + `   ${(perAhead / tier.seconds).toFixed(2).padStart(5)}   ${Math.round(run.bytes / 1024)} KB`);
  }
} finally {
  fs.rmSync(dir, { recursive: true, force: true });
}
