/**
 * Runs the canonical R7RS suite compiled ahead of time, beside the compiled
 * tier.
 *
 * A program compiled ahead of time (`node repl.js --build`) runs with no
 * interpreter: its captures and its moves of frames to the heap are finished
 * by the driver of the runtime's own (`runAhead` in
 * src/core/interpreter/unwind.js), where under the tier the interpreter
 * beneath finishes moves. Each benchmark is calibrated under the tier, as
 * `run_r7rs.js` calibrates it, then built ahead of time at the same count and
 * run as a user runs it, `node OUTPUT`, in a process that loads nothing but
 * the file; the two times per iteration are compared. A program the build
 * refuses is reported with the build's reasons.
 *
 * The program's input, which the canonical harness reads through a string
 * port, is written into the program as data (`assembleAhead`), since a
 * program compiled ahead of time has no reader.
 *
 * Usage:
 *   node benchmarks/run_ahead.js [--profile default|full] [--target SECONDS] [--only name,name]
 */

import { execFileSync, spawnSync } from 'child_process';
import fs from 'fs';
import os from 'os';
import path from 'path';
import { fileURLToPath } from 'url';

import { selectBenchmarks } from './r7rs/manifest.js';
import { calibrate, assembleAhead, parseCsvLine, R7RS_DIR } from './lib/r7rs_harness.js';

const HERE = path.dirname(fileURLToPath(import.meta.url));
const ROOT = path.join(HERE, '..');
const R7RS_WORKER = path.join(HERE, 'lib', 'r7rs_worker.js');

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
 * @param {string} dir - Where to write the program and the file built.
 * @returns {{seconds: (number|null), incorrect: boolean, error: (string|null),
 *   refusals: string[], bytes: number}}
 */
function ahead(bench, count, dir) {
  const program = path.join(dir, `${bench.name}.scm`);
  const output = path.join(dir, `${bench.name}.mjs`);
  fs.writeFileSync(program, assembleAhead(bench.name, bench.params, count, 'scheme-js-4-ahead'));
  try {
    execFileSync(process.execPath, ['repl.js', '--build', program, '-o', output],
      { cwd: ROOT, stdio: ['ignore', 'pipe', 'pipe'], timeout: BUDGET_MS });
  } catch (e) {
    return { seconds: null, incorrect: false, error: null, refusals: String(e.stderr).trim().split('\n'), bytes: 0 };
  }
  // From the suite's directory, as r7rs_worker.js runs a benchmark: some open
  // their data by a path relative to it.
  const ran = spawnSync(process.execPath, [output], { cwd: R7RS_DIR, encoding: 'utf8', timeout: BUDGET_MS });
  const parsed = parseCsvLine(ran.stdout ?? '');
  return {
    seconds: parsed.seconds,
    incorrect: parsed.incorrect,
    error: ran.status === 0 ? null : (ran.stderr || String(ran.error)).trim().slice(0, 120),
    refusals: [],
    bytes: fs.statSync(output).size
  };
}

const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scheme-ahead-bench-'));
const only = ONLY === null ? null : ONLY.split(',');
console.log('per iteration       tier         ahead     ahead/tier   file');
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
