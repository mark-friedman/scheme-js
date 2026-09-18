/**
 * Shared evaluator step counting.
 *
 * Extracted so that `count_steps.js` (the reporting tool) and
 * `record_progress.js` (the history recorder) cannot disagree about how a step
 * count is produced. A progress report whose numbers are gathered differently
 * from the report they are compared against is not a progress report.
 */

import fs from 'fs';
import path from 'path';

import { PROGRAM_DIR, createBenchmarkInterpreter, renderResult, RUN_OPTIONS } from './harness.js';
import { instrumentInterpreter } from '../../src/debug/instrumentation.js';

/**
 * Sizes used when counting steps.
 *
 * Counting is much slower than running, because every dispatch goes through a
 * wrapper, so these are deliberately smaller than the `quick` timing profile.
 * They must stay fixed across stages: a step count is only comparable to
 * another step count at the same size.
 */
export const SWEEP_SIZES = {
  fib: 18, tak: 14, oddeven: 2000, nqueens: 6,
  ctak: 14, contfib: 14, btsearch: 40, threads: 40
};

/**
 * Counts evaluator steps for one benchmark.
 * @param {Object} bench - A manifest entry.
 * @param {number} size - The size to bind to `bench-size`.
 * @returns {{stats: Object, result: string}} Statistics and the rendered result.
 */
export function countBenchmark(bench, size) {
  const { interpreter, env, run, compile } = createBenchmarkInterpreter();
  run(`(define bench-size ${size})`);
  run(fs.readFileSync(path.join(PROGRAM_DIR, bench.file), 'utf8'));

  const ast = compile('(bench-run)');
  const probe = instrumentInterpreter(interpreter);
  let value;
  let stats;
  try {
    value = interpreter.run(ast, env, [], undefined, RUN_OPTIONS);
  } finally {
    stats = probe.stop();
  }
  return { stats, result: renderResult(value) };
}
