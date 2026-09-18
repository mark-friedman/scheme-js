/**
 * Shared benchmark harness for scheme-js-4.
 *
 * Bootstraps an interpreter with the standard library, loads a benchmark
 * program with a supplied `bench-size`, and times `(bench-run)` from the JS
 * side so that timing does not itself go through the interpreter.
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

import { createInterpreter } from '../../src/core/interpreter/index.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { parse } from '../../src/core/interpreter/reader.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
export const PROJECT_ROOT = path.join(__dirname, '..', '..');
export const PROGRAM_DIR = path.join(PROJECT_ROOT, 'benchmarks', 'programs');

/**
 * Standard library files loaded before any benchmark runs. This mirrors the
 * bootstrap in `benchmarks/run_benchmarks.js` rather than going through the
 * library loader, to keep benchmark startup independent of import machinery.
 * @type {string[]}
 */
const BOOTSTRAP_FILES = [
  'src/core/scheme/macros.scm',
  'src/core/scheme/equality.scm',
  'src/core/scheme/cxr.scm',
  'src/core/scheme/numbers.scm',
  'src/core/scheme/list.scm',
  'src/core/scheme/control.scm'
];

/**
 * Creates an interpreter with the standard library loaded.
 * @returns {{interpreter: Object, env: Object, run: function(string): *}} The
 *   interpreter, its global environment, and a helper that evaluates source text.
 */
export function createBenchmarkInterpreter() {
  const { interpreter, env } = createInterpreter();

  const run = (code) => {
    let result;
    for (const expr of parse(code)) {
      result = interpreter.run(analyze(expr), env, [], undefined, { jsAutoConvert: 'raw' });
    }
    return result;
  };

  for (const file of BOOTSTRAP_FILES) {
    run(fs.readFileSync(path.join(PROJECT_ROOT, file), 'utf8'));
  }

  /**
   * Analyzes source text into an executable AST without running it. Callers
   * that need a clean CPU profile should use this and invoke `interpreter.run`
   * directly, so that no harness closure sits in the hot path to absorb
   * inlined frames.
   * @param {string} code - A single Scheme expression.
   * @returns {Object} The analyzed AST node.
   */
  const compile = (code) => {
    const exprs = parse(code);
    if (exprs.length !== 1) {
      throw new Error(`compile() expects exactly one expression, got ${exprs.length}`);
    }
    return analyze(exprs[0]);
  };

  return { interpreter, env, run, compile };
}

/**
 * Options used for every benchmark evaluation. `raw` suppresses the deep
 * Scheme-to-JS conversion that `run` would otherwise apply to results, which
 * would otherwise be charged to the benchmark.
 */
export const RUN_OPTIONS = { jsAutoConvert: 'raw' };

/**
 * Renders a Scheme value as a comparable string. Used for correctness checks,
 * because results span BigInt, boolean and pair values.
 * @param {*} value - A Scheme value.
 * @returns {string} A stable textual representation.
 */
export function renderResult(value) {
  if (value === null) return '()';
  if (value === true) return '#t';
  if (value === false) return '#f';
  if (typeof value === 'bigint') return value.toString();
  if (value && typeof value === 'object' && 'car' in value && 'cdr' in value) {
    return `(${renderResult(value.car)} . ${renderResult(value.cdr)})`;
  }
  return String(value);
}

/**
 * Loads a benchmark program into a fresh interpreter and times `(bench-run)`.
 *
 * A fresh interpreter is used per benchmark so that global state from one
 * program (notably btsearch's `fail` and threads' ready queue) cannot leak into
 * another, and so that allocation from earlier runs does not distort GC timing.
 *
 * @param {Object} bench - A manifest entry.
 * @param {number} size - The size to bind to `bench-size`.
 * @param {number} runs - How many timed repetitions to perform.
 * @returns {{times: number[], median: number, result: string, error: (string|null)}}
 *   Timings in milliseconds, the median, the rendered result, and any error.
 */
export function runBenchmark(bench, size, runs) {
  const source = fs.readFileSync(path.join(PROGRAM_DIR, bench.file), 'utf8');
  const times = [];
  let result = null;
  let error = null;

  try {
    const { run } = createBenchmarkInterpreter();
    run(`(define bench-size ${size})`);
    run(source);

    for (let i = 0; i < runs; i++) {
      const start = performance.now();
      const value = run('(bench-run)');
      times.push(performance.now() - start);
      result = renderResult(value);
    }
  } catch (e) {
    error = e.message;
  }

  times.sort((a, b) => a - b);
  const median = times.length > 0 ? times[Math.floor(times.length / 2)] : null;
  return { times, median, result, error };
}
