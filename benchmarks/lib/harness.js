/**
 * Shared benchmark harness for scheme-js-4.
 *
 * Runs a benchmark program as the REPL runs one, with the standard libraries
 * imported at the top level, and times `(bench-run)` from the JS side so that
 * timing does not itself go through the interpreter.
 *
 * The libraries are imported, not their files read into the program's
 * environment, so that a macro of theirs means what it means in a user's
 * program: the names its expansion introduces are its library's bindings, not
 * names looked up where it is used (R136 in `docs/compiler_findings.md`). Each
 * run loads them into a registry of its own, as `benchmarks/run_tier.js` runs
 * each program as a page of its own: interpreted from their source for the
 * interpreted tier, and for the compiled tier restored from the tables the
 * bundle ships, so that the library a benchmark calls is the one a user's
 * program calls.
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

import { createInterpreter } from '../../src/core/interpreter/index.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { withPrivateLibraries } from '../../src/core/interpreter/library_registry.js';
import { globalMacroRegistry } from '../../src/core/interpreter/macro_registry.js';
import { globalContext } from '../../src/core/interpreter/context.js';
import { GLOBAL_SCOPE_ID } from '../../src/core/interpreter/syntax_object.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';
import { pageLibraries } from '../../tests/harness/page_libraries.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
export const PROJECT_ROOT = path.join(__dirname, '..', '..');
export const PROGRAM_DIR = path.join(PROJECT_ROOT, 'benchmarks', 'programs');

/**
 * The libraries a benchmark program runs with, imported at the top level: what
 * the canonical programs import, and what a page imports as it starts.
 * @type {string}
 */
export const STANDARD_IMPORTS = `(import (scheme base) (scheme write) (scheme read) (scheme char) (scheme inexact)
  (scheme complex) (scheme cxr) (scheme time) (scheme file) (scheme process-context)
  (scheme case-lambda))`;

/**
 * A shipped library's source, or a file one includes.
 * @param {string[]} name - A library name, or an include's path.
 * @returns {string} Its source.
 */
export function bundledSource(name) {
  const last = name[name.length - 1];
  const source = BUNDLED_SOURCES[`${last}.sld`] ?? BUNDLED_SOURCES[last];
  if (source === undefined) throw new Error(`no bundled library ${name.join('/')}`);
  return source;
}

/**
 * Whether a library ships, and so has a prebuilt table.
 * @param {string[]} name - The library's name.
 * @returns {boolean}
 */
export const isPrebuilt = (name) => BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] !== undefined;

/**
 * The shipped libraries, as a page has them: each restored from its prebuilt
 * table.
 * @returns {Object} As `pageLibraries` gives them.
 */
export function shippedLibraries() {
  return pageLibraries({ resolve: bundledSource, isShipped: isPrebuilt });
}

/**
 * Runs `fn` as a page of its own: what it defines by name for the whole
 * process -- a library's macros, and what a top level imports -- goes when it
 * ends, as a page's does. Kept, a program defining its own `quasiquote` would
 * expand every later run's quasiquotes that find `quasiquote` by name.
 * @param {function(): *} fn - What to run.
 * @returns {*} What `fn` returned.
 */
export function asOwnPage(fn) {
  const macros = new Map(globalMacroRegistry.macros);
  const topLevel = new Map(globalContext.keywordBindings.get(GLOBAL_SCOPE_ID) ?? []);
  try {
    return fn();
  } finally {
    globalMacroRegistry.macros = macros;
    globalContext.keywordBindings.set(GLOBAL_SCOPE_ID, topLevel);
  }
}

/**
 * Calls `fn` with an interpreter that has the standard libraries imported, in
 * a registry of its own that is current until `fn` returns: a library's
 * scopes are known to the expander only while its registry is, so everything
 * run with the interpreter is run inside `fn`.
 *
 * @param {Object} options - Options.
 * @param {boolean} [options.compileStdlib=false] - Restore the libraries from
 *   the tables the bundle ships, compiled, rather than interpret them from
 *   their source. Without it a compiled benchmark calls interpreted `map`,
 *   `assq` and `member` on its hottest paths, and the figures measure that
 *   boundary rather than the generated code.
 * @param {function({interpreter: Object, env: Object, run: function(string): *,
 *   compile: function(string): Object}): *} fn - What to run: given the
 *   interpreter, its global environment, a helper that evaluates source text,
 *   and one that analyzes a single expression without running it.
 * @returns {*} What `fn` returned.
 */
export function withBenchmarkInterpreter(options, fn) {
  const libraries = options.compileStdlib ? shippedLibraries() : { resolve: bundledSource, hook: null, restorer: null };
  return asOwnPage(() => withPrivateLibraries(
    { resolver: libraries.resolve, hook: libraries.hook, restorer: libraries.restorer },
    () => {
      const { interpreter, env } = createInterpreter();

      const run = (code) => {
        let result;
        for (const expr of parse(code)) {
          result = interpreter.run(analyze(expr, env), env, [], undefined, RUN_OPTIONS);
        }
        return result;
      };

      run(STANDARD_IMPORTS);
      // A table that no longer matches its library's source leaves the
      // library interpreted, a run no user's program makes.
      if (libraries.fromSource?.size > 0) {
        throw new Error(`${[...libraries.fromSource].join(', ')} read from source, where a page restores `
          + 'it from its table: the table is stale, and `npm run prebuild` rebuilds it');
      }

      /**
       * Analyzes source text into an executable AST without running it.
       * Callers that need a clean CPU profile should use this and invoke
       * `interpreter.run` directly, so that no harness closure sits in the hot
       * path to absorb inlined frames.
       * @param {string} code - A single Scheme expression.
       * @returns {Object} The analyzed AST node.
       */
      const compile = (code) => {
        const exprs = parse(code);
        if (exprs.length !== 1) {
          throw new Error(`compile() expects exactly one expression, got ${exprs.length}`);
        }
        return analyze(exprs[0], env);
      };

      return fn({ interpreter, env, run, compile });
    }));
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
    withBenchmarkInterpreter({}, ({ run }) => {
      run(`(define bench-size ${size})`);
      run(source);

      for (let i = 0; i < runs; i++) {
        const start = performance.now();
        const value = run('(bench-run)');
        times.push(performance.now() - start);
        result = renderResult(value);
      }
    });
  } catch (e) {
    error = e.message;
  }

  times.sort((a, b) => a - b);
  const median = times.length > 0 ? times[Math.floor(times.length / 2)] : null;
  return { times, median, result, error };
}
