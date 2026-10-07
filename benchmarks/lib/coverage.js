/**
 * @fileoverview What a benchmark run exercises: calls to the bindings of an
 * environment counted by name, and the share of them that land on a primitive
 * the compiler expands inline -- the measure of how fitted a workload is to
 * the compiler's fast paths (R20 in `docs/compiler_findings.md`) -- for
 * `benchmarks/run_macro.js` and `benchmarks/run_coverage.js`; and an
 * interpreter set up as the Scheme test runner sets one up, for running the
 * repository's test files as a workload.
 */

import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { parse } from '../../src/core/interpreter/reader.js';
import {
  loadLibrary, applyImports, setFileResolver, registerBuiltinLibrary, createPrimitiveExports
} from '../../src/core/interpreter/library_loader.js';
import { compilerExports } from '../../src/compiler/lowering.js';
import { toArray } from '../../src/core/interpreter/cons.js';

const PROJECT_ROOT = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', '..');

/** Libraries the real test runner bootstraps, in the same order. */
const LIBRARIES = [
  ['scheme', 'base'], ['scheme', 'repl'], ['scheme', 'case-lambda'],
  ['scheme', 'lazy'], ['scheme', 'eval'],
  ['scheme-js', 'promise'], ['scheme-js', 'js-conversion'],
  ['scheme-js', 'interop'], ['scheme-js', 'define-macro']
];

/**
 * Reads a project file.
 * @param {string} relative - Path relative to the project root.
 * @returns {Promise<string>} The contents.
 */
const readProjectFile = (relative) => fs.readFile(path.join(PROJECT_ROOT, relative), 'utf8');

/**
 * Bootstraps an interpreter the way the real Scheme test runner does, with
 * its harness loaded and its reports discarded.
 *
 * Mirrors `tests/run_scheme_tests_lib.js` rather than calling it, because the
 * benchmark needs to time the phases separately and that function runs them as
 * one unit. The library list is kept in the same order so the resulting
 * environment matches.
 *
 * @returns {Promise<{interpreter: Object, env: Object}>} A ready environment.
 */
export async function testEnvironment() {
  const { interpreter, env } = createInterpreter();

  registerBuiltinLibrary(['scheme', 'primitives'], createPrimitiveExports(env), env);

  setFileResolver(async (parts) => {
    const name = parts[parts.length - 1];
    const candidates = name.endsWith('.scm') || name.endsWith('.sld')
      ? [`src/core/scheme/${name}`, `src/extras/scheme/${name}`]
      : [`src/core/scheme/${name}.sld`, `src/extras/scheme/${name}.sld`];
    for (const candidate of candidates) {
      try {
        return await readProjectFile(candidate);
      } catch { /* try the next location */ }
    }
    throw new Error(`Library not found: ${parts.join('/')}`);
  });

  for (const library of LIBRARIES) {
    const exports = await loadLibrary(library, analyze, interpreter, env);
    applyImports(env, exports, { libraryName: library });
  }

  // The Scheme test harness reports through these, so they have to exist. The
  // benchmark discards the results: it is measuring how long real code takes to
  // run, not whether it passes -- `npm test` already covers that.
  env.bindings.set('native-report-test-result', () => undefined);
  env.bindings.set('native-log-title', () => undefined);
  env.bindings.set('native-report-test-skip', () => undefined);

  const harness = await readProjectFile('tests/core/scheme/test.scm');
  for (const form of parse(harness)) {
    interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
  }

  return { interpreter, env };
}

/**
 * Wraps every callable binding in an environment chain to count calls.
 *
 * This counts *callable bindings*, not strictly primitives: a Scheme closure is
 * a function too, and so is a compiled procedure. That means the absolute call
 * counts are not comparable between the two tiers -- compiling a procedure
 * changes how it is reached. What is comparable, and what this is for, is the
 * *share* of calls landing on a primitive the compiler inlines, which is the
 * measure of how narrow a workload is.
 *
 * @param {Object} env - The environment.
 * @returns {{counts: Map<string, number>, restore: function(): void}} Counter.
 */
export function countPrimitives(env) {
  const counts = new Map();
  const originals = [];
  for (let frame = env; frame !== null; frame = frame.parent) {
    for (const [name, value] of [...frame.bindings]) {
      if (typeof value !== 'function') continue;
      originals.push([frame, name, value]);
      const wrapped = (...a) => {
        counts.set(name, (counts.get(name) || 0) + 1);
        return value(...a);
      };
      for (const key of Object.getOwnPropertySymbols(value)) wrapped[key] = value[key];
      Object.assign(wrapped, value);
      frame.bindings.set(name, wrapped);
    }
  }
  return { counts, restore: () => { for (const [f, n, v] of originals) f.bindings.set(n, v); } };
}

/**
 * Runs a function with console output and stdout writes suppressed.
 *
 * The workload is a test suite: it prints progress, and one file evaluates
 * browser-only JavaScript whose failure the interpreter logs. Beyond making the
 * report unreadable, console and stdout writes are slow enough to dominate the
 * very measurement being taken.
 *
 * @param {function(): *} fn - The function to run.
 * @returns {Promise<*>} Whatever `fn` returned.
 */
export async function silenced(fn) {
  const saved = {
    log: console.log, error: console.error, warn: console.warn, info: console.info,
    write: process.stdout.write
  };
  console.log = console.error = console.warn = console.info = () => {};
  process.stdout.write = () => true;
  try {
    return await fn();
  } finally {
    Object.assign(console, { log: saved.log, error: saved.error, warn: saved.warn, info: saved.info });
    process.stdout.write = saved.write;
  }
}

/**
 * Summarizes primitive coverage.
 * @param {Map<string, number>} counts - Primitive call counts.
 * @returns {{calls: number, distinct: number, inlinedShare: number}} Coverage.
 */
export function coverage(counts) {
  // The globals the compiler has an inline expansion for (`inline.scm`).
  const expanded = toArray(compilerExports().get('inline-expansion-names')()).map((symbol) => symbol.name);
  const calls = [...counts.values()].reduce((a, b) => a + b, 0);
  const inlined = [...counts.entries()]
    .filter(([name]) => expanded.includes(name))
    .reduce((a, [, c]) => a + c, 0);
  return { calls, distinct: counts.size, inlinedShare: calls > 0 ? inlined / calls : 0 };
}
