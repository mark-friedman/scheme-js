/**
 * @fileoverview The canonical programs run as a page runs them, with the tier
 * attached, compiling counted.
 *
 * ## Why this exists
 *
 * The canonical suite (`run_r7rs.js`) compiles each definition as it appears,
 * before the run it times, so it measures compiled code and never what
 * compiling it cost. A program under the tier -- every page, every CLI run --
 * pays both, at the moment the tier decides to compile, and a short program can
 * spend most of its run compiling (R94 in `docs/compiler_findings.md`). This
 * measures that, program by program.
 *
 * ## What is measured
 *
 * Each program is assembled as `run_r7rs.js` assembles it, at the sizes the
 * suite runs, and run once from start to end on an interpreter set up as a page
 * sets one up: every shipped library installed from its prebuilt table, the
 * tier attached, and each top-level form run through `runTopLevel`, so the
 * tier sees it as it sees a page's. Reported: the whole run, the part of it
 * spent in the tier's three procedures -- `bound`, `due` and `form`, which is
 * where every compile happens -- how many of the program's names the tier
 * compiled, and the same run with the tier not attached. Each figure is the
 * best of several runs, each in a fresh interpreter, since a compile happens
 * once a run.
 *
 * Usage:
 *   node benchmarks/run_tier.js [--only name,name] [--runs N] [--json]
 */

import path from 'path';
import { createInterpreter } from '../src/core/interpreter/index.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { withPrivateLibraries } from '../src/core/interpreter/library_registry.js';
import { callSchemeProcedure, SCHEME_PRIMITIVE } from '../src/core/interpreter/values.js';
import { installLibraryTable } from '../src/compiler/prebuilt.js';
import { attachTier } from '../src/compiler/tiering.js';
import prebuiltLibraries from '../src/packaging/compiled_libraries.js';
import { BUNDLED_SOURCES } from '../src/packaging/bundled_libraries.js';
import { assembleParts, R7RS_DIR } from './lib/r7rs_harness.js';
import { R7RS_BENCHMARKS } from './r7rs/manifest.js';

const args = process.argv.slice(2);
const valueOf = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i >= 0 ? args[i + 1] : fallback;
};
const ONLY = valueOf('--only', null)?.split(',') ?? null;
const RUNS = Number(valueOf('--runs', 3));
const JSON_OUT = args.includes('--json');

/**
 * The libraries a page imports as it starts, and those the programs need.
 * @type {string}
 */
const IMPORTS = `(import (scheme base) (scheme write) (scheme read) (scheme char) (scheme inexact)
  (scheme complex) (scheme cxr) (scheme time) (scheme file) (scheme process-context)
  (scheme case-lambda))`;

/**
 * A shipped library's source, or a file one includes.
 * @param {string[]} name - A library name, or an include's path.
 * @returns {string} Its source.
 */
function bundledSource(name) {
  const last = name[name.length - 1];
  const source = BUNDLED_SOURCES[`${last}.sld`] ?? BUNDLED_SOURCES[last];
  if (source === undefined) throw new Error(`no bundled library ${name.join('/')}`);
  return source;
}

/**
 * Whether a library ships, and so has a prebuilt table the tier leaves alone.
 * @param {string[]} name - The library's name.
 * @returns {boolean}
 */
const isPrebuilt = (name) => BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] !== undefined;

/**
 * Installs a shipped library's prebuilt table as the library loads.
 * @param {string[]} name - The library's name.
 * @param {Object} env - Its environment.
 */
function installTable(name, env) {
  if (isPrebuilt(name) && env) installLibraryTable(prebuiltLibraries, name, env, (f) => BUNDLED_SOURCES[f]);
}

/**
 * Wraps one of the tier's procedures so that the time spent in it is counted.
 * Marked as taking Scheme values, so the evaluator's call converts nothing.
 * @param {Function} hook - The tier's procedure.
 * @param {{ms: number}} spent - Where the time is added.
 * @returns {Function} The wrapper.
 */
function timed(hook, spent) {
  const wrapper = (...hookArgs) => {
    const start = performance.now();
    try {
      return callSchemeProcedure(hook, hookArgs);
    } finally {
      spent.ms += performance.now() - start;
    }
  };
  wrapper[SCHEME_PRIMITIVE] = true;
  return wrapper;
}

/**
 * Runs one program once, as a page runs it.
 * @param {Object} bench - The manifest entry.
 * @param {boolean} withTier - Whether to attach the tier.
 * @returns {{ms: number, compilingMs: number, compiled: number, correct: boolean}}
 */
function runOnce(bench, withTier) {
  const { prelude, body } = assembleParts(bench.name, bench.params, 1, 'scheme-js-4', R7RS_DIR);
  const output = [];
  const log = console.log;
  let result;
  // Programs that read their own data files name them relative to the suite.
  const cwd = process.cwd();
  process.chdir(R7RS_DIR);
  try {
    withPrivateLibraries({ resolver: bundledSource, hook: installTable }, () => {
      const { interpreter, env } = createInterpreter();
      const runForm = (form) => interpreter.runTopLevel(analyze(form), env, { jsAutoConvert: 'raw' });
      parse(IMPORTS).forEach(runForm);
      console.log = (...line) => output.push(line.join(' '));
      for (const form of parse(prelude)) interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
      const spent = { ms: 0 };
      let tier = null;
      if (withTier) {
        tier = attachTier(interpreter, env, { isPrebuilt });
        for (const hook of ['bound', 'due', 'form']) tier[hook] = timed(tier[hook], spent);
      }
      const earlier = new Set(env.bindings.keys());
      const start = performance.now();
      parse(body).forEach(runForm);
      const ms = performance.now() - start;
      const compiled = tier === null ? 0
        : [...tier.outcomes].filter(([name, outcome]) => outcome === 'compiled' && !earlier.has(name)).length;
      result = { ms, compilingMs: spent.ms, compiled };
    });
  } finally {
    console.log = log;
    process.chdir(cwd);
  }
  result.correct = output.some((line) => line.includes('+!CSVLINE!+') && !line.includes('INCORRECT'));
  return result;
}

/**
 * Formats milliseconds.
 * @param {number} ms - Milliseconds.
 * @returns {string}
 */
const fmt = (ms) => `${ms.toFixed(1)}`.padStart(8);

const rows = [];
if (!JSON_OUT) {
  console.log(`best of ${RUNS}; ms for one run of each program, as a page runs it\n`);
  console.log(`${'program'.padEnd(12)} ${'class'.padEnd(12)} ${'tier'.padStart(8)} ${'compiling'.padStart(9)} `
    + `${'share'.padStart(6)} ${'compiled'.padStart(8)} ${'no tier'.padStart(8)}`);
}
for (const bench of R7RS_BENCHMARKS) {
  if (bench.status !== 'ok') continue;
  if (ONLY && !ONLY.includes(bench.name)) continue;
  let best = null;
  let plain = Infinity;
  for (let r = 0; r < RUNS; r++) {
    const tiered = runOnce(bench, true);
    if (best === null || tiered.ms < best.ms) best = tiered;
    plain = Math.min(plain, runOnce(bench, false).ms);
  }
  const row = { name: bench.name, workload: bench.workload, ...best, noTierMs: plain };
  rows.push(row);
  if (!JSON_OUT) {
    console.log(`${bench.name.padEnd(12)} ${bench.workload.padEnd(12)} ${fmt(best.ms)} ${fmt(best.compilingMs)}  `
      + `${`${Math.round(100 * best.compilingMs / best.ms)}%`.padStart(5)} ${String(best.compiled).padStart(8)} `
      + `${fmt(plain)}${best.correct ? '' : '  WRONG ANSWER'}`);
  }
}
if (JSON_OUT) console.log(JSON.stringify(rows, null, 2));
