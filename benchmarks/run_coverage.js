/**
 * @fileoverview How fitted each benchmark suite is to the compiler's fast
 * paths.
 *
 * ## Why this exists
 *
 * The eight Stage 0 programs were found overfitted (R20 in
 * `docs/compiler_findings.md`): they called 16 distinct procedures, against
 * 136 in the repository's own Scheme, and 98% of their calls landed on a
 * primitive the compiler expands inline, against 34%. Every optimization had
 * been chosen on them, and their speedups did not transfer. The canonical
 * suite replaced them as the gate, but nothing went on checking whether it, or
 * the workloads chosen since, drift the same way. This reports those two
 * measures for every suite, so that a suite fitted to what the compiler does
 * well shows as one.
 *
 * ## What is measured
 *
 * Each program runs once, interpreted, with every procedure bound in its
 * environment wrapped to count its calls by name (`lib/coverage.js`):
 *
 *  - the eight Stage 0 programs (`benchmarks/programs/`), at their `quick`
 *    sizes;
 *  - the canonical suite (`benchmarks/r7rs/`), by workload class and in all,
 *    each program once at its manifest's size;
 *  - the repository's Scheme test files (`tests/core/scheme/`), as
 *    `run_macro.js` runs them, the code that is not a benchmark.
 *
 * For each: the calls counted, the distinct procedures called, and the share
 * of calls on a primitive the compiler expands inline (`inline-expansion-names`
 * in `src/compiler/inline.scm`), pooled over the suite's calls and averaged
 * over its programs. A suite whose share is far above the test files'
 * measures what the compiler inlines more than what programs do.
 *
 * Usage:
 *   node benchmarks/run_coverage.js [--only stage0,canonical,tests]
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { createBenchmarkInterpreter, PROGRAM_DIR } from './lib/harness.js';
import { assembleParts, R7RS_DIR } from './lib/r7rs_harness.js';
import { countPrimitives, coverage, silenced, testEnvironment } from './lib/coverage.js';
import { BENCHMARKS } from './programs/manifest.js';
import { R7RS_BENCHMARKS } from './r7rs/manifest.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/expand.js';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const args = process.argv.slice(2);
const ONLY = args.includes('--only') ? args[args.indexOf('--only') + 1].split(',') : ['stage0', 'canonical', 'tests'];

/**
 * Adds one count map into another.
 * @param {Map<string, number>} into - The total.
 * @param {Map<string, number>} counts - What to add.
 */
function addInto(into, counts) {
  for (const [name, n] of counts) into.set(name, (into.get(name) ?? 0) + n);
}

/**
 * Runs Scheme source in a counting interpreter, its output discarded, and
 * answers the calls counted.
 * @param {{interpreter: Object, env: Object}} pair - The interpreter.
 * @param {string} source - What to run.
 * @returns {Promise<Map<string, number>>}
 */
async function counted({ interpreter, env }, source) {
  const counter = countPrimitives(env);
  try {
    await silenced(() => {
      for (const form of parse(source)) interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
    });
  } finally {
    counter.restore();
  }
  return new Map(counter.counts);
}

/**
 * A percentage, in a column.
 * @param {number} share - A share, from 0 to 1.
 * @returns {string}
 */
const percent = (share) => `${(100 * share).toFixed(1)}%`.padStart(10);

/**
 * A line of the report: the suite's calls pooled, and the share inlined of
 * each program averaged, so that no one program's loop decides the suite's
 * figure, as one million-iteration test once decided the test files' (R21).
 * @param {string} label - The suite.
 * @param {Array<Map<string, number>>} programs - Each program's calls, by name.
 */
function report(label, programs) {
  const all = new Map();
  for (const counts of programs) addInto(all, counts);
  const { calls, distinct, inlinedShare } = coverage(all);
  const counted = programs.filter((counts) => counts.size > 0);
  const mean = counted.reduce((sum, counts) => sum + coverage(counts).inlinedShare, 0) / Math.max(counted.length, 1);
  console.log(`  ${label.padEnd(40)} ${String(calls).padStart(12)} ${String(distinct).padStart(9)} `
    + `${percent(inlinedShare)} ${percent(mean)}`);
}

console.log('Coverage: each suite run once, interpreted, its calls counted by name');
console.log(`\n  ${''.padEnd(40)} ${''.padStart(12)} ${''.padStart(9)} ${'share inlined, pooled'.padStart(21)} / by program`);
console.log(`  ${'suite'.padEnd(40)} ${'calls'.padStart(12)} ${'distinct'.padStart(9)} ${'pooled'.padStart(10)} ${'mean'.padStart(10)}`);

if (ONLY.includes('stage0')) {
  const programs = [];
  for (const bench of BENCHMARKS) {
    const pair = createBenchmarkInterpreter();
    pair.run(`(define bench-size ${bench.quick})`);
    pair.run(fs.readFileSync(path.join(PROGRAM_DIR, bench.file), 'utf8'));
    programs.push(await counted(pair, '(bench-run)'));
  }
  report(`Stage 0 programs (${BENCHMARKS.length})`, programs);
}

if (ONLY.includes('canonical')) {
  const all = [];
  const byClass = new Map();
  const programs = R7RS_BENCHMARKS.filter((bench) => bench.status === 'ok');
  for (const bench of programs) {
    const { prelude, body } = assembleParts(bench.name, bench.params, 1, 'scheme-js-4');
    const pair = createBenchmarkInterpreter();
    pair.run(prelude);
    // Run where the suite keeps its data files, which some programs open.
    process.chdir(R7RS_DIR);
    const counts = await counted(pair, body).finally(() => process.chdir(ROOT));
    all.push(counts);
    if (!byClass.has(bench.workload)) byClass.set(bench.workload, []);
    byClass.get(bench.workload).push(counts);
  }
  for (const [workload, counts] of [...byClass].sort()) report(`canonical: ${workload} (${counts.length})`, counts);
  report(`canonical, all (${programs.length})`, all);
}

if (ONLY.includes('tests')) {
  const dir = path.join(ROOT, 'tests/core/scheme');
  const files = fs.readdirSync(dir).filter((f) => f.endsWith('.scm') && f !== 'test.scm').sort();
  const programs = [];
  for (const file of files) {
    const pair = await silenced(testEnvironment);
    try {
      programs.push(await counted(pair, fs.readFileSync(path.join(dir, file), 'utf8')));
    } catch {
      // A test file that cannot run this way counts nothing.
    }
  }
  report(`the repository's test files (${files.length})`, programs);
}
