/**
 * @fileoverview How fitted each benchmark suite is to the compiler's fast
 * paths.
 *
 * ## Why this exists
 *
 * The eight Stage 0 programs were found overfitted (R20 in
 * `docs/compiler_findings.md`): they called 16 distinct procedures, against
 * 136 in the repository's Scheme test files, and 98% of their calls landed on
 * a primitive the compiler expands inline, against 34%. Every optimization had
 * been chosen on them, and their speedups did not transfer. The canonical
 * suite replaced them as the gate, but nothing went on checking whether it, or
 * the workloads chosen since, drift the same way. This reports, for every
 * suite, how broad it is and how much of it the inline expansions decide.
 *
 * ## What is measured
 *
 * Each program runs once, interpreted, with every procedure bound in its
 * environment before it runs -- the language's, not the program's own --
 * wrapped to count its calls by name (`lib/coverage.js`):
 *
 *  - the Stage 0 programs (`benchmarks/programs/`), at their `quick` sizes;
 *  - the canonical suite (`benchmarks/r7rs/`), by workload class and in all,
 *    each program once at its manifest's size, and then program by program;
 *  - the repository's Scheme test files (`tests/core/scheme/`), as
 *    `run_macro.js` runs them.
 *
 * For each: the calls counted, the distinct procedures called, and the share
 * of calls on a primitive the compiler expands inline (`inline-expansion-names`
 * in `src/compiler/inline.scm`), pooled over the suite's calls and averaged
 * over its programs. Program by program, the canonical suite's table gives each
 * program's size in lines, the procedures it calls beyond those every program
 * calls -- the harness's -- and its share.
 *
 * ## How to read it
 *
 * The share is how much of a workload's use of the language the inline
 * expansions decide, not how fitted the workload is to them: applications
 * spend their calls on `car`, `cdr`, `eq?` and `null?` as kernels do. In the
 * canonical suite, `compiler` -- Gambit's compiler, 11,000 lines -- puts 76% of
 * its calls there, `scheme` 92%, `graphs` 99.8% and `slatex` 39%, a spread no
 * different from the kernels'. The test files' share is lower because tests
 * call the library broadly, by design: it is the share of test code, not of
 * programs (R135, R136). Breadth tells kernels from applications better,
 * though not cleanly: the canonical programs under 250 lines call none to
 * fifteen procedures beyond the harness's, `compiler` 44, `slatex` and
 * `dynamic` 21 -- but `scheme`, at 1,056 lines, calls 11, and `graphs`, at
 * 611, 8. A suite of programs that call little beyond the harness measures a
 * few operations, however its share reads.
 *
 * Only a program's calls into the language are counted: each suite imports the
 * standard libraries (`withBenchmarkInterpreter` in `lib/harness.js`), and a
 * library's procedures call each other in its own environment, which the
 * counting does not wrap.
 *
 * Usage:
 *   node benchmarks/run_coverage.js [--only stage0,canonical,tests]
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { withBenchmarkInterpreter, PROGRAM_DIR } from './lib/harness.js';
import { assembleParts, R7RS_DIR } from './lib/r7rs_harness.js';
import { countPrimitives, coverage, silenced, silencedNow, testEnvironment } from './lib/coverage.js';
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
 * answers the calls counted. Synchronous, so that it can run inside
 * `withBenchmarkInterpreter`.
 * @param {{interpreter: Object, env: Object}} pair - The interpreter.
 * @param {string} source - What to run.
 * @returns {Map<string, number>}
 */
function counted({ interpreter, env }, source) {
  const counter = countPrimitives(env);
  try {
    silencedNow(() => {
      for (const form of parse(source)) interpreter.run(analyze(form, env), env, [], undefined, { jsAutoConvert: 'raw' });
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

/**
 * The canonical suite program by program, largest first: each program's size
 * in lines, the procedures it calls beyond those every program calls -- the
 * harness's, which run each program -- and its share of calls inlined. Lists
 * size beside the share so that a reader can see the share does not follow it.
 * @param {Array<Object>} programs - The manifest's entries, in run order.
 * @param {Array<Map<string, number>>} counts - Each program's calls, by name.
 */
function byProgram(programs, counts) {
  const shared = counts.map((c) => new Set(c.keys()))
    .reduce((common, names) => new Set([...common].filter((name) => names.has(name))));
  const rows = programs.map((bench, i) => ({
    name: bench.name,
    workload: bench.workload,
    lines: fs.readFileSync(path.join(R7RS_DIR, 'src', `${bench.name}.scm`), 'utf8').split('\n').length,
    beyond: [...counts[i].keys()].filter((name) => !shared.has(name)).length,
    share: coverage(counts[i]).inlinedShare
  })).sort((a, b) => b.lines - a.lines);
  console.log(`\n  The canonical suite by program, largest first; the harness calls ${shared.size} procedures in every one`);
  console.log(`  ${'program'.padEnd(14)} ${'class'.padEnd(14)} ${'lines'.padStart(7)} ${'beyond harness'.padStart(15)} ${'inlined'.padStart(10)}`);
  for (const row of rows) {
    console.log(`  ${row.name.padEnd(14)} ${row.workload.padEnd(14)} ${String(row.lines).padStart(7)} `
      + `${String(row.beyond).padStart(15)} ${percent(row.share)}`);
  }
}

console.log('Coverage: each suite run once, interpreted, its calls counted by name');
console.log(`\n  ${''.padEnd(40)} ${''.padStart(12)} ${''.padStart(9)} ${'share inlined, pooled'.padStart(21)} / by program`);
console.log(`  ${'suite'.padEnd(40)} ${'calls'.padStart(12)} ${'distinct'.padStart(9)} ${'pooled'.padStart(10)} ${'mean'.padStart(10)}`);

if (ONLY.includes('stage0')) {
  const programs = [];
  for (const bench of BENCHMARKS) {
    programs.push(withBenchmarkInterpreter({}, (pair) => {
      pair.run(`(define bench-size ${bench.quick})`);
      pair.run(fs.readFileSync(path.join(PROGRAM_DIR, bench.file), 'utf8'));
      return counted(pair, '(bench-run)');
    }));
  }
  report(`Stage 0 programs (${BENCHMARKS.length})`, programs);
}

if (ONLY.includes('canonical')) {
  const all = [];
  const byClass = new Map();
  const programs = R7RS_BENCHMARKS.filter((bench) => bench.status === 'ok');
  for (const bench of programs) {
    const { prelude, body } = assembleParts(bench.name, bench.params, 1, 'scheme-js-4');
    const counts = withBenchmarkInterpreter({}, (pair) => {
      pair.run(prelude);
      // Run where the suite keeps its data files, which some programs open.
      process.chdir(R7RS_DIR);
      try {
        return counted(pair, body);
      } finally {
        process.chdir(ROOT);
      }
    });
    all.push(counts);
    if (!byClass.has(bench.workload)) byClass.set(bench.workload, []);
    byClass.get(bench.workload).push(counts);
  }
  for (const [workload, counts] of [...byClass].sort()) report(`canonical: ${workload} (${counts.length})`, counts);
  report(`canonical, all (${programs.length})`, all);
  byProgram(programs, all);
}

if (ONLY.includes('tests')) {
  const dir = path.join(ROOT, 'tests/core/scheme');
  const files = fs.readdirSync(dir).filter((f) => f.endsWith('.scm') && f !== 'test.scm').sort();
  const programs = [];
  for (const file of files) {
    const pair = await silenced(testEnvironment);
    try {
      programs.push(counted(pair, fs.readFileSync(path.join(dir, file), 'utf8')));
    } catch {
      // A test file that cannot run this way counts nothing.
    }
  }
  report(`the repository's test files, test code (${files.length})`, programs);
}
