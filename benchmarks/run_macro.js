/**
 * Macro-benchmark: the repository's own Scheme, measured as a workload.
 *
 * ## Why this exists
 *
 * The eight programs in `benchmarks/programs/` are microbenchmarks, and a
 * coverage check showed how narrow they are: they exercise **16 distinct
 * primitives**, and **98% of their primitive calls** land on the fifteen
 * primitives the compiler inlines. Real Scheme in this repository exercises
 * **139**. Every optimization in the compiler effort was chosen by measuring against
 * those eight programs, so their numbers cannot be trusted to say what a real
 * program would see -- the suite and the optimizations were fitted to each
 * other.
 *
 * This benchmark is the transfer test. Its workload is the project's own
 * `.scm` test files: real code, written to check correctness rather than to be
 * fast, and not chosen by anyone for its performance characteristics. If an
 * improvement measured on the microbenchmarks does not show up here, it did not
 * generalize.
 *
 * It reports a coverage summary alongside the timings, so a future change that
 * only helps the narrow case is visible as such rather than being reported as a
 * general speedup.
 *
 * Usage:
 *   node benchmarks/run_macro.js [--runs N] [--compile] [--files N]
 */

import fs from 'fs/promises';
import fsSync from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

import { analyze } from '../src/core/interpreter/expand.js';
import { parse } from '../src/core/interpreter/reader.js';
import { isSchemeClosure } from '../src/core/interpreter/values.js';
import { instrumentInterpreter } from '../src/debug/instrumentation.js';
import { tryCompileClosure, tryCompileDefinition } from '../src/compiler/index.js';
import { unsafeDefinitions } from '../src/compiler/index.js';
import { DefineNode } from '../src/core/interpreter/ast_nodes.js';
import { testEnvironment as bootstrap, countPrimitives, silenced, coverage } from './lib/coverage.js';

const PROJECT_ROOT = path.join(path.dirname(fileURLToPath(import.meta.url)), '..');
const SCHEME_TEST_DIR = path.join(PROJECT_ROOT, 'tests', 'core', 'scheme');

const args = process.argv.slice(2);
const valueOf = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i >= 0 ? args[i + 1] : fallback;
};
const RUNS = parseInt(valueOf('--runs', '3'), 10);
const FILE_LIMIT = parseInt(valueOf('--files', '999'), 10);

/**
 * Compiles every interpreted closure reachable from an environment chain.
 *
 * This is what makes the transfer test meaningful. The workload's time is spent
 * inside standard-library procedures, which the library loader created as
 * interpreted closures; compiling the user code alone would measure almost
 * nothing. A closure retains its parameters, body and defining environment, so
 * it can be recompiled after the fact without going back through the loader.
 *
 * @param {Object} env - The environment to sweep.
 * @returns {{compiled: number, declined: number, reasons: Object<string, number>}}
 *   How many procedures were compiled, and why the rest were not.
 */
function compileEnvironment(env) {
  let compiled = 0;
  let declined = 0;
  const reasons = {};

  for (let frame = env; frame !== null; frame = frame.parent) {
    for (const [name, value] of [...frame.bindings]) {
      if (!isSchemeClosure(value)) continue;
      const result = tryCompileClosure(value, name);
      if (result.compiled) {
        frame.bindings.set(name, result.procedure);
        compiled++;
      } else {
        declined++;
        // Group by cause rather than listing every procedure, so the shape of
        // what the compiler cannot yet handle is visible at a glance.
        const cause = (result.reason || 'unknown')
          .replace(/'[^']*'/, "'…'")
          .replace(/: .*/, '');
        reasons[cause] = (reasons[cause] || 0) + 1;
      }
    }
  }
  return { compiled, declined, reasons };
}

/**
 * Runs the workload once, timing each phase.
 * @param {Array<{name: string, source: string}>} workload - The test files.
 * @param {boolean} useCompiler - Whether to compile the environment first.
 * @param {boolean} instrument - Whether to collect coverage and step counts.
 * @returns {Promise<Object>} Timings and, if requested, coverage.
 */
async function runOnce(workload, useCompiler, instrument) {
  const { interpreter, env } = await bootstrap();

  let compilation = null;
  if (useCompiler) compilation = compileEnvironment(env);

  const primitives = instrument ? countPrimitives(env) : null;
  const probe = instrument ? instrumentInterpreter(interpreter) : null;

  let parseMs = 0;
  let analyzeMs = 0;
  let executeMs = 0;
  let workloadCompiled = 0;
  const failures = [];
  const perFile = [];

  for (const file of workload) {
    const fileStart = performance.now();
    try {
      let t = performance.now();
      const forms = parse(file.source);
      parseMs += performance.now() - t;

      t = performance.now();
      const asts = forms.map((form) => analyze(form));
      analyzeMs += performance.now() - t;

      // Which of this file's definitions a continuation could be captured
      // inside. Decided over the whole file before anything runs, because the
      // answer depends on the call graph rather than on one definition's text:
      // a procedure that names no control global is still unsafe if something
      // it calls escapes through it. Not timed -- it is compilation
      // work, and the interpreted run does not do it.
      const unsafe = useCompiler ? unsafeDefinitions(asts, env) : new Map();

      for (const ast of asts) {
        t = performance.now();
        // A procedure the workload defines is compiled as it appears, the way a
        // tiered runtime would. Sweeping the environment beforehand only
        // reaches the standard library; the first version of this benchmark did
        // only that and so left the workload's own hot procedures interpreted,
        // which made compiling look useless.
        let handled = false;
        if (useCompiler && ast instanceof DefineNode && !unsafe.has(ast.name)) {
          const result = tryCompileDefinition(ast, env);
          if (result.compiled) {
            env.define(result.name, result.procedure);
            workloadCompiled++;
            handled = true;
          }
        }
        if (!handled) interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
        executeMs += performance.now() - t;
      }
    } catch (e) {
      failures.push({ file: file.name, error: e.message.slice(0, 90) });
    }
    perFile.push({ name: file.name, ms: performance.now() - fileStart });
  }

  const steps = probe ? probe.stop() : null;
  const counts = primitives ? new Map(primitives.counts) : null;
  if (primitives) primitives.restore();

  return { parseMs, analyzeMs, executeMs, totalMs: parseMs + analyzeMs + executeMs,
    failures, compilation, workloadCompiled, steps, counts, perFile };
}

async function main() {
  const names = (await fs.readdir(SCHEME_TEST_DIR))
    .filter((f) => f.endsWith('.scm') && f !== 'test.scm')
    .sort()
    .slice(0, FILE_LIMIT);

  const workload = [];
  for (const name of names) {
    workload.push({ name, source: fsSync.readFileSync(path.join(SCHEME_TEST_DIR, name), 'utf8') });
  }
  const lines = workload.reduce((a, f) => a + f.source.split('\n').length, 0);

  console.log('='.repeat(80));
  console.log('Macro-benchmark: the repository\'s own Scheme');
  console.log('='.repeat(80));
  console.log(`${workload.length} files, ${lines.toLocaleString()} lines, median of ${RUNS} runs`);
  console.log('');

  const results = {};
  for (const [label, useCompiler] of [['interpreter', false], ['compiler tier', true]]) {
    const times = [];
    let last;
    for (let i = 0; i < RUNS; i++) {
      last = await silenced(() => runOnce(workload, useCompiler, false));
      times.push(last.totalMs);
    }
    times.sort((a, b) => a - b);
    // Coverage and step counts are gathered in a separate run, because wrapping
    // every primitive to count calls would distort the timings.
    const detail = await silenced(() => runOnce(workload, useCompiler, true));

    results[label] = {
      medianMs: times[Math.floor(times.length / 2)],
      phases: { parse: last.parseMs, analyze: last.analyzeMs, execute: last.executeMs },
      failures: last.failures,
      compilation: last.compilation,
      workloadCompiled: last.workloadCompiled,
      coverage: coverage(detail.counts),
      steps: detail.steps.totalSteps,
      perFile: last.perFile
    };
  }

  console.log('| tier | total | parse | analyze | execute |');
  console.log('|------|-------|-------|---------|---------|');
  for (const [label, r] of Object.entries(results)) {
    const p = r.phases;
    console.log(`| ${label.padEnd(13)} | ${(r.medianMs.toFixed(0) + ' ms').padEnd(5)} ` +
      `| ${(p.parse.toFixed(0) + ' ms').padEnd(5)} | ${(p.analyze.toFixed(0) + ' ms').padEnd(7)} ` +
      `| ${(p.execute.toFixed(0) + ' ms').padEnd(7)} |`);
  }

  // A total is the wrong headline for this workload. One file -- a TCO space
  // test that runs a million-iteration loop -- accounts for most of the time,
  // so a total reports that file and calls it a suite. Per-file speedups with a
  // geometric mean give every file equal weight, and the dominant files are
  // named so the total can still be interpreted.
  const byName = (r) => new Map(r.perFile.map((f) => [f.name, f.ms]));
  const baseFiles = byName(results['interpreter']);
  const compiledFiles = byName(results['compiler tier']);

  const ratios = [];
  for (const [name, baseMs] of baseFiles) {
    const compiledMs = compiledFiles.get(name);
    // Files below a millisecond are timer noise, not measurements.
    if (compiledMs === undefined || baseMs < 1 || compiledMs < 1) continue;
    ratios.push({ name, ratio: baseMs / compiledMs, baseMs, compiledMs });
  }
  ratios.sort((a, b) => b.baseMs - a.baseMs);

  const base = results['interpreter'].medianMs;
  const withCompiler = results['compiler tier'].medianMs;
  const geo = ratios.length > 0
    ? Math.exp(ratios.reduce((a, r) => a + Math.log(r.ratio), 0) / ratios.length)
    : NaN;

  console.log('');
  console.log(`Transfer, total:              ${(base / withCompiler).toFixed(2)}x`);
  console.log(`Transfer, per-file geometric: ${geo.toFixed(2)}x over ${ratios.length} measurable files`);
  console.log('');
  console.log('Slowest files, which the total is really reporting:');
  console.log('|  interpreted |    compiled | speedup | file |');
  console.log('|--------------|-------------|---------|------|');
  for (const r of ratios.slice(0, 8)) {
    const share = 100 * r.baseMs / base;
    console.log(`| ${(r.baseMs.toFixed(0) + ' ms').padStart(12)} | ${(r.compiledMs.toFixed(0) + ' ms').padStart(11)} ` +
      `| ${(r.ratio.toFixed(2) + 'x').padStart(7)} | ${r.name}${share > 20 ? ` (${share.toFixed(0)}% of total)` : ''} |`);
  }
  console.log('');
  console.log(`Procedures the workload defined and the tier compiled: ` +
    `${results['compiler tier'].workloadCompiled}`);

  const c = results['compiler tier'].compilation;
  if (c) {
    console.log('');
    console.log(`Procedures compiled: ${c.compiled}, declined: ${c.declined}`);
    for (const [reason, count] of Object.entries(c.reasons).sort((a, b) => b[1] - a[1])) {
      console.log(`  ${String(count).padStart(4)}  ${reason}`);
    }
  }

  console.log('');
  console.log('Coverage of this workload, against the microbenchmarks it should be');
  console.log('compared to (those use 16 distinct callables, 98.0% of calls inlined):');
  for (const [label, r] of Object.entries(results)) {
    const cov = r.coverage;
    console.log(`  ${label.padEnd(13)} ${cov.distinct} distinct callables, ` +
      `${(100 * cov.inlinedShare).toFixed(1)}% of ${cov.calls.toLocaleString()} calls inlined, ` +
      `${r.steps.toLocaleString()} evaluator steps`);
  }
  console.log('  (Absolute call counts are not comparable across tiers -- compiling a');
  console.log('   procedure changes how it is reached. The inlined *share* is.)');

  const failures = results['interpreter'].failures;
  if (failures.length > 0) {
    console.log('');
    console.log(`${failures.length} file(s) did not run to completion and are excluded from the`);
    console.log('workload in effect, so they are listed rather than silently dropped:');
    for (const f of failures) console.log(`  ${f.file}: ${f.error}`);
  }

  console.log('');
  console.log('--- JSON Results ---');
  console.log(JSON.stringify(results, null, 2));
}

main().catch((err) => {
  console.error('Macro-benchmark failed:', err);
  process.exit(1);
});
