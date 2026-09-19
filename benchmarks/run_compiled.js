/**
 * Benchmarks the compiler tier against the interpreter on the standard suite.
 *
 * Runs each program twice in a fresh environment -- once interpreted, once with
 * its definitions compiled -- and reports both the speedup and how many of the
 * program's definitions the compiler accepted. A large speedup on a program
 * where nothing was compiled would mean the harness was measuring the wrong
 * thing, so the acceptance count is reported alongside.
 *
 * Usage: node benchmarks/run_compiled.js [--runs N]
 */

import fs from 'fs';
import path from 'path';

import { BENCHMARKS } from './programs/manifest.js';
import { PROGRAM_DIR, createBenchmarkInterpreter, renderResult, RUN_OPTIONS } from './lib/harness.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { DefineNode } from '../src/core/interpreter/ast_nodes.js';
import { compileProgram } from '../src/compiler/index.js';
import { settle } from '../src/compiler/runtime.js';

const runsIndex = process.argv.indexOf('--runs');
const RUNS = runsIndex >= 0 ? parseInt(process.argv[runsIndex + 1], 10) : 5;

/**
 * Loads a benchmark and returns a timed entry point for one tier.
 * @param {Object} bench - Manifest entry.
 * @param {number} size - Value to bind to `bench-size`.
 * @param {boolean} useCompiler - Whether to compile definitions.
 * @returns {{run: function(): *, compiled: number, declined: Array<Object>}} Entry point.
 */
function prepare(bench, size, useCompiler) {
  const { interpreter, env, run, compile } = createBenchmarkInterpreter();
  run(`(define bench-size ${size})`);
  const source = fs.readFileSync(path.join(PROGRAM_DIR, bench.file), 'utf8');
  const asts = parse(source).map((form) => analyze(form));

  let outcome = { compiled: [], declined: [] };
  if (useCompiler) {
    const definitions = asts.filter((a) => a instanceof DefineNode);
    const others = asts.filter((a) => !(a instanceof DefineNode));
    outcome = compileProgram(definitions, env, interpreter);
    for (const ast of others) interpreter.run(ast, env, [], undefined, RUN_OPTIONS);
  } else {
    for (const ast of asts) interpreter.run(ast, env, [], undefined, RUN_OPTIONS);
  }

  const entry = compile('(bench-run)');
  return {
    run: () => settle(interpreter.run(entry, env, [], undefined, RUN_OPTIONS)),
    compiled: outcome.compiled.length,
    declined: outcome.declined
  };
}

/**
 * Times an entry point.
 * @param {function(): *} entry - The callable.
 * @returns {{median: number, value: *}} Median milliseconds and last value.
 */
function time(entry) {
  const times = [];
  let value;
  for (let i = 0; i < RUNS; i++) {
    const start = performance.now();
    value = entry();
    times.push(performance.now() - start);
  }
  times.sort((a, b) => a - b);
  return { median: times[Math.floor(times.length / 2)], value };
}

console.log('='.repeat(86));
console.log(`Compiler tier vs interpreter (median of ${RUNS} runs)`);
console.log('='.repeat(86));
console.log('');
console.log('| Benchmark | size | interpreted | compiled | speedup | procs compiled | correct |');
console.log('|-----------|------|-------------|----------|---------|----------------|---------|');

const results = {};
for (const bench of BENCHMARKS) {
  const size = bench.quick;
  const interpreted = time(prepare(bench, size, false).run);
  const withCompiler = prepare(bench, size, true);
  const compiled = time(withCompiler.run);

  const rendered = renderResult(compiled.value);
  const ok = rendered === bench.expected;
  const speedup = interpreted.median / compiled.median;

  console.log(
    `| ${bench.name.padEnd(9)} | ${String(size).padEnd(4)} ` +
    `| ${(interpreted.median.toFixed(1) + ' ms').padEnd(11)} ` +
    `| ${(compiled.median.toFixed(1) + ' ms').padEnd(8)} ` +
    `| ${(speedup.toFixed(2) + 'x').padEnd(7)} ` +
    `| ${String(withCompiler.compiled).padEnd(14)} | ${ok ? '  ok   ' : ' WRONG '} |`);

  results[bench.name] = {
    size,
    interpretedMs: Number(interpreted.median.toFixed(2)),
    compiledMs: Number(compiled.median.toFixed(2)),
    speedup: Number(speedup.toFixed(3)),
    proceduresCompiled: withCompiler.compiled,
    declined: withCompiler.declined,
    correct: ok
  };
}

const usable = Object.values(results).filter((r) => r.proceduresCompiled > 0);
if (usable.length > 0) {
  const geo = Math.exp(usable.reduce((a, r) => a + Math.log(r.speedup), 0) / usable.length);
  console.log('');
  console.log(`Geometric mean speedup where anything was compiled: ${geo.toFixed(2)}x ` +
    `(${usable.length} of ${Object.keys(results).length} benchmarks)`);
}

const declinedAll = Object.entries(results).filter(([, r]) => r.proceduresCompiled === 0);
if (declinedAll.length > 0) {
  console.log('');
  console.log('Nothing compiled in these, so their timings are interpreter-only:');
  for (const [name, r] of declinedAll) {
    const reasons = [...new Set(r.declined.map((d) => d.reason))];
    console.log(`  ${name}: ${reasons.join('; ') || 'no definitions'}`);
  }
}

const wrong = Object.entries(results).filter(([, r]) => !r.correct);
if (wrong.length > 0) {
  console.log('');
  console.log(`WRONG RESULTS: ${wrong.map(([n]) => n).join(', ')}`);
  process.exitCode = 1;
}

console.log('');
console.log('--- JSON Results ---');
console.log(JSON.stringify(results, null, 2));
