/**
 * Stage 2a bake-off runner.
 *
 * Compiles each benchmark under both calling conventions, checks the result
 * against the value the interpreter and the reference implementations agree on,
 * and times it. Correctness is checked first and a wrong answer suppresses the
 * timing, because a fast wrong compiler is not a data point.
 *
 * Usage: node experiments/stage2a/run.js [--runs N] [--only name,name]
 */

import { BENCHMARKS } from '../../benchmarks/programs/manifest.js';
import { compile } from './compile.js';
import * as RT from './runtime.js';
import * as STK from './stack_machine.js';

const args = process.argv.slice(2);
const runsIndex = args.indexOf('--runs');
const RUNS = runsIndex >= 0 ? parseInt(args[runsIndex + 1], 10) : 5;
const onlyIndex = args.indexOf('--only');
const ONLY = onlyIndex >= 0 ? new Set(args[onlyIndex + 1].split(',')) : null;

/**
 * Times a compiled entry point.
 * @param {function(): *} entry - The compiled program.
 * @param {number} runs - Repetitions.
 * @returns {{median: number, value: *}} Median milliseconds and the result.
 */
function time(entry, runs) {
  const times = [];
  let value;
  for (let i = 0; i < runs; i++) {
    STK.reset();
    const start = performance.now();
    value = entry();
    times.push(performance.now() - start);
  }
  times.sort((a, b) => a - b);
  return { median: times[Math.floor(times.length / 2)], value };
}

const results = {};

console.log('='.repeat(76));
console.log(`Stage 2a calling-convention bake-off (runs: ${RUNS})`);
console.log('='.repeat(76));
console.log('A = explicit frame stack   B = native JavaScript stack');
console.log('');
console.log('| Benchmark | size | A (explicit) | B (native) | B/A | expected | A ok | B ok |');
console.log('|-----------|------|--------------|------------|-----|----------|------|------|');

for (const bench of BENCHMARKS) {
  if (ONLY && !ONLY.has(bench.name)) continue;
  const size = bench.quick;
  const row = { size, expected: bench.expected };

  for (const convention of ['A', 'B']) {
    try {
      const { run } = compile(bench.file, size, convention);
      const { median, value } = time(run, RUNS);
      const rendered = RT.render(value);
      row[convention] = { ms: median, result: rendered, ok: rendered === bench.expected };
    } catch (e) {
      row[convention] = { ms: null, result: null, ok: false, error: e.message.slice(0, 90) };
    }
  }

  const fmt = (r) => (r.ms === null ? 'ERROR' : `${r.ms.toFixed(1)} ms`);
  const ratio = (row.A.ms && row.B.ms) ? (row.B.ms / row.A.ms).toFixed(2) : '—';
  console.log(
    `| ${bench.name.padEnd(9)} | ${String(size).padEnd(4)} | ${fmt(row.A).padEnd(12)} ` +
    `| ${fmt(row.B).padEnd(10)} | ${String(ratio).padEnd(3)} | ${String(bench.expected).padEnd(8)} ` +
    `| ${row.A.ok ? ' ok ' : 'WRONG'} | ${row.B.ok ? ' ok ' : 'WRONG'} |`);

  results[bench.name] = row;
}

console.log('');
for (const [name, row] of Object.entries(results)) {
  for (const c of ['A', 'B']) {
    if (row[c].error) console.log(`  ${name} [${c}] error: ${row[c].error}`);
    else if (!row[c].ok) console.log(`  ${name} [${c}] wrong: got ${row[c].result}, expected ${row.expected}`);
  }
}

console.log('');
console.log('--- JSON Results ---');
console.log(JSON.stringify(results, null, 2));
