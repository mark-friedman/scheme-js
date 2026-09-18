/**
 * Collects every Stage 2a measurement into one report.
 *
 * Puts the two conventions next to each other and next to the interpreter and
 * the reference implementations, so the numbers can be read as "what does the
 * compiler buy" and not only as "which convention wins".
 */

import { BENCHMARKS } from '../../benchmarks/programs/manifest.js';
import { compile, compileSource } from './compile.js';
import { runBenchmark } from '../../benchmarks/lib/harness.js';
import * as RT from './runtime.js';
import * as STK from './stack_machine.js';

const RUNS = 7;

/**
 * Times a compiled entry point, taking the median.
 * @param {function(): *} entry - Compiled program.
 * @returns {{median: number, value: *}} Median milliseconds and result.
 */
function time(entry) {
  const times = [];
  let value;
  for (let i = 0; i < RUNS; i++) {
    STK.reset();
    const start = performance.now();
    value = entry();
    times.push(performance.now() - start);
  }
  times.sort((a, b) => a - b);
  return { median: times[Math.floor(times.length / 2)], value };
}

const rows = {};

for (const bench of BENCHMARKS) {
  const size = bench.quick;
  const row = { size, capturing: Boolean(bench.usesCallCC), multiShot: Boolean(bench.multiShot) };

  for (const convention of ['A', 'B']) {
    const { run, source } = compile(bench.file, size, convention);
    const { median, value } = time(run);
    row[convention] = {
      ms: Number(median.toFixed(2)),
      bytes: source.length,
      ok: RT.render(value) === bench.expected
    };
  }

  const interpreted = runBenchmark(bench, size, 3);
  row.interpreter = Number(interpreted.median.toFixed(2));

  rows[bench.name] = row;
}

console.log('='.repeat(96));
console.log(`Stage 2a results (median of ${RUNS} runs)`);
console.log('='.repeat(96));
console.log('');
console.log('| Benchmark | capture | A (explicit) | B (native) | winner | interpreter | A vs interp | B vs interp |');
console.log('|-----------|---------|--------------|------------|--------|-------------|-------------|-------------|');

for (const [name, row] of Object.entries(rows)) {
  const winner = row.A.ms < row.B.ms
    ? `A ${(row.B.ms / row.A.ms).toFixed(2)}x`
    : `B ${(row.A.ms / row.B.ms).toFixed(2)}x`;
  console.log(
    `| ${name.padEnd(9)} | ${(row.capturing ? (row.multiShot ? 'multi' : 'yes') : 'no').padEnd(7)} ` +
    `| ${(row.A.ms + ' ms').padEnd(12)} | ${(row.B.ms + ' ms').padEnd(10)} | ${winner.padEnd(6)} ` +
    `| ${(row.interpreter + ' ms').padEnd(11)} ` +
    `| ${(row.interpreter / row.A.ms).toFixed(0)}x`.padEnd(14) +
    `| ${(row.interpreter / row.B.ms).toFixed(0)}x`.padEnd(14) + '|');
}

const wrong = Object.entries(rows).filter(([, r]) => !r.A.ok || !r.B.ok);
console.log('');
console.log(wrong.length === 0
  ? 'Both conventions produced correct results on all eight benchmarks.'
  : `WRONG RESULTS: ${wrong.map(([n]) => n).join(', ')}`);

const totalA = Object.values(rows).reduce((a, r) => a + r.A.bytes, 0);
const totalB = Object.values(rows).reduce((a, r) => a + r.B.bytes, 0);
console.log('');
console.log(`Generated code size across all benchmarks: A ${totalA} bytes, B ${totalB} bytes ` +
  `(B/A = ${(totalB / totalA).toFixed(2)})`);

const geo = (pick) => Math.exp(
  Object.values(rows).reduce((a, r) => a + Math.log(r.interpreter / pick(r)), 0) /
  Object.keys(rows).length);
console.log(`Geometric mean speedup over the interpreter: A ${geo((r) => r.A.ms).toFixed(1)}x, ` +
  `B ${geo((r) => r.B.ms).toFixed(1)}x`);

console.log('');
console.log('--- JSON Results ---');
console.log(JSON.stringify(rows, null, 2));
