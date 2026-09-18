/**
 * Evaluator step counter.
 *
 * Reports how many dispatches, and of which kinds, a benchmark costs. Unlike
 * timings these numbers are deterministic, so they are the right way to check
 * whether an optimization actually removed work. A change that halves the
 * `AppFrame` count has definitely done something; a change that only moves the
 * wall clock might just have got lucky with the JIT.
 *
 * Usage:
 *   node benchmarks/count_steps.js [benchmark ...] [--size N] [--top N]
 *   npm run benchmark:steps
 *
 * With no benchmark named, every program in the manifest is counted at a size
 * small enough to keep the whole sweep quick.
 */

import { BENCHMARKS } from './programs/manifest.js';
import { countBenchmark, SWEEP_SIZES } from './lib/step_counts.js';
import { formatStats } from '../src/debug/instrumentation.js';

function main() {
  const args = process.argv.slice(2);
  const names = args.filter(a => !a.startsWith('--') && !/^\d+$/.test(a));
  const sizeIndex = args.indexOf('--size');
  const topIndex = args.indexOf('--top');
  const top = topIndex >= 0 ? parseInt(args[topIndex + 1], 10) : 12;

  const selected = names.length > 0
    ? BENCHMARKS.filter(b => names.includes(b.name))
    : BENCHMARKS;

  if (selected.length === 0) {
    console.error(`No matching benchmarks. Available: ${BENCHMARKS.map(b => b.name).join(', ')}`);
    process.exit(1);
  }

  const summary = {};

  for (const bench of selected) {
    const size = sizeIndex >= 0
      ? parseInt(args[sizeIndex + 1], 10)
      : (SWEEP_SIZES[bench.name] ?? bench.quick);

    const { stats, result } = countBenchmark(bench, size);
    summary[bench.name] = { size, totalSteps: stats.totalSteps, maxStackDepth: stats.maxStackDepth };

    console.log('='.repeat(66));
    console.log(`${bench.name} (size ${size}) -> ${result}`);
    console.log('='.repeat(66));
    console.log(formatStats(stats, top));
    console.log('');
  }

  if (selected.length > 1) {
    console.log('| Benchmark | size | total steps | max depth |');
    console.log('|-----------|------|-------------|-----------|');
    for (const [name, row] of Object.entries(summary)) {
      console.log(
        `| ${name.padEnd(9)} | ${String(row.size).padEnd(4)} ` +
        `| ${row.totalSteps.toLocaleString().padStart(11)} ` +
        `| ${row.maxStackDepth.toLocaleString().padStart(9)} |`
      );
    }
  }

  console.log('');
  console.log('--- JSON Results ---');
  console.log(JSON.stringify(summary, null, 2));
}

main();
