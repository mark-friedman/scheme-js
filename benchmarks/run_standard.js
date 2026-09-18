/**
 * Standard benchmark suite for scheme-js-4.
 *
 * Runs the portable programs in `benchmarks/programs/` and reports timings plus
 * correctness. This suite is deliberately separate from `run_benchmarks.js`,
 * which is framed around the numeric tower; this one is framed around the three
 * cost centres a compiler has to address (call throughput, tail calls,
 * allocation) plus the four continuation programs that constrain the
 * calling-convention decision.
 *
 * Usage:
 *   node benchmarks/run_standard.js [--profile quick|canonical] [--runs N]
 *   npm run benchmark:standard
 */

import os from 'os';

import { BENCHMARKS, sizeFor } from './programs/manifest.js';
import { runBenchmark } from './lib/harness.js';

/**
 * Parses command-line options.
 * @returns {{profile: string, runs: number}} The selected profile and repetition count.
 */
function parseArgs() {
  const args = process.argv.slice(2);
  const profileIndex = args.indexOf('--profile');
  const runsIndex = args.indexOf('--runs');
  return {
    profile: profileIndex >= 0 ? args[profileIndex + 1] : 'quick',
    runs: runsIndex >= 0 ? parseInt(args[runsIndex + 1], 10) : 5
  };
}

function main() {
  const { profile, runs } = parseArgs();

  console.log('='.repeat(72));
  console.log(`scheme-js-4 standard benchmarks  (profile: ${profile}, runs: ${runs})`);
  console.log('='.repeat(72));
  console.log(`Node ${process.version} on ${os.platform()}/${os.arch()}`);
  console.log('');

  const results = {};
  let failures = 0;
  let category = '';

  for (const bench of BENCHMARKS) {
    if (bench.category !== category) {
      category = bench.category;
      console.log(`--- ${category} ---`);
    }

    const size = sizeFor(bench, profile);
    process.stdout.write(`  ${bench.name} (size ${size}): `);

    const { median, result, error } = runBenchmark(bench, size, runs);

    if (error) {
      console.log(`ERROR: ${error}`);
      results[bench.name] = { size, ms: null, result: null, error };
      failures++;
      continue;
    }

    // `expected` is only meaningful at the quick size, since the canonical
    // sizes produce different values. Cross-implementation agreement, via
    // compare_implementations.js, is the stronger correctness check.
    let status = '';
    if (profile === 'quick' && bench.expected !== null) {
      const expected = bench.expected;
      if (result !== expected) {
        status = `  WRONG (expected ${expected}, got ${result})`;
        failures++;
      } else {
        status = '  ok';
      }
    } else {
      status = `  = ${result}`;
    }

    console.log(`${median.toFixed(1)} ms${status}`);
    results[bench.name] = { size, ms: Number(median.toFixed(3)), result, error: null };
  }

  console.log('');
  if (failures > 0) {
    console.log(`${failures} benchmark(s) produced wrong results or errored.`);
  } else {
    console.log('All benchmarks produced correct results.');
  }

  console.log('');
  console.log('--- JSON Results ---');
  console.log(JSON.stringify({
    suite: 'standard',
    profile,
    runs,
    nodeVersion: process.version,
    platform: `${os.platform()}/${os.arch()}`,
    results
  }, null, 2));

  if (failures > 0) process.exitCode = 1;
}

main();
