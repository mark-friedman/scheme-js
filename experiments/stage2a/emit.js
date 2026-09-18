/**
 * Dumps generated code for inspection.
 *
 * Usage: node experiments/stage2a/emit.js <benchmark> <A|B> [size]
 */

import { compileSource } from './compile.js';
import { BENCHMARKS } from '../../benchmarks/programs/manifest.js';

const [name, convention = 'B', size] = process.argv.slice(2);
const bench = BENCHMARKS.find((b) => b.name === name);
if (!bench) {
  console.error(`Unknown benchmark '${name}'. Available: ${BENCHMARKS.map((b) => b.name).join(', ')}`);
  process.exit(1);
}
console.log(compileSource(bench.file, size ? parseInt(size, 10) : bench.quick, convention));
