/**
 * The differential fuzzer, from the command line, for runs longer than the
 * one inside `npm test`.
 *
 * Run with: node tests/fuzz/run_fuzz.js [--from N] [--count M] [--show]
 *
 * Runs the programs from seed N (1 by default) to N + M - 1 (M 200 by
 * default) in both tiers, and prints each disagreement with its seed and
 * program; `--show` prints every program. A seed reproduces its program
 * exactly, so a disagreement found here can be turned into a test.
 */

import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { createGenerator, createTiers, runBoth } from './fuzz_harness.js';

const projectRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const args = process.argv.slice(2);
const valueOf = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i >= 0 ? Number(args[i + 1]) : fallback;
};
const from = valueOf('--from', 1);
const count = valueOf('--count', 200);

const generate = await createGenerator((p) => fs.promises.readFile(path.join(projectRoot, p), 'utf-8'));
const tiers = createTiers();
let disagreements = 0, compiled = 0, reentered = 0, errors = 0, slowest = { ms: 0 };
const start = Date.now();
for (let seed = from; seed < from + count; seed++) {
  const program = generate(seed);
  const result = runBoth(tiers, program);
  compiled += result.compiledCount;
  if (result.reference.startsWith('error:')) errors++;
  // The driver's records: three when the saved continuation was re-entered.
  if (/^\(\(\(.*\) \(.*\) \(.*\)\)/.test(result.reference)) reentered++;
  if (result.ms > slowest.ms) slowest = { ms: result.ms, seed };
  if (args.includes('--show') || !result.agree) {
    console.log(`\n=== seed ${seed}${result.agree ? '' : ': DISAGREE'} (${result.ms} ms, compiled ${program.compiled.join(' ')}) ===`);
    for (const form of program.forms) console.log(form);
    console.log(`interpreted: ${result.reference}`);
    console.log(`compiled:    ${result.compiled}`);
  }
  if (!result.agree) disagreements++;
}
console.log(`\n${count} programs in ${Date.now() - start} ms: ${disagreements} disagreements; `
  + `${compiled} procedures compiled; ${reentered} re-entered a saved continuation; `
  + `${errors} ended in an uncaught error; slowest ${slowest.ms} ms (seed ${slowest.seed})`);
if (disagreements > 0) process.exitCode = 1;
