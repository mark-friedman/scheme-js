/**
 * Runs the canonical-program correctness pass on its own, with progress.
 *
 * The same checks `npm test` makes, separated out so that a compiler change can
 * be checked against real programs in a few seconds without waiting for the
 * whole suite -- and so the `slow` programs, which the default pass leaves out
 * for taking about thirty seconds between them, can be asked for.
 *
 *     npm run test:programs
 *     npm run test:programs -- --slow
 *     npm run test:programs -- --only earley,peval
 */

import { checkPrograms, labelFor, plannedRuns } from '../../benchmarks/lib/correctness.js';

const args = process.argv.slice(2);
const valueOf = (flag) => {
  const i = args.indexOf(flag);
  return i >= 0 ? args[i + 1] : null;
};

const only = valueOf('--only');
const options = {
  includeSlow: args.includes('--slow'),
  only: only === null ? null : new Set(only.split(','))
};

const planned = plannedRuns(options);
console.log(`Canonical R7RS programs -- ${planned.length / 2} programs, `
  + 'interpreted and compiled, checked against their own expected results.');
console.log('');

const started = Date.now();
let done = 0;
const results = await checkPrograms({
  ...options,
  onResult: (result) => {
    done++;
    const mark = result.ok ? 'ok  ' : 'FAIL';
    const detail = result.ok
      ? (result.useCompiler ? `${result.compiled}/${result.definitions} compiled` : '')
      : result.reason;
    console.log(`  [${String(done).padStart(2)}/${planned.length}] ${mark} `
      + `${labelFor(result).padEnd(26)} ${detail}`);
  }
});

const failed = results.filter((r) => !r.ok);
console.log('');
console.log(`${results.length - failed.length} passed, ${failed.length} failed `
  + `in ${((Date.now() - started) / 1000).toFixed(1)}s`);
if (failed.length > 0) {
  console.log('');
  for (const f of failed) console.log(`  ${labelFor(f)}: ${f.reason}`);
}
process.exit(failed.length > 0 ? 1 : 0);
