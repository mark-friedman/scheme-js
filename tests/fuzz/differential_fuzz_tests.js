/**
 * @fileoverview The differential fuzzer inside `npm test`: a fixed run of
 * generated programs, each answered alike interpreted, with chosen procedures
 * compiled, and with the compiler tier choosing.
 *
 * The seeds are fixed, so the run is the same every time and a failure names a
 * seed that reproduces it; `run_fuzz.js` runs as many more as wanted. The count
 * is set by what finds bugs: five reintroduced -- a nested run passing no
 * frames on, the capturing run's frames dropped, the frames an unwind collects
 * stacked the wrong way round, assigned locals never boxed, and a spill saving
 * only what its own block reads -- were each found by one of the first 27
 * programs.
 *
 * The run also checks that the programs still do what makes them worth
 * running: re-enter a saved continuation, recurse deep enough to move frames,
 * end in an uncaught error, and compile procedures at all. A generator that
 * quietly stopped doing one would otherwise leave this passing and useless.
 */

import { assert } from '../harness/helpers.js';
import { createGenerator, createTiers, runBoth } from './fuzz_harness.js';

/**
 * How many programs, from seed 1.
 * @type {number}
 */
const PROGRAMS = 120;

/**
 * Runs the fuzzer's fixed programs.
 * @param {Object} logger - Test logger.
 * @param {Function} loader - Reads a file by its path from the project root.
 * @returns {Promise<void>}
 */
export async function runDifferentialFuzzTests(logger, loader) {
  logger.title('Compiler - Generated Programs Answered Alike by Both Tiers');
  const generate = await createGenerator(loader);
  const tiers = createTiers();
  let compiled = 0, tiered = 0, reentered = 0, deep = 0, uncaught = 0;
  for (let seed = 1; seed <= PROGRAMS; seed++) {
    const program = generate(seed);
    const result = runBoth(tiers, program);
    compiled += result.compiledCount;
    tiered += result.tieredCount;
    if (/^\(\(\(.*\) \(.*\) \(.*\)\)/.test(result.reference)) reentered++;
    if (program.forms.some((form) => form.startsWith('(define (hop '))) deep++;
    if (result.reference.startsWith('error:')) uncaught++;
    if (result.agree) {
      logger.pass(`program ${seed}`);
    } else {
      logger.fail(`program ${seed} (compiled: ${program.compiled.join(' ')}): interpreted ${result.reference}, `
        + `compiled ${result.compiled}, tiered ${result.tiered}; `
        + `run \`node tests/fuzz/run_fuzz.js --from ${seed} --count 1\` to see it`);
    }
  }
  assert(logger, 'the programs compile procedures', compiled > PROGRAMS, true);
  assert(logger, 'and the tier compiles procedures of its own choosing', tiered > PROGRAMS, true);
  assert(logger, 'some re-enter a saved continuation', reentered > 10, true);
  assert(logger, 'some recurse deep enough to move frames', deep > 10, true);
  assert(logger, 'some end in an uncaught error', uncaught > 3, true);
}
