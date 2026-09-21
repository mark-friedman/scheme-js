/**
 * Asserts that forty-one real Scheme programs still compute the right answers,
 * under the interpreter and under the compiler tier.
 *
 * ## Why these programs are a test and not only a benchmark
 *
 * They come from the Gabriel and Gambit lineage by way of Larceny and
 * `ecraven/r7rs-benchmarks`, they predate this project by decades, and each one
 * carries an expected result that its own `run-benchmark` checks. Nobody here
 * chose them, which is the point: the hand-written suites test the cases
 * somebody thought of, and these test whatever a partial evaluator, an Earley
 * parser, a theorem prover and a ray tracer happen to do.
 *
 * That difference is not theoretical. Three compiler defects in a single
 * increment were found by these programs and missed by 2,344 unit tests -- a
 * free-variable scan that bound internal `define` names too late, a
 * box-creation walk that missed `if`, and a staleness check that compared
 * renamed parameter names. All three produced wrong answers in working
 * programs while every unit test stayed green, because each needed a procedure
 * shaped in a way no unit test happened to build.
 *
 * ## Both tiers
 *
 * Every program runs twice, interpreted and compiled, and both must answer
 * correctly. The interpreter is the reference semantics, so an interpreted
 * failure is a much larger finding than a compiled one, and a disagreement
 * between the two localises a compiler bug immediately.
 *
 * ## Node only
 *
 * The programs and their input data are read from disk, and several open their
 * own data files by relative path. Running them in a browser would mean
 * bundling the suite and rewriting those paths inside sources that are
 * deliberately kept verbatim.
 *
 * Also runnable on its own, with progress and the `slow` programs available:
 *
 *     npm run test:programs -- --slow
 */

import { checkPrograms, labelFor, plannedRuns } from '../../benchmarks/lib/correctness.js';

/**
 * Runs the program-correctness pass and reports each run to the logger.
 *
 * @param {Object} logger - Test logger with `title`, `pass`, `fail`, `skip`.
 * @param {Object} [options] - Passed through to `checkPrograms`.
 * @returns {Promise<void>} Resolves when every program has run.
 */
export async function runProgramCorrectnessTests(logger, options = {}) {
  if (typeof process === 'undefined') {
    logger.skip('Canonical program correctness (Node.js only)');
    return;
  }

  logger.title('Canonical R7RS program correctness');

  const results = await checkPrograms(options);

  // Reported in planned order rather than completion order, so that a failure
  // list reads the same way twice running even though the runs do not finish in
  // a fixed order.
  for (const result of results) {
    if (result.ok) {
      logger.pass(labelFor(result));
    } else {
      logger.fail(`${labelFor(result)}: ${result.reason}`);
    }
  }

  // A tier that compiled nothing would make every compiled row above a second
  // interpreted run wearing a different label, and the suite would report full
  // agreement while testing half of what it claims to. This is the check that
  // the programs reached the compiler at all.
  const compiledRuns = results.filter((r) => r.useCompiler && r.ok);
  const withCode = compiledRuns.filter((r) => r.compiled > 0);
  if (compiledRuns.length > 0 && withCode.length < compiledRuns.length / 2) {
    logger.fail(`Compiler tier produced code for only ${withCode.length} of `
      + `${compiledRuns.length} programs; the compiled runs are not testing the compiler`);
  }
}

export { plannedRuns };
