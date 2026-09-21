/**
 * Running the canonical R7RS programs for their *answers* rather than their
 * times.
 *
 * ## Why this is separate from the timed suite
 *
 * Every program in `benchmarks/r7rs/` carries an expected result, and
 * `run-r7rs-benchmark` in `common.scm` prints `INCORRECT` when the result does
 * not match. That makes the suite a correctness corpus of forty-one real
 * programs -- larger and far more varied than anything written by hand for this
 * project -- and it was catching defects that the unit tests did not. Three
 * compiler bugs in a single increment surfaced here first: a free-variable scan
 * that bound internal `define` names too late, a box-creation walk that missed
 * `if`, and a staleness guard that compared renamed parameter names.
 *
 * The obstacle to making those checks a test target was never principle, only
 * time: the timed suite calibrates each program to about a second of work and
 * repeats it, which takes twenty minutes. Correctness needs one iteration.
 *
 * ## What being correctness-only buys
 *
 * Two things the timed suite cannot have.
 *
 * Runs go in parallel. A timed run must have the machine to itself or the
 * number means nothing; an answer is the same answer whether or not seven other
 * processes are busy, so this saturates the cores and finishes in a fraction of
 * the wall time.
 *
 * And sizes can shrink. `nboyer` and `sboyer` at their benchmark size are 82 of
 * the suite's 141 seconds and exercise exactly the same code as at size 0, so
 * the manifest gives them a smaller `check` size -- with its expected value
 * taken from Gambit, because an expected value derived from the implementation
 * under test cannot detect that the implementation is wrong.
 *
 * ## Both tiers, always
 *
 * Each program runs interpreted and compiled, and both must answer correctly.
 * The interpreted run is not redundant: it is the reference semantics, and a
 * disagreement between the two is the single most informative failure this
 * suite can produce. Two of the three bugs above showed up as exactly that.
 */

import { execFile } from 'child_process';
import os from 'os';
import path from 'path';
import { fileURLToPath } from 'url';

import { R7RS_BENCHMARKS } from '../r7rs/manifest.js';

const WORKER = path.join(path.dirname(fileURLToPath(import.meta.url)), 'r7rs_worker.js');

/**
 * Wall-clock budget for one program, in seconds.
 *
 * Generous, because these run in parallel and a machine under load is slower
 * than a machine that is not. It is here to stop a program that loops forever
 * from hanging the test run, which has happened, and not to police speed.
 */
export const CHECK_BUDGET_SECONDS = 120;

/**
 * The runs a correctness pass should make.
 *
 * `slow` programs are excluded by default. Each is `slow` precisely because it
 * takes no size parameter that can be reduced, so they cost about thirty
 * seconds between them and are worth having on demand rather than on every
 * commit.
 *
 * @param {Object} [options] - Options.
 * @param {boolean} [options.includeSlow] - Include the `slow` programs.
 * @param {Set<string>|null} [options.only] - Restrict to these program names.
 * @returns {Array<{name: string, workload: string, params: (string|null),
 *   useCompiler: boolean}>} One entry per program per tier.
 */
export function plannedRuns(options = {}) {
  const { includeSlow = false, only = null } = options;
  const runs = [];
  for (const bench of R7RS_BENCHMARKS) {
    if (bench.status === 'blocked') continue;
    if (bench.status === 'slow' && !includeSlow) continue;
    if (only !== null && !only.has(bench.name)) continue;
    // `check` overrides `params` here and nowhere else, so shrinking a size for
    // the sake of this pass cannot quietly shrink a published benchmark.
    const params = bench.check !== undefined ? bench.check : bench.params;
    for (const useCompiler of [false, true]) {
      runs.push({ name: bench.name, workload: bench.workload, params, useCompiler });
    }
  }
  return runs;
}

/**
 * Runs one program in its own process and reports whether it answered.
 *
 * A child process rather than an in-process call, for the reason
 * `r7rs_worker.js` exists: these programs can exhaust the stack or fail to
 * terminate, and neither should take the test run with it.
 *
 * @param {Object} run - An entry from `plannedRuns`.
 * @returns {Promise<{ok: boolean, reason: (string|null), compiled: number,
 *   definitions: number}>} The outcome.
 */
export function runOne(run) {
  const request = JSON.stringify({
    name: run.name, params: run.params, count: 1, useCompiler: run.useCompiler
  });
  return new Promise((resolve) => {
    execFile(process.execPath, [WORKER, request], {
      timeout: CHECK_BUDGET_SECONDS * 1000, maxBuffer: 16 * 1024 * 1024
    }, (error, stdout) => {
      if (error) {
        const killed = error.killed || error.signal === 'SIGTERM';
        resolve({
          ok: false, compiled: 0, definitions: 0,
          reason: killed
            ? `did not finish within ${CHECK_BUDGET_SECONDS}s`
            : (error.message || 'the worker failed').split('\n')[0].slice(0, 160)
        });
        return;
      }
      let result;
      try {
        result = JSON.parse(stdout);
      } catch {
        resolve({ ok: false, reason: 'the worker printed no result', compiled: 0, definitions: 0 });
        return;
      }
      resolve({
        ok: reasonFor(result) === null,
        reason: reasonFor(result),
        compiled: result.compiled,
        definitions: result.definitions
      });
    });
  });
}

/**
 * Why a worker result counts as a failure, or null if it does not.
 *
 * A missing timing line is a failure and not an absence. The benchmark prints
 * `+!CSVLINE!+` on both the correct and the incorrect path, so a run that
 * printed neither did not reach the end of the program, however quietly it
 * returned.
 *
 * @param {{seconds: (number|null), incorrect: boolean, error: (string|null)}} result -
 *   A worker result.
 * @returns {string|null} The reason, or null when the run answered correctly.
 */
function reasonFor(result) {
  if (result.error) return result.error;
  if (result.incorrect) return 'returned an incorrect result';
  if (result.seconds === null) return 'printed no result line';
  return null;
}

/**
 * Runs a correctness pass, several programs at a time.
 *
 * @param {Object} [options] - Options; `includeSlow` and `only` are passed to
 *   `plannedRuns`.
 * @param {number} [options.concurrency] - Processes to keep in flight.
 * @param {function(Object): void} [options.onResult] - Called with each
 *   `{...run, ...outcome}` as it completes, for progress reporting.
 * @returns {Promise<Array<Object>>} Every run's outcome, in planned order.
 */
export async function checkPrograms(options = {}) {
  const runs = plannedRuns(options);
  const concurrency = options.concurrency ?? Math.max(2, Math.min(8, os.cpus().length));
  const onResult = options.onResult ?? (() => {});

  const results = new Array(runs.length);
  let next = 0;

  /**
   * Takes runs from the shared queue until there are none left.
   * @returns {Promise<void>} Resolves when the queue is empty.
   */
  async function worker() {
    for (;;) {
      const index = next++;
      if (index >= runs.length) return;
      const outcome = await runOne(runs[index]);
      results[index] = { ...runs[index], ...outcome };
      onResult(results[index]);
    }
  }

  await Promise.all(Array.from({ length: concurrency }, worker));
  return results;
}

/**
 * A one-line label for a run, used in test output and on the command line.
 * @param {{name: string, useCompiler: boolean}} run - A planned run.
 * @returns {string} Something like `earley (compiled)`.
 */
export function labelFor(run) {
  return `${run.name} (${run.useCompiler ? 'compiled' : 'interpreted'})`;
}
