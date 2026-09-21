/**
 * Benchmark manifest.
 *
 * Each entry names a portable R7RS program in `benchmarks/programs/` together
 * with the sizes to run it at. Programs are written so that the driver supplies
 * `bench-size` and then calls `(bench-run)`, which keeps the same sources usable
 * under scheme-js-4, Gambit and Racket without modification.
 *
 * Two size profiles exist:
 *
 *  - `canonical` is the size used in the published literature (Thivierge &
 *    Feeley, SFP 2012) so our numbers can be placed next to theirs directly.
 *    Most canonical sizes are currently far too slow to run under this
 *    interpreter -- `fib` at 35 takes roughly 70 seconds.
 *  - `quick` is sized so the whole suite completes in a workable time on the
 *    interpreter as it stands today.
 *
 * As the implementation gets faster, raise `quick` towards `canonical` rather
 * than adding new benchmarks. The profile used is always recorded in the
 * results, because a time is meaningless without it.
 */

/**
 * @typedef {Object} Benchmark
 * @property {string} name - Identifier used in results and baselines.
 * @property {string} file - Program filename within `benchmarks/programs/`.
 * @property {string} category - Grouping for report output.
 * @property {number} quick - Size used by the `quick` profile.
 * @property {number} canonical - Size used in the published literature.
 * @property {string} expected - Rendered result at the `quick` size, verified by
 *   three-way agreement between scheme-js-4, Gambit and Racket.
 * @property {boolean} [usesCallCC] - True if the program captures continuations.
 * @property {boolean} [multiShot] - True if correctness requires multi-shot continuations.
 */

/** @type {Benchmark[]} */
export const BENCHMARKS = [
  // --- Call throughput, no continuations -----------------------------------
  {
    name: 'fib', file: 'fib.scm', category: 'Call throughput',
    quick: 25, canonical: 35, expected: '121393'
  },
  {
    name: 'tak', file: 'tak.scm', category: 'Call throughput',
    quick: 18, canonical: 18, expected: '7'
  },
  {
    name: 'oddeven', file: 'oddeven.scm', category: 'Tail calls',
    quick: 100000, canonical: 100000000, expected: '#f'
  },
  {
    name: 'nqueens', file: 'nqueens.scm', category: 'Allocation',
    quick: 8, canonical: 12, expected: '92'
  },

  // --- Continuations -------------------------------------------------------
  // btsearch and
  // threads additionally require multi-shot semantics, so a wrong answer there
  // is a correctness failure rather than a slow result.
  {
    name: 'ctak', file: 'ctak.scm', category: 'Continuations',
    quick: 18, canonical: 22, expected: '9', usesCallCC: true
  },
  {
    name: 'contfib', file: 'contfib.scm', category: 'Continuations',
    quick: 20, canonical: 30, expected: '10946', usesCallCC: true
  },
  {
    name: 'btsearch', file: 'btsearch.scm', category: 'Continuations',
    quick: 200, canonical: 2000, expected: '(200 . 200)', usesCallCC: true, multiShot: true
  },
  {
    name: 'threads', file: 'threads.scm', category: 'Continuations',
    quick: 400, canonical: 2000, expected: '4000', usesCallCC: true, multiShot: true
  }
];

/**
 * Looks up the size for a benchmark under a given profile.
 * @param {Benchmark} bench - The benchmark entry.
 * @param {string} profile - Either 'quick' or 'canonical'.
 * @returns {number} The size to run at.
 */
export function sizeFor(bench, profile) {
  const size = bench[profile];
  if (size === undefined) {
    throw new Error(`Unknown profile '${profile}' for benchmark '${bench.name}'`);
  }
  return size;
}
