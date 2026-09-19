/**
 * Manifest for the canonical R7RS benchmark suite vendored in this directory.
 *
 * ## What the fields mean
 *
 * `workload` is the field that matters most and the reason this manifest exists
 * rather than a bare list of filenames. The eight microbenchmarks in
 * `benchmarks/programs/` turned out to be overfitted to the optimizations
 * chosen against them -- 98% of their calls landed on a primitive the compiler
 * inlines, against 34% in real code (R20 in `docs/compiler_strategy.md`). The
 * defence against repeating that is not a larger suite but a *classified* one:
 * report each workload class separately and never blend them into one number,
 * so that an optimization which helps hot fixnum loops and does nothing for
 * string processing reads as exactly that.
 *
 * `params` supplies replacement text for everything after the repetition count
 * when the canonical size is out of reach. Canonical `fib` is five repetitions
 * of `fib(40)`, which takes Gambit's interpreter 143 seconds and this
 * implementation rather longer. Where `params` is null the canonical input file
 * is used unchanged and results are directly comparable to the numbers
 * published in `ecraven/r7rs-benchmarks`.
 *
 * Every replacement parameter set was sized here, and **its expected result was
 * derived from Gambit, not from our own output** -- a benchmark whose expected
 * value came from the implementation under test cannot detect that the
 * implementation is wrong.
 *
 * `status` is `'ok'` for programs that run and agree, `'slow'` for programs
 * that run but have no parameter that can be reduced without changing what they
 * measure, and `'blocked'` for programs this implementation cannot run yet. The
 * blocked entries are kept rather than dropped: they are the evidence for four
 * conformance gaps this suite found on its first run, and each becomes
 * available the moment its gap closes.
 */

/**
 * @typedef {Object} R7rsBenchmark
 * @property {string} name - Program name, matching `src/<name>.scm`.
 * @property {string} workload - Workload class; see `WORKLOADS`.
 * @property {string|null} params - Replacement text for everything after the
 *   repetition count, or null to use the canonical input verbatim.
 * @property {string} status - `'ok'`, `'slow'` or `'blocked'`.
 * @property {string} [note] - Why the size was changed, or what blocks it.
 */

/**
 * Workload classes, with what each one is meant to stress.
 *
 * These are reported separately and never averaged together. There is no
 * "average Scheme program" to weight them against, so a blended figure would
 * bake a guess about an unknown target workload into every future decision.
 * @type {Object<string, string>}
 */
export const WORKLOADS = {
  call: 'Procedure call and recursion throughput',
  fixnum: 'Small exact-integer arithmetic',
  bignum: 'Arbitrary-precision exact arithmetic',
  flonum: 'Inexact real and complex arithmetic',
  list: 'Pairs, symbols and symbolic processing',
  vector: 'Vectors, bytevectors and records',
  string: 'Strings, characters and text',
  continuation: 'call/cc and dynamic-wind'
};

/** @type {R7rsBenchmark[]} */
export const R7RS_BENCHMARKS = [
  // --- Procedure call throughput -------------------------------------------
  {
    name: 'fib', workload: 'call', status: 'ok', params: '25\n75025',
    note: 'canonical is fib(40); Gambit\'s interpreter needs 143 s for the canonical run'
  },
  {
    name: 'tak', workload: 'call', status: 'ok', params: '18\n12\n6\n7',
    note: 'the "older inputs" documented in the canonical input file'
  },
  {
    name: 'takl', workload: 'call', status: 'ok',
    params: '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)\n'
      + '(12 11 10 9 8 7 6 5 4 3 2 1)\n'
      + '(6 5 4 3 2 1)\n7',
    note: 'list-based tak, sized to match our `tak` entry at 18/12/6. The expected '
      + 'value is the LENGTH of the result list, not the list -- the program compares '
      + '(length result). The "old inputs" documented in the canonical file are '
      + 'tak(32,16,8), which is far larger than it looks and did not finish here in two '
      + 'minutes -- a sizing mistake on our side, not an implementation pathology'
  },
  {
    name: 'ack', workload: 'call', status: 'ok', params: '3\n6\n509',
    note: 'canonical is ack(3,12); expected value derived from Gambit'
  },
  {
    name: 'cpstak', workload: 'call', status: 'ok', params: '18\n12\n6\n7',
    note: 'the "older inputs" documented in the canonical input file; CPS-converted tak, '
      + 'no call/cc but deep closure chains'
  },
  { name: 'deriv', workload: 'call', status: 'ok', params: null },
  { name: 'divrec', workload: 'call', status: 'ok', params: null },
  { name: 'diviter', workload: 'call', status: 'ok', params: null },

  // --- Exact integer arithmetic --------------------------------------------
  { name: 'sum', workload: 'fixnum', status: 'ok', params: null },
  { name: 'primes', workload: 'fixnum', status: 'ok', params: null },
  {
    name: 'nqueens', workload: 'fixnum', status: 'ok', params: '9\n352',
    note: 'canonical is 13; expected value derived from Gambit'
  },
  { name: 'puzzle', workload: 'fixnum', status: 'ok', params: null },
  { name: 'pi', workload: 'bignum', status: 'ok', params: null },
  { name: 'chudnovsky', workload: 'bignum', status: 'ok', params: null },

  // --- Inexact arithmetic ---------------------------------------------------
  {
    name: 'fibfp', workload: 'flonum', status: 'ok', params: '25.\n75025.',
    note: 'canonical is fibfp(35.0); expected value derived from Gambit'
  },
  { name: 'sumfp', workload: 'flonum', status: 'ok', params: null },
  { name: 'mbrot', workload: 'flonum', status: 'ok', params: null },
  { name: 'mbrotZ', workload: 'flonum', status: 'ok', params: null, note: 'complex numbers' },
  {
    name: 'fft', workload: 'flonum', status: 'ok', params: '16384\n0.0\n0.0',
    note: 'canonical is 65536; flonum vector work'
  },
  { name: 'simplex', workload: 'flonum', status: 'ok', params: null },
  { name: 'pnpoly', workload: 'flonum', status: 'ok', params: null },
  {
    name: 'ray', workload: 'flonum', status: 'slow', params: null,
    note: 'no parameter reduces without changing the scene; ~9 s per iteration here'
  },
  {
    name: 'nucleic', workload: 'flonum', status: 'slow', params: null,
    note: 'takes no size parameter; ~6.5 s per iteration here'
  },

  // --- Symbolic processing --------------------------------------------------
  { name: 'browse', workload: 'list', status: 'ok', params: null },
  { name: 'destruc', workload: 'list', status: 'ok', params: null },
  { name: 'peval', workload: 'list', status: 'ok', params: null },
  { name: 'scheme', workload: 'list', status: 'ok', params: null },
  {
    name: 'maze', workload: 'list', status: 'ok', params: null,
    note: 'correct interpreted; returns a wrong answer under the compiler tier because '
      + 'dig-maze escapes with (quit #f) and the escape unwinds past compiled make-maze, '
      + 'which never mentions call/cc. This is R28\'s unsoundness, not a compiler defect (R34)'
  },
  { name: 'mazefun', workload: 'list', status: 'ok', params: null },
  { name: 'quicksort', workload: 'list', status: 'ok', params: null },
  {
    name: 'conform', workload: 'list', status: 'slow', params: null,
    note: 'takes no size parameter; ~4 s per iteration here'
  },
  {
    name: 'earley', workload: 'list', status: 'ok', params: '10\n4862',
    note: 'canonical is 15; expected value derived from Gambit'
  },
  {
    name: 'graphs', workload: 'list', status: 'ok', params: '5\n596',
    note: 'canonical is 7; expected value derived from Gambit'
  },
  {
    name: 'lattice', workload: 'list', status: 'ok', params: '33\n10',
    note: 'canonical is 44; the program accepts only 33/44/45/54/55'
  },
  {
    name: 'nboyer', workload: 'list', status: 'ok', params: '2\n1813975',
    note: 'canonical is 5; expected value derived from Gambit'
  },
  {
    name: 'sboyer', workload: 'list', status: 'ok', params: '2\n1813975',
    note: 'canonical is 5; the shared-structure variant of nboyer'
  },
  {
    name: 'paraffins', workload: 'list', status: 'ok', params: '17\n24894',
    note: 'canonical is 23; expected value derived from Gambit'
  },
  {
    name: 'triangl', workload: 'list', status: 'slow', params: null,
    note: 'the parameters are board positions, not a size; ~18 s per iteration here'
  },

  // --- Vectors and bytevectors ---------------------------------------------
  {
    name: 'array1', workload: 'vector', status: 'ok', params: '100000\n100000',
    note: 'canonical is 1000000'
  },
  {
    name: 'bv2string', workload: 'vector', status: 'ok', params: '300\n300\n0',
    note: 'canonical is 1000/1000; bytevector/string conversion'
  },

  // --- Strings and text -----------------------------------------------------
  { name: 'string', workload: 'string', status: 'ok', params: null },
  { name: 'read1', workload: 'string', status: 'ok', params: null, note: 'reads inputs/parsing.data' },

  // --- Continuations --------------------------------------------------------
  {
    name: 'ctak', workload: 'continuation', status: 'ok', params: '18\n12\n6\n7',
    note: 'the "old inputs" documented in the canonical input file'
  },
  {
    name: 'fibc', workload: 'continuation', status: 'ok', params: '20\n6765',
    note: 'canonical is 30; expected value derived from Gambit'
  },
  {
    name: 'dynamic', workload: 'continuation', status: 'ok', params: null,
    note: 'dynamic-wind through a type-inference pass; reads inputs/dynamic.data'
  },

  // --- Blocked: this implementation cannot run these yet ---------------------
  // Each is kept because it is the evidence for a conformance gap, and each
  // becomes available the moment its gap closes. See docs/compiler_strategy.md.
  {
    name: 'gcbench', workload: 'vector', status: 'blocked', params: null,
    note: 'uses record accessors named node.left, node.right; identifiers containing a '
      + 'dot are rejected by extended dot notation, a deliberate interop feature '
      + '(tests/extras/scheme/dot_access_tests.scm)'
  },
  {
    name: 'matrix', workload: 'vector', status: 'blocked', params: null,
    note: 'same dotted-identifier gap'
  },
  {
    name: 'slatex', workload: 'string', status: 'blocked', params: null,
    note: 'same dotted-identifier gap (slatex.ormap and friends)'
  },
  {
    name: 'parsing', workload: 'string', status: 'blocked', params: null,
    note: 'read-char returns a JavaScript string rather than a Scheme character, '
      + 'so list->string rejects what read-char produced'
  },
  {
    name: 'read0', workload: 'string', status: 'blocked', params: null,
    note: 'same read-char gap; also exercises reader syntax we do not accept'
  },
  {
    name: 'equal', workload: 'list', status: 'blocked', params: null,
    note: 'equal? does not terminate on circular structure, which R7RS 6.1 requires. '
      + 'Gambit runs this in 0.08 s; we hang at every size, including the smallest. '
      + 'The program\'s own comment calls a circular list "a worst case for R5RS equal?"'
  }
];

/**
 * Returns the benchmarks that should run under a given profile.
 * @param {string} profile - `'default'` omits `slow` entries; `'full'` includes them.
 * @returns {R7rsBenchmark[]} The selected benchmarks.
 */
export function selectBenchmarks(profile = 'default') {
  return R7RS_BENCHMARKS.filter((b) => {
    if (b.status === 'blocked') return false;
    if (b.status === 'slow') return profile === 'full';
    return true;
  });
}
