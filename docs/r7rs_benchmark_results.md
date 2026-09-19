# Canonical R7RS benchmark results

First run of the vendored Gabriel/Gambit/Larceny suite, 2026-09-18, darwin/arm64, Node v24.11.1,
against the Stage 2b increment 1b implementation. Methodology, provenance and sizing are in
[benchmarks/r7rs/README.md](../benchmarks/r7rs/README.md).

Reproduce with:

```bash
npm run benchmark:r7rs
```

```bash
npm run benchmark:r7rs-implementations
```

**Results are reported per workload class and never blended into one number.** There is no average
Scheme program to weight the classes against, so a single figure would bake a guess about an unknown
target workload into every future decision — which is exactly how the eight microbenchmarks in
`benchmarks/programs/` came to report a 12x that turned out to be 1.39x on real code.

---

## How far behind we are, by workload class

Geometric mean of per-iteration time relative to each reference, 41 programs. Gambit `gsi` is an
*interpreter*, so it is the fair comparison for our interpreter tier; Racket CS is a compiler and is
the target for the compiler stages.

| Workload class | vs Gambit `gsi` | range | vs Racket CS | programs |
|---|---|---|---|---|
| Strings and characters | **1.7x** | 0.3x – 9.2x | **0.5x** | 2 |
| `vector`, bytevectors | 11.2x | 11.2x – 11.2x | 274x | 2 |
| Small exact integers | 11.3x | 9.4x – 16.2x | 630x | 4 |
| Procedure call | 12.3x | 8.2x – 25.7x | 564x | 8 |
| Inexact / complex | 13.5x | 7.9x – 17.7x | 280x | 7 |
| Symbolic / list | 17.3x | 3.6x – 60.2x | 645x | 13 |
| `call/cc`, `dynamic-wind` | 22.0x | 11.4x – 59.8x | 141x | 3 |
| Bignums | **52.8x** | 29.6x – 94.3x | 103x | 2 |

**Quote the worst class, not the best.** Against Gambit's interpreter that is bignums at 52.8x; the
eight microbenchmarks, which have no bignum program, would have you believe the figure is 7–14x.

### Two results worth reading closely

**We are faster than both references on strings** — 0.3x of Gambit and 0.5x of Racket on `string`,
which builds a half-megabyte string by repeated `string-append` and `substring`. This is not an
error; it is the direct payoff of representing Scheme strings as JavaScript strings, where V8's
rope representation makes append close to free. It is also the *same* decision that makes
`string-set!` throw. Stage 2b increment 4 proposes a mutable `SchemeString` to close that
conformance gap, and this result says that change has a real cost attached rather than being a
straightforward fix. Measure it before committing.

**Bignums are our worst class by a wide margin**, at 52.8x Gambit and 94x on `pi`. The plan puts the
whole numeric tower at "roughly 3x, not the story" — measured on `fib`, whose values fit in a
machine word. That conclusion does not hold for arbitrary-precision work, and nothing in the
previous suite would have shown it.

---

## What the compiler tier is worth, by workload class

> [!WARNING]
> **These numbers were measured in a configuration that cannot ship.** The harness compiles through
> `tryCompileDefinition`, which carries no continuation guard — that is the per-procedure declining
> R15 proved unsound. Under the sound unit-level guard, **zero of these 41 programs compile
> anything**, because `common.scm` defines `hide` with `call-with-values`. None of the programs
> below capture a continuation, so nothing here was mis-executed, but every figure must be re-taken
> once the guard is both sound and useful. See **R28**.

Measured after the increment-2a boundary fix. The figures before that fix were lower across the
board and are superseded — see the note below.

| Workload class | speedup | range | programs |
|---|---|---|---|
| Procedure call | **6.69x** | 1.02x – 29.78x | 8 |
| Inexact / complex | 1.34x | 0.98x – 6.59x | 7 |
| Symbolic / list | 1.27x | 0.96x – 3.68x | 12 |
| Bignums | 1.11x | 0.99x – 1.24x | 2 |
| `call/cc`, `dynamic-wind` | 1.07x | 0.98x – 1.16x | 3 |
| Strings and characters | 1.05x | 0.99x – 1.13x | 2 |
| Small exact integers | 1.01x | 0.98x – 1.05x | 4 |
| Vectors, bytevectors | **1.01x** | 0.98x – 1.04x | 2 |

**The tier is worth 6.69x on call-heavy code and essentially nothing on everything else.** `tak`
reaches 29.8x and `takl` 23.6x because both are tight recursions over operations the compiler
inlines; `sum`, `nqueens`, `array1` and `sumfp` sit at 1.00x because what dominates them is the
value representation, which code generation does not touch.

The decision rule this supports needs no weighting at all: **ship an optimization when it improves
at least one class and regresses none.** The boundary fix passes it — the call class improved 60%
and no class regressed.

### The boundary fix also raised the numbers, which is worth understanding

Before increment 2a the same table read: call 4.17x, flonum 1.35x, list 1.34x, continuation 1.04x,
fixnum 0.98x, string 0.97x, and every bignum program failed. `fib` went 5.60x → 23.67x and `tak`
9.35x → 29.78x on a change that was made purely for correctness.

Every canonical program passes its input through the interpreted `hide`, so every program's working
value was arriving converted from `BigInt` to a JavaScript number. The inline fast paths are guarded
on `typeof x === 'bigint'`, so a converted input failed that guard *on every operation for the whole
run* and the program fell back to the generic tower primitives throughout. **A correctness defect at
a type boundary was masquerading as a performance ceiling.** Recorded as R32.

## Correctness: one program still fails under the tier

`maze` returns a wrong answer. It is a second, unrelated defect, narrowed but open: the generated
code for `make-maze` is correct on inspection and produces the right answer when its tail-call chain
is driven by hand, but not when the chain is driven by the interpreter. Details and a disproved
hypothesis in **R33**.

The other nine — `pi`, `chudnovsky`, `lattice`, `puzzle`, `destruc`, `earley`, `array1`,
`bv2string`, `string` — were all one root cause, fixed in increment 2a: a value returned from an
interpreted closure into compiled code had JavaScript auto-conversion applied, so exact integers
became inexact and large `BigInt`s threw. Six of the nine had produced a *wrong answer with no
error*.

None of it was detectable by the existing tests or benchmarks. All 2,152 tests passed throughout,
because the cross-tier test cases in `tests/functional/compiler_tests.js` force their callee to stay
interpreted by writing it with `apply` — which trips the unit-level guard and compiles *nothing*, so
they compared the interpreter against itself — and because `render` displays a `BigInt` and a
JavaScript number identically. Both are now fixed: eleven boundary cases compile selectively and ask
Scheme (`exact?`, `eqv?`, `pair?`) about the result.

## Conformance: six programs cannot run at all

| Programs | Gap |
|---|---|
| `gcbench`, `matrix`, `slatex` | identifiers containing `.` are rejected by **extended dot notation**, a deliberate and tested interop feature |
| `parsing`, `read0` | `read-char` / `peek-char` return JavaScript strings, not Scheme characters |
| `equal` | `equal?` does not terminate on circular structure, which R7RS §6.1 requires |

Tracked in [ROADMAP.md](../ROADMAP.md) and recorded as **R25**.

---

## Caveats

- Sizes are reduced from canonical wherever the canonical run is out of reach; the manifest records
  every substitution and its Gambit-derived expected value. Results at canonical sizes would be
  directly comparable to the tables published by `ecraven/r7rs-benchmarks`; these are not.
- `nboyer` and `sboyer` take 23 and 26 seconds per iteration here and dominate the wall-clock cost
  of a run without dominating any reported figure, because everything is per-class and per-iteration.
- Three classes rest on two or three programs each. Treat `vector`, `string` and `bignum` as
  directional until step 3 of the benchmark plan adds more.
