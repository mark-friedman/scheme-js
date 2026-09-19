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

Geometric mean of per-iteration time relative to each reference, interpreter tier, 41 programs.
Gambit `gsi` is an *interpreter*, so it is the fair comparison for ours; Racket CS is a compiler.

| Workload class | vs Gambit `gsi` | range | programs |
|---|---|---|---|
| Strings and characters | **1.5x** | 0.3x – 8.9x | 2 |
| Vectors, bytevectors | 11.1x | 10.9x – 11.3x | 2 |
| Small exact integers | 11.2x | 9.4x – 15.3x | 4 |
| Procedure call | 12.1x | 8.3x – 20.6x | 8 |
| Inexact / complex | 13.1x | 8.0x – 17.7x | 7 |
| Symbolic / list | 14.5x | 3.5x – 34.4x | 13 |
| `call/cc`, `dynamic-wind` | 19.8x | 10.9x – 44.2x | 3 |
| Bignums | **53.2x** | 30.1x – 94.0x | 2 |

**Quote the worst class, not the best.** Against Gambit's interpreter that is bignums at 53.2x.

Making `letrec` a core form (increment 2c′) sped the *interpreter* up on symbolic code as a side
effect — `lattice` 18.7 → 10.7 ms, `graphs` 1.00 s → 662 ms, `earley` 2.10 → 1.59 s — which is the
removed cost of a per-reference scope-registry lookup plus a list allocated and walked to deliver
each lambda.

### Two results worth reading closely

**We are faster than both references on strings** — 0.3x of Gambit on `string`, which builds a
half-megabyte string by repeated `string-append` and `substring`. That is the payoff of representing
Scheme strings as JavaScript strings, where V8's ropes make append close to free. It is also the
*same* decision that makes `string-set!` throw, so the mutable `SchemeString` proposed for increment
4 has a real cost attached — see R31 for the immutable-until-mutated design that keeps it.

**Bignums are the worst class by a wide margin.** The strategy document puts the whole numeric tower
at "roughly 3x, not the story", measured on `fib`, whose values fit in a machine word. That does not
hold for arbitrary-precision work, and the compiler tier does not help either (1.11x).

---

## What the compiler tier is worth, by workload class

> [!WARNING]
> Measured through `tryCompileDefinition`, which carries no continuation guard of its own. The
> harness applies the call-graph guard from `src/compiler/safety.js` over the whole program before
> compiling, so these runs are guarded — but the tier is still **not sound** and stays off by
> default until increment 2b. See R28 and R35.

| Workload class | speedup | range | programs |
|---|---|---|---|
| Small exact integers | **11.95x** | 5.71x – 27.03x | 4 |
| Inexact / complex | 8.94x | 3.34x – 14.44x | 7 |
| Procedure call | 5.55x | 0.98x – 26.29x | 8 |
| Vectors, bytevectors | 4.32x | 0.99x – 18.82x | 2 |
| Symbolic / list | 1.60x | 0.96x – 23.51x | 13 |
| Strings and characters | 1.24x | — | 1 |
| Bignums | 1.11x | 0.99x – 1.23x | 2 |
| `call/cc`, `dynamic-wind` | **1.06x** | 1.00x – 1.19x | 3 |

All 41 programs return the right answer.

### This table replaced a very different one, and the reason matters

Before increment 2c′ the same measurement read: fixnum 1.01x, flonum 1.30x, vector 1.01x, list
1.09x. That supported a confident conclusion (R29) that the tier was "a control-flow optimizer" and
that five of seven classes were not control-flow-bound — a conclusion used to reorder the roadmap.

It was wrong, and wrong for a mundane reason: **named `let`, `do` and internal definitions could not
be compiled at all**, so the hot loop of every fixnum, flonum and vector program was running
interpreted. The tier was not being measured. `sum` went 0.99x → 5.71x and `nqueens` 0.97x → 27.0x
on a change that touched no code generation whatsoever.

What survives is the bignum finding: 1.11x, because BigInt arithmetic genuinely dominates there and
code generation cannot reach it. The generalisation to small-integer code does not survive. See
**R39**.

The decision rule is unchanged and needs no weighting: **ship an optimization when it improves at
least one class and regresses none.**

### Coverage

Of 754 definitions in the suite: **573 can be lowered** (the `unsupported node` category is gone
entirely, R38) and **358 survive the safety guard**. What stops the rest is now the guard and
`define` of non-procedures, not the compiler's front end.

## Correctness: the suite is clean

No wrong answers and no errors under either tier. Getting here took two fixes:

- **Increment 2a** — values crossing from interpreted code into compiled code no longer have
  JavaScript auto-conversion applied. Nine programs recovered, six of which had been returning a
  *wrong answer with no error*.
- **Increment 2b′** — the continuation guard is now a call-graph closure rather than a per-procedure
  name check, which fixed `maze` (R34) and catches the `btsearch` shape as well.

Neither was detectable by the tests as they stood; both now have regression coverage.

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
