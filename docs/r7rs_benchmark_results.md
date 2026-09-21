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

### Bignums are not a coverage problem

`pi` compiled **0 of 9** definitions for most of this work, all of them blocked by `values`, which
made coverage the obvious explanation for the class sitting at 1.2x — and coverage had already been
the answer three times running. It compiles **9 of 9** now and measures **1.00x**. So the class
really is bound by BigInt arithmetic, code generation cannot reach it, and the remaining gap against
Gambit is worth profiling in the numeric tower rather than the compiler.

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

Re-measured 2026-09-21, with the standard library compiled ahead of time and `apply` supported.
A compiled procedure can now take part in a captured continuation, so the guard that holds some
definitions back is a speed heuristic rather than a soundness requirement.

| Workload class | speedup | before this run of work | programs |
|---|---|---|---|
| Procedure call | **21.79x** | 5.70x | 8 |
| Small exact integers | **16.18x** | 10.52x | 4 |
| Vectors, bytevectors | **15.19x** | 17.12x | 2 |
| Inexact / complex | **10.18x** | 2.95x | 7 |
| Symbolic / list | **10.14x** | 1.92x | 13 |
| `call/cc`, `dynamic-wind` | **3.18x** | 1.00x | 3 |
| Strings and characters | 1.22x | 1.19x | 2 |
| Bignums | 1.22x | 1.07x | 2 |

**Every program returns the right answer**, `read1` included — it used to fail under the tier
because `call-with-input-file` closed the port before a compiled thunk's pending tail call ran.

**Not one of these gains came from changing how code is generated.** All of it was coverage: the
standard library was interpreted underneath compiled code (**R46**), `apply` was wrongly treated as
a control operation and blocked `map` and `for-each` (**R46**), `let` expanded into nested
procedures that could not be emitted (**R47**), and `call-with-values` blocked the shared `hide`
idiom that all 51 programs use (**R48**).

**839 of 1089 definitions compile.** What remains is 229 top-level definitions that are not
procedures and 21 reaching `call/cc`.



### The library underneath was most of what the earlier figures measured

`list`, `call` and `continuation` are the three classes that spend their time inside the standard
library, and they are the three that moved: 2.4x, 2.2x and 2.9x. The numeric classes, which do not,
did not move at all. The library is itself Scheme, so until it was compiled, compiled code crossed
into the interpreter on its hottest path — and that boundary, not code generation, was what the
symbolic figures were reporting. `peval` went 1.21x → 6.96x and `scheme` 1.30x → 6.54x without any
change to how code is generated. See **R45** and **R46**.

### The regressions the library introduced, and how they went away

Compiling the library on its own made four programs *slower* — `earley` 1.00x → 0.87x, `sum` 5.62x
→ 4.05x, `sumfp` 3.35x → 2.17x, `takl` 25.66x → 20.93x — because the tier boundary costs the same
in both directions, and a partly-compiled program's interpreted procedures then called compiled
library code and paid the crossing the other way.

Raising coverage was the right response rather than reverting, and it worked: `earley` is now
**21.41x** with 6 of 8 definitions compiled, up from 4 of 8. `paraffins` 1.38x → 23.33x, `fft`
1.00x → 12.21x, `mbrotZ` 1.05x → 13.10x, `simplex` 1.14x → 13.59x.

Four programs are 4–11% lower than their best recorded figure (`fibfp`, `tak`, `array1`, `ack`).
Re-measured at a five-times-longer target they are unchanged, so that is run-to-run variance on
short benchmarks rather than a regression.

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

**827 of 1089 definitions across the corpus are compiled**, up from 623 before `apply` was
supported and 807 before binding chains could be emitted. A histogram of why the rest are
declined — the measurement that found both problems — now reads:

| count | reason |
|---|---|
| 229 | the definition is not a procedure at all |
| 21 | reaches `call/cc` |

Those 21 are the entire remaining real decline list — ctak(3), fibc(2), maze(5), puzzle(3),
read0(7). The library itself is **61 of 61**.

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
