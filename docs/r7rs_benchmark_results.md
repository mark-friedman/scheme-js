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

## What is run

Every program is assembled the way upstream assembles it -- the program, then `common.scm`, then a
call to `(run-benchmark)` -- and timed **inside the program** by `run-r7rs-benchmark` in
`common.scm`, using R7RS `current-jiffy` around the repetitions. Nothing outside that loop is
timed: not starting the process, not loading the library, not compiling. Each measurement is a
fresh process, the repetition count is calibrated per implementation until a run takes at least
half a second, the first run is thrown away, and times are reported **per iteration**. Sizes are
reduced from canonical where the canonical run is out of reach; the manifest records each one, with
an expected value derived from Gambit.

scheme-js-4 runs in two configurations, set up by `benchmarks/lib/r7rs_harness.js`. The programs'
own `(import ...)` is removed and a small harness prelude supplies `read` from the input text;
that prelude is always interpreted.

| | **Interpreter tier** | **Compiled tier** |
|---|---|---|
| Standard library | Its Scheme sources, **interpreted** | The same sources, every procedure then **compiled to JavaScript** at start-up |
| The program's procedure `define`s | Interpreted | Each defined by the interpreter, then **compiled from the closure** that made (`tryCompileClosure`) -- including `common.scm`'s `hide` and the timing loop `run-r7rs-benchmark` |
| Top-level expressions, and `define`s of values | Interpreted | **Compiled as a thunk** and called once (`tryCompileExpression`) -- which is where `nboyer` and `sboyer` make their procedures |

Procedures that capture continuations are compiled too; `run_r7rs.js --decline-captures` restores
the old rule that declined them and whatever reaches them. The harness compiles everything before
the clock starts, where the tier a user gets compiles the same code by its own policy
(`src/compiler/tier.scm`) as the program runs.

The references run upstream's own preludes, unmodified, on the program with its `(import ...)`
intact. Both Gambit compilers build each program once, with upstream's `(declare
(standard-bindings) (extended-bindings) (block))`, in safe mode, and the build is run at every
calibrated count; Racket compiles on load, before the program starts its clock.

| Reference | What it is |
|---|---|
| Gambit `gsi` | Gambit's interpreter |
| Gambit compiled to JavaScript | `gsc -target js -exe`, run by the same Node as us -- the like-for-like reference for our compiler |
| Gambit compiled to C | `gsc -exe`, native code; needs the C compiler Gambit was configured with (`benchmarks/r7rs/README.md`) |
| Racket CS | Racket on Chez Scheme's native compiler |
| plain JavaScript | What a JavaScript programmer would write, for seven programs (`benchmarks/r7rs/plain_js_kernels.js`) |

---

## The compiled tier against other implementations

2026-09-27, darwin/arm64, Node v24.11.1, after task 33. Both our tiers -- the interpreter, and the
compiled tier as a user would run it, the standard library and the program's definitions compiled --
against Gambit's interpreter, Racket CS, Gambit compiled to JavaScript (`gsc -target js`, run by
the same Node), and, for seven programs, plain JavaScript (`benchmarks/r7rs/plain_js_kernels.js`).
The manifest's default sizes throughout, the same for every implementation, so per-iteration times
compare; canonical sizes would take hours and are what published results use. Gambit compiled to C
is not in this table: its build failed here, which is not the machine lacking a toolchain but
Homebrew's Gambit naming a C compiler that is not installed -- see *Gambit compiled to C*, below.
Reproduce with `node benchmarks/compare_r7rs.js`, or `--ours` with a saved `run_r7rs.js` run to
measure only the references.

Our time over theirs, per class, geometric mean; below 1 we are faster:

| class | vs Gambit compiled to JavaScript | vs Racket CS | vs Gambit's interpreter | vs plain JavaScript |
|---|---|---|---|---|
| call | 0.89x (0.42-1.53) | 6.4x | 0.14x | 2.5x (fib, tak, ack) |
| fixnum | 1.10x (0.41-2.23) | 10.3x | 0.19x | 6.3x (sum, nqueens) |
| flonum | 0.27x (0.05-1.06) | 2.2x | 0.11x | 2.6x (fibfp, sumfp) |
| list | 0.60x (was 2.04x) | 7.4x (was 20.8x) | 0.20x | -- |
| vector | 1.20x | 6.4x | 0.26x | -- |
| string | 0.04x | 0.46x | 1.48x | -- |
| bignum | 1.65x | 87x | 45x | -- |
| continuation | 2.59x | 24.7x | 3.9x | -- |

- **Against the other Scheme compiled to JavaScript** we are level on calls and fixnums, 4x faster on
  flonums (unboxed JavaScript numbers where Gambit boxes), 25x faster on strings (ours are JavaScript
  strings), and behind on lists, bignums and continuations. Gambit's calling convention is the one
  the stage 2a bake-off rejected, an explicit frame stack: its captures are cheaper, as expected.
- **The founding 650x gap to plain JavaScript** on `fib(30)` is 2.9x on `fib` now, and 2.0-2.9x on
  the call kernels. On fixnum loops it is 3.4-12x: exact integers are `BigInt`, which the plain
  versions do not pay for.
- **`nboyer` and `sboyer` were the outliers**: 16 and 20 s compiled, barely faster than the
  interpreter's 21 and 24 s, and 40x and 53x behind Gambit compiled to JavaScript -- most of the list
  class's gap. They had never been compiled: every procedure in them is assigned from inside one
  top-level `let`, and the tier compiled only top-level definitions (R84). With top-level
  expressions compiled (task 35) they take 0.20 s each, ahead of Gambit's 0.41 and 0.37 s; `scheme`
  and `lattice` went 7-8x faster for the same reason; the list row above is re-measured with them.
  **`pi` and `chudnovsky`** are no faster compiled than interpreted, as known: bignum arithmetic.
- **`ctak`** is slower compiled than interpreted (167 against 134 ms): its procedures capture, so
  they are declined, and the capture unwinds through the compiled ones between.
- Gambit compiled to JavaScript failed on `quicksort` and `graphs`, both Gambit's own failures:
  `quicksort` exhausts the JavaScript stack on the canonical 10,000-element input, and `graphs`
  defines its own three-argument `fold`, which the JavaScript target replaces with Gambit's built-in
  SRFI-1 `fold` ("(Argument 1, kons) PROCEDURE expected"). Gambit compiled to C runs both.

### Gambit compiled to C

It builds, with three workarounds recorded in `benchmarks/r7rs/README.md`: a link for the versioned
GCC Homebrew's Gambit names, `DEVELOPER_DIR` where the selected Xcode is broken, and a define that
lets code from a newer GCC load into the older runtime, which the harness now always passes. Its
column is missing from the table above only because the table was taken before it built.

The references' times, measured 2026-09-26, are below. They do not depend on our code, so they can
be set against any later `run_r7rs.js` run; only ratios were kept before, and ratios cannot be
recombined. **Taken on a loaded machine** -- another benchmark run and an IDE were using about two
of eight performance cores -- and one run each: two runs of `gsc` on `fib` gave 354 and 820 µs, so
read single programs as ±2x.

| Program | Class | Gambit `gsi` | Gambit to C | Gambit to JS | Racket CS |
|---|---|---|---|---|---|
| `fib` | call | 22.3 ms | 819.7 µs | 3.6 ms | 1.2 ms |
| `tak` | call | 14.0 ms | 120.3 µs | 664.5 µs | 120.7 µs |
| `takl` | call | 57.1 ms | 442.7 µs | 7.9 ms | 792.0 µs |
| `ack` | call | 18.7 ms | 253.7 µs | 2.0 ms | 348.8 µs |
| `cpstak` | call | 11.5 ms | 266.1 µs | 3.7 ms | 225.0 µs |
| `deriv` | call | 2.9 µs | 0.1 µs | 1.0 µs | 0.1 µs |
| `divrec` | call | 49.8 µs | 1.9 µs | 8.5 µs | 1.9 µs |
| `diviter` | call | 52.5 µs | 1.7 µs | 6.4 µs | 1.4 µs |
| `sum` | fixnum | 1.0 ms | 14.3 µs | 46.0 µs | 13.3 µs |
| `primes` | fixnum | 3.1 ms | 62.7 µs | 586.6 µs | 76.9 µs |
| `nqueens` | fixnum | 28.4 ms | 594.2 µs | 5.4 ms | 517.0 µs |
| `puzzle` | fixnum | 117.6 ms | 3.3 ms | 50.1 ms | 2.2 ms |
| `pi` | bignum | 6.3 ms | 6.1 ms | 196.5 ms | 4.7 ms |
| `chudnovsky` | bignum | 504.2 µs | 249.0 µs | 8.7 ms | 184.3 µs |
| `fibfp` | flonum | 23.3 ms | 877.7 µs | 9.9 ms | 2.0 ms |
| `sumfp` | flonum | 106.9 ms | 4.9 ms | 50.7 ms | 6.3 ms |
| `mbrot` | flonum | 71.4 ms | 3.3 ms | 24.8 ms | 4.7 ms |
| `mbrotZ` | flonum | 77.1 ms | 8.4 ms | 117.9 ms | 3.9 ms |
| `fft` | flonum | 65.8 ms | 3.2 ms | 16.4 ms | 3.0 ms |
| `simplex` | flonum | 91.7 µs | 1.4 µs | 20.2 µs | 1.8 µs |
| `pnpoly` | flonum | 82.0 µs | 2.6 µs | 16.5 µs | 4.2 µs |
| `browse` | list | 29.1 ms | 519.7 µs | 6.4 ms | 1.0 ms |
| `destruc` | list | 18.2 ms | 433.2 µs | 4.4 ms | 509.0 µs |
| `peval` | list | 34.8 ms | 765.3 µs | 12.2 ms | 882.0 µs |
| `scheme` | list | 624.4 µs | 30.5 µs | 433.0 µs | 27.8 µs |
| `maze` | list | 4.6 ms | 44.8 µs | 610.2 µs | 73.7 µs |
| `mazefun` | list | 8.3 ms | 173.8 µs | 2.2 ms | 207.9 µs |
| `quicksort` | list | 44.9 ms | 1.3 ms | fails | 3.4 ms |
| `earley` | list | 138.9 ms | 4.0 ms | 32.1 ms | 3.1 ms |
| `graphs` | list | 37.7 ms | 784.7 µs | fails | 570.0 µs |
| `lattice` | list | 320.8 µs | 9.9 µs | 128.9 µs | 7.7 µs |
| `nboyer` | list | 1.85 s | 59.6 ms | 427.0 ms | 70.8 ms |
| `sboyer` | list | 2.24 s | 38.5 ms | 396.5 ms | 38.8 ms |
| `paraffins` | list | 9.0 ms | 692.5 µs | 2.3 ms | 284.5 µs |
| `array1` | vector | 25.8 ms | 529.4 µs | 1.9 ms | 530.0 µs |
| `bv2string` | vector | 16.4 ms | 963.2 µs | 10.2 ms | 1.4 ms |
| `string` | string | 13.9 ms | 4.9 ms | 591.0 ms | 12.8 ms |
| `read1` | string | 438.7 µs | 524.0 µs | 12.6 ms | 5.7 ms |
| `ctak` | continuation | 14.4 ms | 2.8 ms | 27.8 ms | 2.0 ms |
| `fibc` | continuation | 11.1 ms | 1.1 ms | 11.7 ms | 912.0 µs |
| `dynamic` | continuation | 54.9 ms | 7.3 ms | 84.1 ms | 21.2 ms |

Gambit compiled to C is up to 100x faster than its interpreter (`maze`), and least where the work
is in the runtime rather than the program, since the interpreter calls the same compiled runtime:
level on `pi` and `read1`, 2-3x on `chudnovsky` and `string`. Gambit's JavaScript backend loses that runtime: its
bignums are 14-bit digits in JavaScript (`##bignum.adigit-width` is 14 in `_gambit.js`), 32x
slower than its C build on `pi`, and its mutable strings make `string` 591 ms.

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

> **Corrected 2026-10-05 (task 42, R119):** profiled, it was neither BigInt arithmetic nor the
> tower. `pi` and `chudnovsky` spent 98% of their time in `exact-integer-sqrt`, whose Newton
> iteration started from the integer itself. With a root that doubles its precision each step, `pi`
> takes 2.0 ms compiled (560 ms before; Gambit's interpreter 6.3 ms, Racket CS 4.7 ms) and
> `chudnovsky` 0.15 ms (10 ms before; 0.50 and 0.18 ms), and the class is 7x faster compiled than
> interpreted. The tables on this page were measured before.

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

**2026-09-26, commit `803ed49`** -- after tasks 18-27 in `compiler_plan.md`, before 28-36, and so
before top-level expressions were compiled; on the same loaded machine as the reference times
above. Compiled tier over interpreter tier, same run:

| Workload class | speedup | 2026-09-21, below | programs |
|---|---|---|---|
| Inexact / complex | **126.48x** | 10.18x | 7 |
| Procedure call | **86.70x** | 21.79x | 8 |
| Small exact integers | **58.47x** | 16.18x | 4 |
| Vectors, bytevectors | **39.08x** | 15.19x | 2 |
| Symbolic / list | **24.46x** | 10.14x | 13 |
| `call/cc`, `dynamic-wind` | **4.32x** | 3.18x | 3 |
| Bignums | 1.23x | 1.22x | 2 |
| Strings and characters | 1.04x | 1.22x | 2 |

Loop contification, primitive guards that survive a call, cell-based global reads, flonum fast
paths, inline vector access, direct tail calls and deep recursion on the heap, measured together.
`nboyer` and `sboyer` gained only 1.2x here -- the same thing task 36 found, and compiling top-level
expressions fixed (above).

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
