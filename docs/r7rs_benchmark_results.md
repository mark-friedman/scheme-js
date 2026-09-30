# Canonical R7RS benchmark results

The vendored Gabriel/Gambit/Larceny suite: 41 programs that run, classified by workload.
Methodology, provenance and sizing are in [benchmarks/r7rs/README.md](../benchmarks/r7rs/README.md).
First run 2026-09-18; the figures below are from **2026-09-26**, commit `803ed49`, darwin/arm64,
Node v24.11.1, Gambit v4.9.5, Racket v8.6 CS.

Reproduce with:

```bash
npm run benchmark:r7rs > ours.log
```

```bash
npm run benchmark:r7rs-implementations -- --ours ours.log
```

The second command measures only the reference implementations and takes our figures from the
first; without `--ours` it measures both of our tiers again. Gambit's C compiler may need a
link to the C compiler it was configured with -- see the README.

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
half a second, the first (uncalibrated) run is thrown away, and times are reported **per
iteration**. Sizes are reduced from canonical where the canonical run is out of reach; the manifest
records each one, with an expected value derived from Gambit.

scheme-js-4 is run in two configurations by `benchmarks/lib/r7rs_harness.js`. The programs' own
`(import ...)` is removed and a small harness prelude supplies `read` from the input text; that
prelude is always interpreted.

| | **Interpreter tier** | **Compiled tier** |
|---|---|---|
| Standard library | The Scheme sources (`macros`, `equality`, `cxr`, `numbers`, `list`, `control`), **interpreted** | The same sources, then every library procedure **compiled to JavaScript** in place at start-up (`compileEnvironment`) -- the code generator the shipped prebuilt tables come from |
| The program's `define`s | Interpreted | Each **compiled to JavaScript as it appears** (`tryCompileDefinition`), unless the call-graph guard (`safety.js`) declines it because a `call/cc` capture could happen inside it; declined definitions run interpreted. Includes `common.scm`'s `hide` and the timing loop `run-r7rs-benchmark` |
| Top-level expressions | Interpreted | Interpreted -- only the final `(run-benchmark)` call, which enters compiled code at once |

The "compiled defs" column below counts the program's definitions the compiler accepted
(611 of 754 across the suite). Compilation happens before the timed loop and is not charged.

**Neither is what a user runs today.** A page or REPL installs the compiled standard library from
prebuilt tables, but user code is never compiled -- the tier is not yet enabled for it
(`compiler_plan.md`, *Enable the tier for user code*). So a user's program runs like the
interpreter tier over a compiled library, a configuration this suite does not measure.

The references run upstream's own preludes, unmodified, on the program with its `(import ...)`
intact:

| Reference | What it is | How it is run |
|---|---|---|
| Gambit `gsi` | Gambit's interpreter | `gsi program.scm` |
| Gambit `gsc` (C) | Gambit's compiler, to C, then native code | `gsc -exe`, once per program; the executable is timed |
| Gambit `gsc` (JS) | Gambit's compiler, to JavaScript | `gsc -target js -exe`, once per program; the `.js` is run by the same Node as us |
| Racket CS | Racket on Chez Scheme's native compiler | `racket program.rkt` (compiled on load, before the clock starts) |

Both Gambit compilers get upstream's `(declare (standard-bindings) (extended-bindings) (block))`
and stay in safe mode. **Gambit's JavaScript backend is the like-for-like comparison for our
compiler**: Scheme compiled to JavaScript, running on the same engine. Gambit 4.9.5 has no
WebAssembly target.

---

## Standing against other implementations

Geometric mean over each class of our per-iteration time divided by the reference's. "Faster" and
"slower" describe scheme-js-4.

### The compiled tier

| Workload class | vs Gambit `gsi` | vs Gambit `gsc` (JS) | vs Gambit `gsc` (C) | vs Racket CS | programs |
|---|---|---|---|---|---|
| Inexact / complex | 9.9x faster | **3.8x faster** | 2.4x slower | 2.0x slower | 7 |
| Procedure call | 8.1x faster | **1.2x faster** | 5.8x slower | 5.1x slower | 8 |
| Small exact integers | 5.4x faster | 1.1x slower | 9.2x slower | 10.1x slower | 4 |
| Vectors, bytevectors | 3.8x faster | 1.2x slower | 7.6x slower | 6.4x slower | 2 |
| Symbolic / list | 1.8x faster | 2.0x slower ¹ | 21.8x slower | 20.2x slower | 13 |
| Strings and characters | 1.5x slower | **23.1x faster** | 2.3x slower | 2.3x faster | 2 |
| `call/cc`, `dynamic-wind` | 3.9x slower | 2.7x slower | 29.2x slower | 24.1x slower | 3 |
| Bignums | **42.8x slower** | 1.9x slower | 62.2x slower | 81.9x slower | 2 |

¹ Over 11 programs: Gambit's JavaScript backend fails on `quicksort` (it exhausts the JavaScript
stack on the canonical 10,000-element input) and on `graphs` (the program defines its own
three-argument `fold`, and the JavaScript target calls Gambit's built-in SRFI-1 `fold` instead; the
C target gets it right).

**Against Gambit's JavaScript backend -- same engine, same target -- the compiled tier is ahead in
three classes, within 1.25x in two more, and within 2x in all but `call/cc`.** Flonums lead by 3.8x
(`mbrot` 18x, `sumfp` 11x), where two flonum operands compile to one JavaScript operator; strings
lead by 23x, because Scheme strings are JavaScript strings. Bignums are 1.9x behind Gambit's
JavaScript backend against 62x behind Gambit compiled to C, which suggests most of that class's gap
is what arbitrary precision costs on a JavaScript host rather than something peculiar to our tower.

**The list-class gap is two programs.** `nboyer` and `sboyer` -- both the Boyer theorem prover's
term rewriter -- run 43x and 53x slower than Gambit's JavaScript backend and only 1.2x faster than
our own interpreter; the other nine list programs together are at parity with it (0.97x). They are
the first thing to profile in this class.

**Against native compilers the gap is 2-10x on numeric and call-bound code and about 20x on
symbolic code**, and against both of them `call/cc` (24-29x) and bignums (62-82x) are the worst
classes. Strings stay ahead of Racket and within 2.3x of Gambit compiled to C.

### The interpreter tier

| Workload class | vs Gambit `gsi` | vs Gambit `gsc` (JS) | vs Gambit `gsc` (C) | vs Racket CS | programs |
|---|---|---|---|---|---|
| Strings and characters | 1.6x slower | 22.1x faster | 2.4x slower | 2.2x faster | 2 |
| Vectors, bytevectors | 10.3x slower | 48.0x slower | 296x slower | 249x slower | 2 |
| Procedure call | 10.7x slower | 71.1x slower | 499x slower | 442x slower | 8 |
| Small exact integers | 10.8x slower | 66.8x slower | 538x slower | 593x slower | 4 |
| Inexact / complex | 12.8x slower | 33.5x slower | 310x slower | 258x slower | 7 |
| Symbolic / list | 13.7x slower | 51.5x slower ¹ | 533x slower | 494x slower | 13 |
| `call/cc`, `dynamic-wind` | 17.0x slower | 11.7x slower | 126x slower | 104x slower | 3 |
| Bignums | **52.6x slower** | 2.3x slower | 76x slower | 101x slower | 2 |

Against `gsi`, the fair comparison for an interpreter, every class is within 15% of what it was on
2026-09-19 (then: call 12.1x, list 14.5x, `call/cc` 19.8x, bignums 53.2x) -- the interpreter has
barely moved while the compiler work went on, as intended.

### Bignums are not a coverage problem

`pi` compiled **0 of 9** definitions for most of this work, all of them blocked by `values`, which
made coverage the obvious explanation for the class sitting at 1.2x — and coverage had already been
the answer three times running. It compiles **9 of 9** now and measures **1.02x**. So the class
really is bound by BigInt arithmetic, code generation cannot reach it, and the remaining gap is
worth profiling in the numeric tower rather than the compiler. Gambit's JavaScript backend puts that
gap in proportion: it is 32x slower on `pi` than Gambit compiled to C, and we are 2.9x behind it.

### Strings: ahead of every reference on `string`

`string` builds a half-megabyte string by repeated `string-append` and `substring`. The compiled
tier runs it in 3.5 ms, against 4.9 ms for Gambit compiled to C, 12.8 ms for Racket and 591 ms for
Gambit's JavaScript backend. That is the payoff of representing Scheme strings as JavaScript
strings, where V8's ropes make append close to free. It is also the *same* decision that makes
`string-set!` throw, so the mutable `SchemeString` in the plan has a real cost attached — see R31
for the immutable-until-mutated design that keeps it. `read1`, the other program in the class, is
parsing and 7-9x behind `gsi` and `gsc`.

---

## What the compiler tier is worth, by workload class

Compiled tier against the interpreter tier, same run. Re-measured 2026-09-26, after tasks 18-27 in
[compiler_plan.md](compiler_plan.md): loop contification, primitive guards that survive a call,
the emitter rewritten as Scheme passes over the IR, the compiler as its own library, cell-based
global reads, `case` and direct-call code generation, flonum fast paths, inline vector access,
direct tail calls between procedures, and deep recursion spilling to the heap.

| Workload class | speedup | 2026-09-21 | programs |
|---|---|---|---|
| Inexact / complex | **126.48x** | 10.18x | 7 |
| Procedure call | **86.70x** | 21.79x | 8 |
| Small exact integers | **58.47x** | 16.18x | 4 |
| Vectors, bytevectors | **39.08x** | 15.19x | 2 |
| Symbolic / list | **24.46x** | 10.14x | 13 |
| `call/cc`, `dynamic-wind` | **4.32x** | 3.18x | 3 |
| Bignums | 1.23x | 1.22x | 2 |
| Strings and characters | 1.04x | 1.22x | 2 |

**Every program returns the right answer in both tiers.** 611 of the suite's 754 definitions
compile.

**Bignums held flat**, as expected: BigInt arithmetic, not code generation, is the ceiling there.
**Strings read lower than on 2026-09-21**, 1.04x against 1.22x, but both programs sit at parity
(`string` 1.11x, `read1` 0.98x) and differ by a fraction of a millisecond, on a machine that was
loaded during this run -- not attributable to any change without a quieter re-run.

**`ctak` runs slower compiled than interpreted**, 0.78x, with 2 of 5 definitions compiled: it is
`call/cc`-bound and the guard declines most of it, so what compiles probably pays the tier boundary
on every continuation -- unprofiled. The class figure is carried by `dynamic` (41.3x, 159 of 232 compiled); `fibc` is 2.51x.

**`nboyer` and `sboyer` gain 1.2x** (5 of 7 compiled each) where the rest of the list class gains
10-210x -- the same two programs that account for the list class's gap to Gambit, above.

### Construct-level benchmarks (`npm run benchmark:codegen`)

The canonical suite decides whether a change ships, but it is blind to a construct its 41 programs
do not exercise hot. Each code-generation task since 23 also gets a workload timing that construct
directly, in both tiers, best of five runs, nanoseconds per call with an empty-loop baseline
subtracted:

| Construct (compiled tier) | ns/call | vs interpreted |
|---|---|---|
| `case`, symbol key, 2 clauses | 1.1 | 900.9 ns (818x) |
| `case`, symbol key, 8 clauses | 5.9 | 2048.8 ns (347x) |
| exact-integer `+`/`=` | 5.1 | 488.5 ns (96x) |
| flonum `+`/`=` | 40.8 | 506.9 ns (12x) |
| `vector-ref` | 9.9 | 357.5 ns (36x) |
| sum of 8 vector elements (a loop of refs) | 146.7 | 12,415.5 ns (85x) |
| one direct tail call | 2.7 | 129.8 ns (48x) |
| mutual recursion, 10 tail calls | 55.9 | 7,072.6 ns (127x) |
| non-tail recursion, 100 levels deep | 1,742.7 | 76,364.9 ns (44x) |
| recursion 100,000 deep (moves to the heap) | 10.56 ms | 102.25 ms (10x) |

The last row matches task 27's figure at landing (10 ms against 96 ms): deep recursion has not
regressed. Mixed-type arithmetic (an exact integer and a flonum, 92.2 ns; a rational and a flonum,
103.4 ns) still takes the numeric tower, as designed.

```bash
npm run benchmark:codegen
```

---

## Per-program times, 2026-09-26

Seconds per iteration, the figures every ratio above is computed from. Kept so that a later run of
one side can be compared with this one without measuring the other again -- only ratios were
recorded before, and they cannot be recombined.

| Program | Class | Compiled defs | Interpreter | Compiled | Gambit `gsi` | Gambit `gsc` (C) | Gambit `gsc` (JS) | Racket CS |
|---|---|---|---|---|---|---|---|---|
| `fib` | call | 4/4 | 187.8 ms | 2.8 ms | 22.3 ms | 819.7 µs | 3.6 ms | 1.2 ms |
| `tak` | call | 4/4 | 59.9 ms | 690.1 µs | 14.0 ms | 120.3 µs | 664.5 µs | 120.7 µs |
| `takl` | call | 6/9 | 910.0 ms | 3.3 ms | 57.1 ms | 442.7 µs | 7.9 ms | 792.0 µs |
| `ack` | call | 4/4 | 159.0 ms | 2.2 ms | 18.7 ms | 253.7 µs | 2.0 ms | 348.8 µs |
| `cpstak` | call | 4/4 | 104.3 ms | 2.9 ms | 11.5 ms | 266.1 µs | 3.7 ms | 225.0 µs |
| `deriv` | call | 4/4 | 54.7 µs | 0.9 µs | 2.9 µs | 0.1 µs | 1.0 µs | 0.1 µs |
| `divrec` | call | 5/5 | 665.3 µs | 9.5 µs | 49.8 µs | 1.9 µs | 8.5 µs | 1.9 µs |
| `diviter` | call | 5/5 | 808.4 µs | 4.6 µs | 52.5 µs | 1.7 µs | 6.4 µs | 1.4 µs |
| `sum` | fixnum | 4/4 | 9.6 ms | 71.2 µs | 1.0 ms | 14.3 µs | 46.0 µs | 13.3 µs |
| `primes` | fixnum | 6/6 | 31.7 ms | 645.1 µs | 3.1 ms | 62.7 µs | 586.6 µs | 76.9 µs |
| `nqueens` | fixnum | 4/5 | 282.0 ms | 2.7 ms | 28.4 ms | 594.2 µs | 5.4 ms | 517.0 µs |
| `puzzle` | fixnum | 7/21 | 1.69 s | 101.3 ms | 117.6 ms | 3.3 ms | 50.1 ms | 2.2 ms |
| `pi` | bignum | 9/9 | 575.0 ms | 566.0 ms | 6.3 ms | 6.1 ms | 196.5 ms | 4.7 ms |
| `chudnovsky` | bignum | 7/12 | 15.3 ms | 10.3 ms | 504.2 µs | 249.0 µs | 8.7 ms | 184.3 µs |
| `fibfp` | flonum | 4/4 | 189.2 ms | 1.4 ms | 23.3 ms | 877.7 µs | 9.9 ms | 2.0 ms |
| `sumfp` | flonum | 4/4 | 959.0 ms | 4.4 ms | 106.9 ms | 4.9 ms | 50.7 ms | 6.3 ms |
| `mbrot` | flonum | 6/6 | 960.0 ms | 1.3 ms | 71.4 ms | 3.3 ms | 24.8 ms | 4.7 ms |
| `mbrotZ` | flonum | 6/6 | 1.29 s | 33.5 ms | 77.1 ms | 8.4 ms | 117.9 ms | 3.9 ms |
| `fft` | flonum | 5/6 | 1.05 s | 10.0 ms | 65.8 ms | 3.2 ms | 16.4 ms | 3.0 ms |
| `simplex` | flonum | 10/10 | 1.2 ms | 19.5 µs | 91.7 µs | 1.4 µs | 20.2 µs | 1.8 µs |
| `pnpoly` | flonum | 5/5 | 1.3 ms | 13.4 µs | 82.0 µs | 2.6 µs | 16.5 µs | 4.2 µs |
| `browse` | list | 15/19 | 104.0 ms | 495.1 µs | 29.1 ms | 519.7 µs | 6.4 ms | 1.0 ms |
| `destruc` | list | 5/5 | 439.5 ms | 4.6 ms | 18.2 ms | 433.2 µs | 4.4 ms | 509.0 µs |
| `peval` | list | 32/43 | 819.0 ms | 11.4 ms | 34.8 ms | 765.3 µs | 12.2 ms | 882.0 µs |
| `scheme` | list | 109/112 | 12.7 ms | 1.3 ms | 624.4 µs | 30.5 µs | 433.0 µs | 27.8 µs |
| `maze` | list | 60/69 | 54.3 ms | 2.9 ms | 4.6 ms | 44.8 µs | 610.2 µs | 73.7 µs |
| `mazefun` | list | 27/28 | 103.5 ms | 1.1 ms | 8.3 ms | 173.8 µs | 2.2 ms | 207.9 µs |
| `quicksort` | list | 5/9 | 665.0 ms | 183.2 ms | 44.9 ms | 1.3 ms | fails | 3.4 ms |
| `earley` | list | 8/8 | 1.62 s | 25.5 ms | 138.9 ms | 4.0 ms | 32.1 ms | 3.1 ms |
| `graphs` | list | 18/18 | 525.0 ms | 6.3 ms | 37.7 ms | 784.7 µs | fails | 570.0 µs |
| `lattice` | list | 14/17 | 6.6 ms | 435.0 µs | 320.8 µs | 9.9 µs | 128.9 µs | 7.7 µs |
| `nboyer` | list | 5/7 | 22.68 s | 18.48 s | 1.85 s | 59.6 ms | 427.0 ms | 70.8 ms |
| `sboyer` | list | 5/7 | 25.45 s | 21.11 s | 2.24 s | 38.5 ms | 396.5 ms | 38.8 ms |
| `paraffins` | list | 7/7 | 127.4 ms | 1.2 ms | 9.0 ms | 692.5 µs | 2.3 ms | 284.5 µs |
| `array1` | vector | 7/7 | 249.8 ms | 5.0 ms | 25.8 ms | 529.4 µs | 1.9 ms | 530.0 µs |
| `bv2string` | vector | 5/6 | 179.4 ms | 5.9 ms | 16.4 ms | 963.2 µs | 10.2 ms | 1.4 ms |
| `string` | string | 6/7 | 3.9 ms | 3.5 ms | 13.9 ms | 4.9 ms | 591.0 ms | 12.8 ms |
| `read1` | string | 4/4 | 3.9 ms | 3.9 ms | 438.7 µs | 524.0 µs | 12.6 ms | 5.7 ms |
| `ctak` | continuation | 2/5 | 139.5 ms | 179.2 ms | 14.4 ms | 2.8 ms | 27.8 ms | 2.0 ms |
| `fibc` | continuation | 5/7 | 170.6 ms | 67.8 ms | 11.1 ms | 1.1 ms | 11.7 ms | 912.0 µs |
| `dynamic` | continuation | 159/232 | 1.81 s | 43.9 ms | 54.9 ms | 7.3 ms | 84.1 ms | 21.2 ms |

---

## Earlier measurements

Kept for the reasoning they record. The tables they refer to are superseded by the ones above.

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

Raising coverage was the right response rather than reverting, and it worked: `earley` reached
**21.41x** with 6 of 8 definitions compiled, up from 4 of 8. `paraffins` 1.38x → 23.33x, `fft`
1.00x → 12.21x, `mbrotZ` 1.05x → 13.10x, `simplex` 1.14x → 13.59x.

### A tier table that was wrong, and why

Before increment 2c′ the tier measurement read: fixnum 1.01x, flonum 1.30x, vector 1.01x, list
1.09x. That supported a confident conclusion (R29) that the tier was "a control-flow optimizer" and
that five of seven classes were not control-flow-bound — a conclusion used to reorder the roadmap.

It was wrong, and wrong for a mundane reason: **named `let`, `do` and internal definitions could not
be compiled at all**, so the hot loop of every fixnum, flonum and vector program was running
interpreted. The tier was not being measured. `sum` went 0.99x → 5.71x and `nqueens` 0.97x → 27.0x
on a change that touched no code generation whatsoever. What survives is the bignum finding. See
**R39**.

The decision rule is unchanged and needs no weighting: **ship an optimization when it improves at
least one class and regresses none.**

### Coverage, 2026-09-21

827 of 1089 definitions across the whole corpus -- these programs, the standard library and more --
compiled, up from 623 before `apply` was supported and 807 before binding chains could be emitted.
The remaining declines were 229 top-level definitions that are not procedures and 21 reaching
`call/cc`: ctak(3), fibc(2), maze(5), puzzle(3), read0(7).

Making `letrec` a core form (increment 2c′) also sped the *interpreter* up on symbolic code —
`lattice` 18.7 → 10.7 ms, `graphs` 1.00 s → 662 ms, `earley` 2.10 → 1.59 s.

## Correctness: the suite is clean

No wrong answers and no errors under either tier. Getting here took two fixes:

- **Increment 2a** — values crossing from interpreted code into compiled code no longer have
  JavaScript auto-conversion applied. Nine programs recovered, six of which had been returning a
  *wrong answer with no error*.
- **Increment 2b′** — the continuation guard is now a call-graph closure rather than a per-procedure
  name check, which fixed `maze` (R34) and catches the `btsearch` shape as well.

Neither was detectable by the tests as they stood; both now have regression coverage, and the
suite's answers are checked in both tiers by `npm test`.

## Conformance: six programs cannot run at all

| Programs | Gap |
|---|---|
| `gcbench`, `matrix`, `slatex` | identifiers containing `.` are rejected by **extended dot notation**, a deliberate and tested interop feature |
| `parsing`, `read0` | `read-char` / `peek-char` return JavaScript strings, not Scheme characters |
| `equal` | `equal?` does not terminate on circular structure, which R7RS §6.1 requires |

Tracked in [ROADMAP.md](../ROADMAP.md) and recorded as **R25**.

---

## Caveats

- **The 2026-09-26 figures were taken on a loaded machine**: another benchmark run and an IDE were
  using about two of eight performance cores throughout. Two runs of the same reference on the same
  program differed by up to 2x (`gsc` on `fib`, 354 µs and 820 µs). Class figures, which are
  geometric means over several programs, are much steadier than any one program's; read single
  programs as ±2x until re-run on a quiet machine.
- Every figure is one run of each measurement, not a best of several.
- Sizes are reduced from canonical wherever the canonical run is out of reach; the manifest records
  every substitution and its Gambit-derived expected value. Results at canonical sizes would be
  directly comparable to the tables published by `ecraven/r7rs-benchmarks`; these are not.
- `nboyer` and `sboyer` take 19-25 seconds per iteration here and dominate the wall-clock cost of a
  run without dominating any reported figure, because everything is per-class and per-iteration.
- Three classes rest on two or three programs each. Treat `vector`, `string`, `bignum` and
  `call/cc` as directional until the benchmark suite is finished (`compiler_plan.md`).
