# Performance baseline (Stage 0)

This is the measurement gate for the compiler effort described in
[compiler_findings.md](compiler_findings.md). It records where the implementation stands before
any optimization work, so that later claims of improvement are checkable rather than asserted.

**Measured 2026-09-18 on Apple Silicon (darwin/arm64), Node v24.11.1.** Raw data is in
[`benchmarks/baseline_standard.json`](../benchmarks/baseline_standard.json). Re-measure before
relying on any of this on other hardware.

## How to reproduce

```bash
npm run benchmark:standard          # timings, with correctness checks
npm run benchmark:steps             # deterministic evaluator step counts
npm run benchmark:implementations   # same programs under Gambit and Racket
npm run benchmark:profile fib       # CPU profile of a single benchmark
npm run audit:r7rs                  # R7RS-small conformance audit
```

## The benchmark suite

Eight portable R7RS programs live in [`benchmarks/programs/`](../benchmarks/programs/). They are
written so the same source runs unmodified under scheme-js-4, Gambit and Racket: the driver
supplies `bench-size` and then calls `(bench-run)`. Seven of the eight are the complete benchmark set from
Thivierge & Feeley, *Efficient Compilation of Tail Calls and Continuations to JavaScript*
(SFP 2012) -- `tak` is the addition, from the Gabriel set -- so our numbers can be placed next to
theirs.

> [!WARNING]
> Two caveats, recorded in R20-R22 of [compiler_findings.md](compiler_findings.md). **`threads` is
> not their `threads10`**: theirs uses a vector-based doubly-linked queue and runs about a million
> context switches, mine is a list-based scheduler doing four thousand, so its numbers are not
> comparable to their table. And comparability requires `canonical` sizes, which nothing reported
> here uses. More importantly, a coverage check showed this suite exercises **16 distinct callables
> against 136 in real code**, with 98% of its calls landing on the fifteen primitives the compiler
> inlines against 34% in real code -- so it is **overfitted** to the optimizations chosen against
> it. Use [`npm run benchmark:macro`](../benchmarks/run_macro.js) for a transfer check.

| Program | Measures |
|---|---|
| `fib` | Raw non-tail call throughput. No allocation. |
| `tak` | Call throughput with three arguments and a deeper call graph. |
| `oddeven` | Pure tail-call throughput; no continuation frames created at all. |
| `nqueens` | Allocation and GC pressure from short-lived lists. |
| `ctak` | Continuation capture at moderate stack depth, once per recursion step. |
| `contfib` | Very frequent capture at shallow depth. Separates per-capture cost from O(depth) cost. |
| `btsearch` | Backtracking. **Requires multi-shot continuations** — a wrong answer is a correctness failure. |
| `threads` | Coroutine scheduler. Continuations stored and resumed out of order. |

Sizes come in two profiles. `canonical` matches the published literature; `quick` is sized so the
suite finishes in workable time on the interpreter as it stands. All numbers below use `quick`,
which is recorded alongside every result — a time is meaningless without its size. As the
implementation gets faster, raise `quick` towards `canonical` rather than adding new programs.

## Cross-implementation timings

All three implementations agree on the result of all eight programs, which is a stronger
correctness check than any hardcoded expected value.

| Benchmark | size | scheme-js-4 | Gambit `gsi` | Racket CS | vs gsi | vs Racket |
|-----------|------|-------------|--------------|-----------|--------|-----------|
| fib       | 25   | 564.5 ms | 21.5 ms | 0.4 ms | 26x | 1384x |
| tak       | 18   | 167.5 ms | 6.3 ms  | 0.1 ms | 27x | 1485x |
| oddeven   | 100000 | 195.7 ms | 7.0 ms | 0.1 ms | 28x | 2646x |
| nqueens   | 8    | 134.7 ms | 6.1 ms  | 0.1 ms | 22x | 1133x |
| ctak      | 18   | 215.6 ms | 11.0 ms | 2.6 ms | 20x | 84x |
| contfib   | 20   | 68.7 ms  | 3.2 ms  | 1.7 ms | 21x | 40x |
| btsearch  | 200  | 305.7 ms | 10.8 ms | 2.2 ms | 28x | 137x |
| threads   | 400  | 161.8 ms | 15.3 ms | 7.8 ms | 11x | 21x |

Gambit `gsi` is an interpreter; Racket CS is a compiler. Two things stand out.

**We are 20–28x slower than another interpreter.** Being interpreted does not account for the
gap. Roughly a factor of 25 is available without changing the execution model at all.

**Our continuations are relatively less bad than our baseline execution.** Against Racket the
ratio falls from ~1400x on `fib` to 40–140x on the continuation programs. The explicit
frame-stack representation is not the problem; ordinary evaluation is. This is worth carrying
into the Stage 2a calling-convention decision, which had assumed continuation cost was the thing
most at risk.

For external calibration on the same machine: plain JavaScript computes `fib(30)` in 10 ms,
Racket CS in 6.3 ms, and this implementation in 6,524 ms.

## Where the time goes

`npm run benchmark:profile fib`:

```
 share |    self | function
-------|---------|-------------------------------------------
  30.1% |   204.7 | trampoline (inlined Interpreter.run loop)
  25.8% |   175.5 | AppFrame.step        frames.js:270
   7.1% |    48.1 | TailAppNode.step     ast_nodes.js:342
   6.1% |    41.3 | Environment.lookup   environment.js:58
   5.1% |    34.9 | Environment.extendMany environment.js:38
   3.9% |    26.8 | garbage collector
   1.7% |    11.8 | Interpreter.pushJsContext
   1.4% |     9.7 | `-`                  primitives/math.js:318

evaluator overhead: 83.8%
primitive work:     2.8%
```

**The program's actual arithmetic is under 3% of runtime.** Everything else is the cost of
interpreting it. That ratio, not the absolute times, is the argument for a compiler.

A note on reading this profile: V8 inlines `Interpreter.run`'s dispatch loop into whichever
function calls it, so the caller absorbs the trampoline's self time. `benchmarks/profile.js`
therefore routes the timed call through a function named `trampoline` purely so that time stays
attributable instead of disappearing into `main`.

## Evaluator step counts

Timings are noisy and machine-specific; step counts are neither. `npm run benchmark:steps` reports
how many dispatches a program costs, which is the right way to check whether an optimization
removed work rather than got lucky with the JIT.

`fib` at size 18 — 8,361 calls to `fib` — costs **338,619 evaluator steps**, about **40 steps per
call**:

| share | steps | node / frame type |
|------:|------:|---|
| 38.3% | 129,593 | `AppFrame` |
| 25.9% |  87,791 | `VariableNode` |
| 14.8% |  50,166 | `TailAppNode` |
|  8.6% |  29,263 | `LiteralNode` |
|  6.2% |  20,903 | `IfNode` |
|  6.2% |  20,903 | `IfFrame` |

The source has six applications per call (`<`, `+`, two `-`, two recursive `fib`), so ~6
`TailAppNode` steps is expected. The 15.5 `AppFrame` steps and 10.5 variable lookups per call are
not: `AppFrame` allocates a fresh frame per *argument*, and `<` is a Scheme-level variadic
procedure with a rest parameter rather than a primitive, so a single integer comparison expands
into four nested applications.

Instrumentation lives in [`src/debug/instrumentation.js`](../src/debug/instrumentation.js) and
attaches by wrapping the interpreter rather than by adding a branch inside it, so it costs nothing
when nobody is measuring.

## R7RS-small conformance

`npm run audit:r7rs` probes every identifier required by the standard. Result: **285 bound, 43
syntactic keywords present, 7 missing, 2 stubs, 2 libraries not importable.**

**Stubs — bound, but throw unconditionally.** These are the dangerous category, because they look
like conformance until called:

| Procedure | Behaviour |
|---|---|
| `string-set!` | throws: *strings are immutable in this implementation for JavaScript interoperability* |
| `string-fill!` | throws, same reason |

**Missing:**

| Library | Identifier |
|---|---|
| `(scheme base)` | `call-with-port`, `rationalize`, `read-bytevector!`, `string-copy!` |
| `(scheme file)` | `open-binary-input-file`, `open-binary-output-file` |
| `(scheme load)` | `load` |

**Libraries that cannot be imported:** `(scheme inexact)` and `(scheme load)` have no `.sld` file.
Note that all twelve `(scheme inexact)` procedures (`sin`, `sqrt`, `nan?` and so on) *are* bound
globally — this is a packaging gap, not a functionality gap, and is cheap to close.

The string-mutability cluster is the substantive one: `string-set!`, `string-fill!` and
`string-copy!` are exactly the three mutation procedures, and they are absent for the same
deliberate reason. Closing it means changing the value representation, which
[compiler_findings.md](compiler_findings.md) schedules into Stage 2b alongside the other
representation work.

The audit is a reporting tool rather than a registered test, because wiring it into `npm test`
today would simply fail the build. It should be promoted to a test once the deviations are closed,
so the surface cannot silently regress.

## What this establishes for Stage 1

The plan predicts 10–30x from fixing the interpreter's representation, with no compiler. The
measurements above give three independent targets to check that against:

1. **`fib` step count** should fall well below 40 steps per call — most directly by making `<` a
   primitive and by allocating one `AppFrame` per call rather than per argument.
2. **`Environment.lookup` and `extendMany`** together account for 11% of profile time and should
   approach zero once lexical addressing replaces string-keyed `Map` chains.
3. **The gap to Gambit `gsi`** should close from ~25x to roughly parity. `gsi` is the honest
   target for an interpreter; Racket is the target for the compiler.
