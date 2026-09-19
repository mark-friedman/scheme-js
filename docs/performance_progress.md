# Performance progress

> **This file is generated.** Do not edit it by hand — run
> `npm run benchmark:record -- --stage "<name>" --note "<what changed>"`,
> which appends to `benchmarks/history.json` and rewrites this document.

Tracks the effect of each stage of the compiler effort described in
[compiler_strategy.md](compiler_strategy.md). Methodology and the full Stage 0
analysis are in [performance_baseline.md](performance_baseline.md).

## Snapshots

| Stage | Recorded | Commit | Platform | What changed |
|---|---|---|---|---|
| **Stage 0 (baseline)** | 2026-09-18 | `cfa2d17` | darwin/arm64, Node v24.11.1 | Measurement infrastructure only; no optimization work. Three pre-existing debugger bugs fixed. |
| **Stage 1** | 2026-09-18 | `044aa69` | darwin/arm64, Node v24.11.1 | Interpreter representation. Native comparison primitives (also fixing rational comparison, which had been comparing Rational objects as strings); inlined evaluation of operators, operands and if-tests that cannot capture a continuation; application logic reached through a live binding; lazy JS-context capture. Lexical addressing deliberately skipped -- measured at only ~15% of runtime. |

## Wall-clock timings

Lower is better. Sizes are held fixed across stages so the numbers stay comparable;
a cell reading `size changed` means that benchmark was re-sized and cannot be
compared to the baseline.

| Benchmark | size | Stage 0 (baseline) | Stage 1 | vs baseline |
|---|---|---|---|---|
| `fib` | 25 | 593 ms | 136 ms | **4.35x** |
| `tak` | 18 | 178 ms | 55 ms | **3.25x** |
| `oddeven` | 100000 | 216 ms | 53 ms | **4.08x** |
| `nqueens` | 8 | 143 ms | 58 ms | **2.48x** |
| `ctak` | 18 | 233 ms | 110 ms | **2.12x** |
| `contfib` | 20 | 75 ms | 30 ms | **2.51x** |
| `btsearch` | 200 | 326 ms | 165 ms | **1.98x** |
| `threads` | 400 | 160 ms | 128 ms | **1.25x** |

**Mean speedup vs Stage 0 (baseline):** 2.75x arithmetic, 2.57x geometric (over 8 comparable benchmarks).

The geometric mean is the one to quote. An arithmetic mean over ratios is
dominated by whichever benchmark improved most, which flatters the result.

## Evaluator step counts

Deterministic dispatch counts, measured at smaller sizes than the timings. These are
the honest measure of whether an optimization removed work: they are identical on
every machine and immune to JIT warm-up, so a change here is real in a way that a
change in wall-clock time is not.

| Benchmark | size | Stage 0 (baseline) | Stage 1 | reduction |
|---|---|---|---|---|
| `fib` | 18 | 338,619 | 66,886 | **5.06x** |
| `tak` | 14 | 437,092 | 93,110 | **4.69x** |
| `oddeven` | 2000 | 68,039 | 12,006 | **5.67x** |
| `nqueens` | 6 | 103,789 | 30,408 | **3.41x** |
| `ctak` | 14 | 349,518 | 80,296 | **4.35x** |
| `contfib` | 14 | 58,518 | 12,800 | **4.57x** |
| `btsearch` | 40 | 174,762 | 51,868 | **3.37x** |
| `threads` | 40 | 56,481 | 21,107 | **2.68x** |

## Distance to reference implementations

How many times slower than each reference, at the same size. Gambit `gsi` is an
*interpreter*, so reaching parity with it means we have stopped being slow for
avoidable reasons. Racket CS is a compiler and is the target for the compiler
stages, not for interpreter work.

### vs Gambit `gsi` (interpreter)

| Benchmark | Stage 0 (baseline) | Stage 1 |
|---|---|---|
| `fib` | 28.1x | 6.78x |
| `tak` | 26.8x | 8.20x |
| `oddeven` | 29.4x | 6.93x |
| `nqueens` | 22.1x | 8.91x |
| `ctak` | 19.4x | 9.73x |
| `contfib` | 22.2x | 10.2x |
| `btsearch` | 29.0x | 15.0x |
| `threads` | 10.5x | 8.39x |

### vs Racket CS (compiled)

| Benchmark | Stage 0 (baseline) | Stage 1 |
|---|---|---|
| `fib` | 1372x | 345x |
| `tak` | 1498x | 475x |
| `oddeven` | 2718x | 674x |
| `nqueens` | 1143x | 444x |
| `ctak` | 81.6x | 42.8x |
| `contfib` | 40.8x | 20.3x |
| `btsearch` | 292x | 150x |
| `threads` | 19.8x | 16.1x |

---

*Regenerate with `npm run benchmark:record -- --stage "<name>"`.*
