# Chrome DevTools Debugging — Performance Analysis

Results from `benchmarks/benchmark_devtools_overhead.js`, measuring the cost of the probe-based DevTools integration on interpreter execution.

## Test Workloads

| Workload | Description | Character |
|----------|-------------|-----------|
| **fib(25)** | Deeply recursive Fibonacci | Many function calls, many unique source locations |
| **factorial(500)** | Short recursive chain | Few iterations, fast baseline |
| **sum-to(50000)** | Tail-recursive loop | 50K iterations, exercises TCO and StackTracer hooks |

## Runtime Overhead

Each column adds one layer of instrumentation on top of the previous:

| Workload | Baseline | + StackTracer | + TaskStack (no CT) | + Mock createTask | + Full Probes |
|----------|----------|---------------|---------------------|-------------------|---------------|
| **fib(25)** | 606 ms | 675 ms (+11%) | 675 ms (+11%) | 681 ms (+12%) | 1103 ms (+82%) |
| **factorial(500)** | 1.56 ms | 1.76 ms (+13%) | 1.66 ms (+7%) | 1.74 ms (+12%) | 2.53 ms (+63%) |
| **sum-to(50000)** | 137 ms | 5481 ms (+3901%) | 5540 ms (+3944%) | 5700 ms (+4061%) | 5740 ms (+4091%) |

### Interpretation

- **fib(25) and factorial(500)** are the representative workloads. Full probe overhead of **62–82%** is within the expected range for a source-level debugger — comparable to Python's `sys.settrace` or Node.js Inspector overhead. This is acceptable because DevTools debugging is only active when explicitly enabled via `?scheme-debug=true` or the `debug` script attribute.

- **sum-to(50000)** shows an extreme ~40× slowdown, but this is almost entirely caused by the **StackTracer** (the first instrumentation layer), not by probes. The StackTracer fires `enterFrame`/`exitFrame` hooks on every tail-call iteration — 50K enter/exit pairs. Probes add only ~5% on top of that already-inflated time. This is a known limitation of the StackTracer with tight tail-recursive loops and is orthogonal to the DevTools probe system.

- **TaskStack and createTask** add negligible overhead (~1–2%) beyond the StackTracer, confirming that async stack tagging (Phase 4) is essentially free.

## Hot-Path Performance

| Metric | Result | Target | Status |
|--------|--------|--------|--------|
| `maybeHit()` throughput (same location, fast-path skip) | **12.4M calls/sec** | > 1M/sec | ✅ |
| Probe generation (1201 lines, 19962 bytes) | **4.8 ms** | < 100 ms | ✅ |

- **`maybeHit()` fast path**: When the source location hasn't changed (the common case during sub-expression evaluation), the dedup check (`key === lastHitKey && env === lastHitEnv`) short-circuits immediately. At 12M+ calls/sec, this adds virtually zero overhead to the trampoline hot loop.

- **Probe generation**: Parsing a ~1200-line Scheme file, assigning expression IDs, generating the probe script with source maps, and injecting it completes in under 5ms. This is a one-time cost per source file.

## When Overhead Applies

The DevTools integration is **zero-cost when disabled**. The trampoline check is:

```javascript
if (this.devtoolsDebug?.enabled) { ... }
```

When `devtoolsDebug` is `null` (the default), this is a single falsy property access — negligible in V8's optimizing compiler.

## Running the Benchmark

```bash
# Node.js (uses mock console.createTask)
node benchmarks/benchmark_devtools_overhead.js

# Browser (uses real console.createTask for V8 overhead)
# Serve project root and open benchmarks/benchmark_devtools_browser.html
```
