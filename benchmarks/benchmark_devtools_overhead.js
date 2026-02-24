/**
 * @fileoverview Benchmark: DevTools async stack tagging overhead.
 *
 * Measures the performance impact of Phase 4's console.createTask-based
 * async stack tagging on the interpreter's hot loop.
 *
 * Since console.createTask is not available in Node.js, this benchmark
 * uses a mock that replicates the object allocation and function wrapping
 * to measure our code's overhead. For real V8 createTask overhead, use
 * the browser benchmark (benchmark_devtools_browser.html).
 *
 * Usage:
 *   node benchmarks/benchmark_devtools_overhead.js
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import { createInterpreter } from '../src/core/interpreter/index.js';
import { parse } from '../src/core/interpreter/reader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { DevToolsDebugIntegration } from '../src/debug/devtools/devtools_debug.js';
import { SchemeSourceRegistry } from '../src/debug/devtools/source_registry.js';
import SchemeDebugRuntime from '../src/debug/scheme_debug_runtime.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const projectRoot = path.join(__dirname, '..');

// =============================================================================
// Helpers
// =============================================================================

/**
 * Evaluates Scheme code in the given interpreter/env.
 */
function evalScheme(interpreter, env, code) {
    const exprs = parse(code);
    let result;
    for (const expr of exprs) {
        const ast = analyze(expr);
        result = interpreter.run(ast, env);
    }
    return result;
}

/**
 * Loads a Scheme file into the interpreter.
 */
function loadSchemeFile(interpreter, env, filepath) {
    const code = fs.readFileSync(filepath, 'utf8');
    return evalScheme(interpreter, env, code);
}

/**
 * Runs a benchmark N times and returns [medianMs, timesMs[]].
 */
function benchmark(fn, runs = 7) {
    const times = [];
    for (let i = 0; i < runs; i++) {
        const start = performance.now();
        fn();
        times.push(performance.now() - start);
    }
    times.sort((a, b) => a - b);
    return {
        median: times[Math.floor(times.length / 2)],
        times
    };
}

// =============================================================================
// Main
// =============================================================================

async function main() {
    console.log('='.repeat(60));
    console.log('DevTools Async Stack Tagging — Overhead Benchmark');
    console.log('='.repeat(60));
    console.log('');

    // --- Set up interpreter with standard library ---
    const { interpreter, env } = createInterpreter();
    const schemeFiles = [
        'src/core/scheme/macros.scm',
        'src/core/scheme/equality.scm',
        'src/core/scheme/cxr.scm',
        'src/core/scheme/numbers.scm',
        'src/core/scheme/list.scm',
        'src/core/scheme/control.scm',
    ];
    for (const file of schemeFiles) {
        loadSchemeFile(interpreter, env, path.join(projectRoot, file));
    }

    // Define test procedures.
    // Wrapper functions discard the result to avoid BigInt-to-JS conversion errors.
    evalScheme(interpreter, env, `
    (define (fib n)
      (if (<= n 1) n
        (+ (fib (- n 1)) (fib (- n 2)))))

    (define (factorial n)
      (if (<= n 1) 1
        (* n (factorial (- n 1)))))

    (define (sum-to n)
      (define (loop i acc)
        (if (> i n) acc
          (loop (+ i 1) (+ acc i))))
      (loop 1 0))

    (define (bench-fib) (fib 25) (if #f #f))
    (define (bench-fact) (factorial 500) (if #f #f))
    (define (bench-sum) (sum-to 50000) (if #f #f))
  `);

    const RUNS = 7;
    const FIB = '(bench-fib)';
    const FACT = '(bench-fact)';
    const SUM = '(bench-sum)';

    // --- Benchmark 1: Baseline (no debug runtime) ---
    console.log('--- Baseline (no debug) ---');
    const baseline = {};

    const b1 = benchmark(() => evalScheme(interpreter, env, FIB), RUNS);
    baseline.fib25 = b1.median;
    console.log(`  fib(25):       ${b1.median.toFixed(2)} ms`);

    const b2 = benchmark(() => evalScheme(interpreter, env, FACT), RUNS);
    baseline.fact500 = b2.median;
    console.log(`  factorial(500): ${b2.median.toFixed(2)} ms`);

    const b3 = benchmark(() => evalScheme(interpreter, env, SUM), RUNS);
    baseline.sum50k = b3.median;
    console.log(`  sum-to(50000): ${b3.median.toFixed(2)} ms`);

    // --- Benchmark 2: With debug runtime (StackTracer only, no DevTools) ---
    console.log('\n--- With Debug Runtime (StackTracer only) ---');
    const debugOnly = {};

    const debugRuntime = new SchemeDebugRuntime();
    debugRuntime.enable();
    interpreter.setDebugRuntime(debugRuntime);

    const d1 = benchmark(() => evalScheme(interpreter, env, FIB), RUNS);
    debugOnly.fib25 = d1.median;
    console.log(`  fib(25):       ${d1.median.toFixed(2)} ms`);

    const d2 = benchmark(() => evalScheme(interpreter, env, FACT), RUNS);
    debugOnly.fact500 = d2.median;
    console.log(`  factorial(500): ${d2.median.toFixed(2)} ms`);

    const d3 = benchmark(() => evalScheme(interpreter, env, SUM), RUNS);
    debugOnly.sum50k = d3.median;
    console.log(`  sum-to(50000): ${d3.median.toFixed(2)} ms`);

    // --- Benchmark 3: With DevTools integration (taskStack tracking, no real createTask) ---
    console.log('\n--- With DevTools Integration (task tracking, no createTask) ---');
    const withDevtools = {};

    const registry = new SchemeSourceRegistry();
    const devtoolsDebug = new DevToolsDebugIntegration(registry);
    devtoolsDebug.hasCreateTask = false; // No real createTask in Node
    debugRuntime.setDevToolsIntegration(devtoolsDebug);

    const t1 = benchmark(() => evalScheme(interpreter, env, FIB), RUNS);
    withDevtools.fib25 = t1.median;
    console.log(`  fib(25):       ${t1.median.toFixed(2)} ms`);

    const t2 = benchmark(() => evalScheme(interpreter, env, FACT), RUNS);
    withDevtools.fact500 = t2.median;
    console.log(`  factorial(500): ${t2.median.toFixed(2)} ms`);

    const t3 = benchmark(() => evalScheme(interpreter, env, SUM), RUNS);
    withDevtools.sum50k = t3.median;
    console.log(`  sum-to(50000): ${t3.median.toFixed(2)} ms`);

    // --- Benchmark 4: With mock createTask (object allocation overhead) ---
    console.log('\n--- With Mock createTask (allocation overhead) ---');
    const withMock = {};

    // Install a mock createTask that mimics V8's allocation pattern
    console.createTask = (name) => ({
        run: (fn) => fn()
    });
    devtoolsDebug.hasCreateTask = true;

    const m1 = benchmark(() => evalScheme(interpreter, env, FIB), RUNS);
    withMock.fib25 = m1.median;
    console.log(`  fib(25):       ${m1.median.toFixed(2)} ms`);

    const m2 = benchmark(() => evalScheme(interpreter, env, FACT), RUNS);
    withMock.fact500 = m2.median;
    console.log(`  factorial(500): ${m2.median.toFixed(2)} ms`);

    const m3 = benchmark(() => evalScheme(interpreter, env, SUM), RUNS);
    withMock.sum50k = m3.median;
    console.log(`  sum-to(50000): ${m3.median.toFixed(2)} ms`);

    // Clean up mock
    delete console.createTask;

    // --- Summary ---
    console.log('\n' + '='.repeat(60));
    console.log('Summary: Overhead vs Baseline');
    console.log('='.repeat(60));

    const reportOverhead = (name, base, debug, devtools, mock) => {
        const debugPct = ((debug / base - 1) * 100).toFixed(1);
        const devtoolsPct = ((devtools / base - 1) * 100).toFixed(1);
        const mockPct = ((mock / base - 1) * 100).toFixed(1);
        console.log(`  ${name}:`);
        console.log(`    Baseline:            ${base.toFixed(2)} ms`);
        console.log(`    + StackTracer:       ${debug.toFixed(2)} ms (${debugPct}%)`);
        console.log(`    + TaskStack (no CT): ${devtools.toFixed(2)} ms (${devtoolsPct}%)`);
        console.log(`    + Mock createTask:   ${mock.toFixed(2)} ms (${mockPct}%)`);
    };

    reportOverhead('fib(25)', baseline.fib25, debugOnly.fib25, withDevtools.fib25, withMock.fib25);
    reportOverhead('factorial(500)', baseline.fact500, debugOnly.fact500, withDevtools.fact500, withMock.fact500);
    reportOverhead('sum-to(50000)', baseline.sum50k, debugOnly.sum50k, withDevtools.sum50k, withMock.sum50k);

    console.log('\nNote: For real console.createTask overhead, use benchmark_devtools_browser.html in Chrome.');
}

main().catch(err => {
    console.error('Benchmark failed:', err);
    process.exit(1);
});
