
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import { performance } from 'perf_hooks';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const projectRoot = path.join(__dirname, '..');

import { createInterpreter } from '../src/core/interpreter/index.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { parse } from '../src/core/interpreter/reader.js';
import { SchemeDebugRuntime } from '../src/core/debug/scheme_debug_runtime.js';

// Setup Interpreter
const { interpreter, env } = createInterpreter();

// Load Standard Library
console.log('Loading standard library...');
const schemeFiles = [
    'src/core/scheme/macros.scm',
    'src/core/scheme/equality.scm',
    'src/core/scheme/cxr.scm',
    'src/core/scheme/numbers.scm',
    'src/core/scheme/list.scm',
    'src/core/scheme/control.scm',
];

for (const file of schemeFiles) {
    const code = fs.readFileSync(path.join(projectRoot, file), 'utf8');
    const exprs = parse(code);
    for (const expr of exprs) {
        interpreter.run(analyze(expr), env);
    }
}
console.log('Standard library loaded.\n');

// Benchmark workload: Fibonacci 25 to keep it quick but measurable
const fibCode = `
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
(fib 25)
`;

const ast = analyze(parse(fibCode)[1]); // [1] because [0] is the define, [1] is the (fib 25) call. 
// Actually let's just analyze/run the define first, then benchmark the call.

const defCode = `
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
`;
const defAst = analyze(parse(defCode)[0]);
interpreter.run(defAst, env);

const callCode = '(fib 25)';
const callAst = analyze(parse(callCode)[0]);

// Configuration
const RUNS = 5;

function runSync() {
    interpreter.setDebugRuntime(null); // Ensure no debug interactions
    const start = performance.now();
    interpreter.run(callAst, env);
    const end = performance.now();
    return end - start;
}

async function runAsyncNoDebug() {
    interpreter.setDebugRuntime(null);
    const start = performance.now();
    await interpreter.runAsync(callAst, env, { stepsPerYield: 10000 }); // High yield step to minimize yield overhead separate from trampoline
    const end = performance.now();
    return end - start;
}

async function runAsyncDebugOff() {
    const runtime = new SchemeDebugRuntime();
    runtime.disable();
    interpreter.setDebugRuntime(runtime);

    const start = performance.now();
    await interpreter.runAsync(callAst, env, { stepsPerYield: 10000 });
    const end = performance.now();
    return end - start;
}

async function runAsyncDebugOn() {
    const runtime = new SchemeDebugRuntime();
    runtime.enable(); // Enabled, but no breakpoints set
    interpreter.setDebugRuntime(runtime);

    const start = performance.now();
    await interpreter.runAsync(callAst, env, { stepsPerYield: 10000 });
    const end = performance.now();
    return end - start;
}

async function benchmark(name, fn) {
    process.stdout.write(`${name.padEnd(25)}: `);
    let times = [];
    for (let i = 0; i < RUNS; i++) {
        const t = await fn();
        times.push(t);
        process.stdout.write('.');
    }
    const avg = times.reduce((a, b) => a + b, 0) / RUNS;
    console.log(` Avg: ${avg.toFixed(2)} ms`);
    return avg;
}

async function main() {
    console.log('Running Benchmarks (Fibonacci 25)...');

    const tSync = await benchmark('Synchronous', () => Promise.resolve(runSync()));
    const tAsync = await benchmark('Async (No DebugRuntime)', runAsyncNoDebug);
    const tDebugOff = await benchmark('Async (Debug Off)', runAsyncDebugOff);
    const tDebugOn = await benchmark('Async (Debug On)', runAsyncDebugOn); // No breakpoints

    console.log('\n--- Relative Slowdown (vs Sync) ---');
    console.log(`Async (No Runtime): ${(tAsync / tSync).toFixed(2)}x`);
    console.log(`Async (Debug Off):  ${(tDebugOff / tSync).toFixed(2)}x`);
    console.log(`Async (Debug On):   ${(tDebugOn / tSync).toFixed(2)}x`);
}

main();
