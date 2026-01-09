/**
 * Benchmark runner for comparing numeric tower implementations.
 * 
 * Usage:
 *   node benchmarks/run_benchmarks.js
 * 
 * Run this in both branches (BigInt and Number) and compare results.
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const projectRoot = path.join(__dirname, '..');

// Import from the project's actual structure - match run_all.js approach
import { createInterpreter } from '../src/core/interpreter/index.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { parse } from '../src/core/interpreter/reader.js';

function evalScheme(interpreter, env, code) {
    const exprs = parse(code);
    let result;
    for (const expr of exprs) {
        const ast = analyze(expr);
        result = interpreter.run(ast, env);
    }
    return result;
}

function loadSchemeFile(interpreter, env, filepath) {
    const code = fs.readFileSync(filepath, 'utf8');
    return evalScheme(interpreter, env, code);
}

async function runBenchmarks() {
    console.log('='.repeat(60));
    console.log('Numeric Tower Performance Benchmarks');
    console.log('='.repeat(60));
    console.log('');

    const { interpreter, env } = createInterpreter();

    // Bootstrap standard library files directly (like run_all.js does)
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
        const filepath = path.join(projectRoot, file);
        loadSchemeFile(interpreter, env, filepath);
    }
    console.log('Standard library loaded.');

    // Load benchmark definitions
    console.log('Loading benchmark definitions...');
    loadSchemeFile(interpreter, env, path.join(__dirname, 'benchmark_arithmetic.scm'));
    loadSchemeFile(interpreter, env, path.join(__dirname, 'benchmark_mixed.scm'));

    // Try to load interop benchmarks (may fail if interop not available)
    let hasInterop = false;
    try {
        // Check if scheme->js-deep is available
        evalScheme(interpreter, env, '(define __has-interop (procedure? scheme->js-deep))');
        hasInterop = evalScheme(interpreter, env, '__has-interop');
    } catch (e) {
        hasInterop = false;
    }

    if (hasInterop) {
        try {
            loadSchemeFile(interpreter, env, path.join(__dirname, 'benchmark_interop.scm'));
            console.log('Interop benchmarks loaded.');
        } catch (e) {
            console.log('Interop benchmarks skipped:', e.message);
            hasInterop = false;
        }
    } else {
        console.log('scheme->js-deep not available - skipping interop benchmarks.');
    }
    console.log('');

    const benchmarks = [
        // Arithmetic benchmarks
        { name: 'sum-to-1M', expr: '(time-sum-to)', category: 'Arithmetic' },
        { name: 'factorial-100x1K', expr: '(time-factorial)', category: 'Arithmetic' },
        { name: 'fib-30', expr: '(time-fib)', category: 'Arithmetic' },
        { name: 'division-chain-1K', expr: '(time-division)', category: 'Arithmetic' },
        { name: 'collatz-10K', expr: '(time-collatz)', category: 'Arithmetic' },

        // Mixed benchmarks (baseline)
        { name: 'list-reverse-100K', expr: '(time-list-processing)', category: 'Non-Numeric' },
        { name: 'map-filter-10K', expr: '(time-map-filter)', category: 'Non-Numeric' },
        { name: 'append-loop-1K', expr: '(time-append-loop)', category: 'Non-Numeric' },
        { name: 'symbol-lookup-100K', expr: '(time-symbol-lookup)', category: 'Non-Numeric' },
    ];

    // Add interop benchmarks if available
    if (hasInterop) {
        benchmarks.push(
            { name: 'js-calls-10K', expr: '(time-js-calls)', category: 'JS Interop' },
            { name: 'js-roundtrip-10K', expr: '(time-roundtrip)', category: 'JS Interop' },
            { name: 'array-convert-10K', expr: '(time-array-conversion)', category: 'JS Interop' },
            { name: 'nested-convert', expr: '(time-nested-conversion)', category: 'JS Interop' },
        );
    }

    const results = {};
    const RUNS = 5;

    // Get jiffies per second for time conversion
    const jps = evalScheme(interpreter, env, '(jiffies-per-second)');
    const jpsNum = Number(jps);

    let currentCategory = '';

    for (const bench of benchmarks) {
        if (bench.category !== currentCategory) {
            currentCategory = bench.category;
            console.log(`\n--- ${currentCategory} ---`);
        }

        const times = [];
        process.stdout.write(`  ${bench.name}: `);

        for (let i = 0; i < RUNS; i++) {
            try {
                const jiffies = evalScheme(interpreter, env, bench.expr);
                times.push(Number(jiffies));
                process.stdout.write('.');
            } catch (e) {
                console.log(` ERROR: ${e.message}`);
                times.push(null);
                break;
            }
        }

        // Calculate median (ignoring nulls)
        const validTimes = times.filter(t => t !== null);
        if (validTimes.length > 0) {
            validTimes.sort((a, b) => a - b);
            const median = validTimes[Math.floor(validTimes.length / 2)];
            results[bench.name] = median;

            const ms = (median / jpsNum) * 1000;
            console.log(` ${median} jiffies (${ms.toFixed(2)} ms)`);
        } else {
            results[bench.name] = null;
        }
    }

    // Correctness check for factorial
    console.log('\n--- Correctness Check ---');
    try {
        // Use number->string in Scheme to avoid deep conversion issues with large BigInts
        const factStr = evalScheme(interpreter, env, '(number->string (factorial 100))');
        const digits = factStr.length;
        console.log(`  factorial(100) has ${digits} digits`);
        console.log(`  First 20 chars: ${factStr.substring(0, 20)}...`);

        // Correct value starts with: 93326215443944152681
        const expected = '93326215443944152681';
        const actual = factStr.substring(0, 20);
        if (actual === expected) {
            console.log('  ✓ CORRECT');
        } else {
            console.log(`  ✗ INCORRECT (expected ${expected})`);
        }
    } catch (e) {
        console.log(`  factorial(100) ERROR: ${e.message}`);
    }

    // Output JSON for comparison
    console.log('\n--- JSON Results ---');
    console.log(JSON.stringify(results, null, 2));
}

runBenchmarks().catch(err => {
    console.error('Benchmark failed:', err);
    process.exit(1);
});
