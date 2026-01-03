/**
 * Callable Closures Interop Tests
 * 
 * Tests that verify Scheme closures and continuations are callable
 * directly from JavaScript in all contexts.
 */

import { assert, createTestLogger } from '../harness/helpers.js';
import { Interpreter } from '../../src/core/interpreter/interpreter.js';
import { createGlobalEnvironment } from '../../src/core/primitives/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { isSchemeClosure, isSchemeContinuation } from '../../src/core/interpreter/values.js';
import * as fs from 'fs';
import * as path from 'path';

/**
 * Creates a fresh interpreter instance for testing.
 */
function createInterpreter() {
    const interpreter = new Interpreter();
    const globalEnv = createGlobalEnvironment(interpreter);
    interpreter.setGlobalEnv(globalEnv);
    return { interpreter, globalEnv };
}

/**
 * Helper to run Scheme code and return the result.
 */
function runScheme(interpreter, code) {
    const ast = analyze(parse(code)[0]);
    return interpreter.run(ast);
}

/**
 * Helper to run multiple Scheme expressions
 */
function runSchemeProgram(interpreter, code) {
    const exprs = parse(code);
    let result;
    for (const expr of exprs) {
        const ast = analyze(expr);
        result = interpreter.run(ast);
    }
    return result;
}

/**
 * Loads the Scheme bootstrap files needed for tests.
 */
function loadBootstrap(interpreter) {
    const schemeFiles = [
        'src/core/scheme/macros.scm',     // Core macros: and, let, letrec, cond
        'src/core/scheme/equality.scm',   // equal?
        'src/core/scheme/cxr.scm',        // caar, cadr, etc.
        'src/core/scheme/numbers.scm',    // =, <, >, zero?, max, gcd, round
        'src/core/scheme/list.scm',       // map, for-each, memq, assq, length
        'src/core/scheme/control.scm',    // when, unless, or, let*, do, case
    ];

    for (const file of schemeFiles) {
        const filePath = path.join(process.cwd(), file);
        const code = fs.readFileSync(filePath, 'utf8');
        runSchemeProgram(interpreter, code);
    }
}

export async function runCallableClosuresTests(logger) {
    logger.title('Running Callable Closures Interop Tests...');

    const { interpreter, globalEnv } = createInterpreter();

    // Load bootstrap files for full Scheme support (=, define, etc.)
    loadBootstrap(interpreter);

    // =========================================================================
    // Basic Callable Closure Tests
    // =========================================================================

    logger.title('Basic Callable Closures...');

    // Test 1: Closures are functions
    {
        const closure = runScheme(interpreter, '(lambda (x) (+ x 1))');
        assert(logger, "Closure is a function",
            typeof closure, 'function');
        assert(logger, "Closure has isSchemeClosure marker",
            isSchemeClosure(closure), true);
    }

    // Test 2: Closures can be called directly from JS
    {
        const closure = runScheme(interpreter, '(lambda (x) (+ x 1))');
        const result = closure(41);
        assert(logger, "Closure callable from JS",
            result, 42);
    }

    // Test 3: Closures work with multiple arguments
    {
        const closure = runScheme(interpreter, '(lambda (a b c) (+ a b c))');
        const result = closure(1, 2, 3);
        assert(logger, "Closure with multiple args",
            result, 6);
    }

    // Test 4: Closures work with rest parameters
    {
        const closure = runScheme(interpreter, '(lambda (first . rest) (cons first rest))');
        const result = closure(1, 2, 3);
        // Result is a Cons pair (1 . (2 . (3 . ())))
        assert(logger, "Closure with rest param - car",
            result.car, 1);
    }

    // =========================================================================
    // Closures in Data Structures
    // =========================================================================

    logger.title('Closures in Data Structures...');

    // Test 5: Closure in vector
    {
        const vector = runScheme(interpreter, '(vector (lambda () 42) 99)');
        assert(logger, "Vector first element is callable",
            typeof vector[0], 'function');
        const result = vector[0]();
        assert(logger, "Closure in vector callable",
            result, 42);
    }

    // Test 6: Closure assigned to JS global via js-eval + set!
    // This is the ORIGINAL USE CASE that motivated this feature!
    {
        // Use js-eval to define the JS global variable
        runScheme(interpreter, '(js-eval "var testClosure = null")');

        // Use set! to assign a closure to it
        runScheme(interpreter, '(set! testClosure (lambda (x) (* x x)))');

        assert(logger, "JS global is callable function",
            typeof globalThis.testClosure, 'function');

        // Call from JS!
        const result = globalThis.testClosure(7);
        assert(logger, "Closure from JS global callable",
            result, 49);

        // Also call from JS via js-eval
        const result2 = runScheme(interpreter, '(js-eval "testClosure(8)")');
        assert(logger, "Closure called via js-eval",
            result2, 64);

        // Cleanup
        runScheme(interpreter, '(js-eval "delete globalThis.testClosure")');
    }

    // Test 7: Closure accessible via environment lookup is callable
    {
        runScheme(interpreter, '(define test-closure-var (lambda (x) (* x x)))');

        // Get the closure from Scheme's environment
        const closure = globalEnv.lookup('test-closure-var');

        assert(logger, "Env lookup returns callable function",
            typeof closure, 'function');

        const result = closure(7);
        assert(logger, "Closure from env lookup callable",
            result, 49);
    }

    // Test 8: Closure in Map
    {
        const fn = runScheme(interpreter, '(lambda (n) (+ n 100))');
        const map = new Map();
        map.set('add100', fn);

        const result = map.get('add100')(5);
        assert(logger, "Closure from Map callable",
            result, 105);
    }

    // =========================================================================
    // Continuation Tests
    // =========================================================================

    logger.title('Callable Continuations...');

    // Test 9: Continuation is a function
    {
        let capturedK = null;
        globalEnv.define('capture-k', (k) => { capturedK = k; return 0; });

        runScheme(interpreter, '(call/cc (lambda (k) (capture-k k)))');

        assert(logger, "Continuation is a function",
            typeof capturedK, 'function');
        assert(logger, "Continuation has marker",
            isSchemeContinuation(capturedK), true);
    }

    // Test 10: Continuation callable from JS
    {
        let capturedK = null;
        globalEnv.define('capture-k2', (k) => { capturedK = k; return 'first'; });

        const result1 = runScheme(interpreter, `
            (+ 100 (call/cc (lambda (k) 
                              (capture-k2 k)
                              10)))
        `);

        // First run: returns 100 + 10 = 110
        assert(logger, "First continuation run",
            result1, 110);

        // Invoke continuation from JS with value 50
        const result2 = capturedK(50);
        // Should return 100 + 50 = 150
        assert(logger, "Continuation invoked from JS",
            result2, 150);
    }

    // =========================================================================
    // TCO Tests
    // =========================================================================

    logger.title('TCO Compliance...');

    // Test 11: Pure Scheme tail recursion (large N)
    {
        runScheme(interpreter, `
            (define (tco-loop n acc)
                (if (= n 0) acc (tco-loop (- n 1) (+ acc 1))))
        `);
        const result = runScheme(interpreter, '(tco-loop 50000 0)');
        assert(logger, "Pure Scheme TCO (50000 iterations)",
            result, 50000);
    }

    // Test 12: Mutual tail recursion
    {
        runSchemeProgram(interpreter, `
            (define (tco-even? n) 
                (if (= n 0) #t (tco-odd? (- n 1))))
            (define (tco-odd? n) 
                (if (= n 0) #f (tco-even? (- n 1))))
        `);
        const result = runScheme(interpreter, '(tco-even? 50000)');
        assert(logger, "Mutual TCO even?/odd?",
            result, true);
    }

    // Test 13: Closure stored in JS, called from JS with TCO
    {
        let tcoFn = null;
        globalEnv.define('store-fn', (fn) => { tcoFn = fn; });

        runSchemeProgram(interpreter, `
            (define (js-tco-loop n acc)
                (if (= n 0) acc (js-tco-loop (- n 1) (+ acc 1))))
            (store-fn js-tco-loop)
        `);

        // Call the closure from pure JS
        const result = tcoFn(10000, 0);
        assert(logger, "TCO closure called from JS",
            result, 10000);
    }

    // Test 14: Closure in vector, called repeatedly (TCO)
    {
        const result = runScheme(interpreter, `
            (let ((v (vector #f)))
                (vector-set! v 0 
                    (lambda (n acc)
                        (if (= n 0) acc 
                            ((vector-ref v 0) (- n 1) (+ acc n)))))
                ((vector-ref v 0) 1000 0))
        `);
        // Sum of 1000 + 999 + ... + 1 = 500500
        assert(logger, "TCO with closure in vector",
            result, 500500);
    }

    // =========================================================================
    // Dynamic-Wind + Continuation Tests
    // =========================================================================

    logger.title('Dynamic-Wind with Callable Continuations...');

    // Test 15: Basic dynamic-wind with global log
    {
        runScheme(interpreter, "(define dw-log1 '())");
        runScheme(interpreter, `
            (dynamic-wind
                (lambda () (set! dw-log1 (cons 'before dw-log1)))
                (lambda () (set! dw-log1 (cons 'in dw-log1)))
                (lambda () (set! dw-log1 (cons 'after dw-log1))))
        `);
        const result = runScheme(interpreter, 'dw-log1');
        // Should be (after in before) in reverse order  
        assert(logger, "Dynamic-wind basic",
            result.car.name, 'after');
    }

    // Test 16: Continuation from JS works  
    {
        let capturedK = null;
        globalEnv.define('capture-k-final', (k) => { capturedK = k; });

        // Capture a continuation and store it
        const result1 = runScheme(interpreter, `
            (+ 100 (call/cc (lambda (k) 
                              (capture-k-final k)
                              10)))
        `);

        assert(logger, "Continuation capture result",
            result1, 110);

        // Invoke the captured continuation from JS
        const result2 = capturedK(50);
        assert(logger, "Continuation invoked from JS returns correct value",
            result2, 150);
    }
}
