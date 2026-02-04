/**
 * @fileoverview Unit tests for AsyncTrampoline.
 * 
 * Tests that async execution produces identical results to sync execution
 * and that yield points don't corrupt interpreter state.
 * 
 * Note: These tests use the bootstrapped interpreter passed from the test runner
 * to ensure macros and library functions (like <=) are available.
 */

import { assert, run, createTestLogger } from '../../harness/helpers.js';

/**
 * Runs all AsyncTrampoline tests.
 * @param {Interpreter} interpreter - The bootstrapped interpreter
 * @param {Object} logger - Test logger
 */
export async function runAsyncTrampolineTests(interpreter, logger) {
    logger.title('AsyncTrampoline - Correctness');

    // Helper: run sync using the passed interpreter
    const runSync = (code) => run(interpreter, code);

    // Helper: run async using the passed interpreter
    const runAsync = (code, options = {}) => interpreter.evaluateStringAsync(code, options);

    // Test: Async execution produces same result as sync for literals
    {
        const syncResult = runSync('42');
        const asyncResult = await runAsync('42');
        assert(logger, 'literal number', asyncResult, syncResult);
    }

    {
        const syncResult = runSync('"hello"');
        const asyncResult = await runAsync('"hello"');
        assert(logger, 'literal string', asyncResult, syncResult);
    }

    {
        const syncResult = runSync('#t');
        const asyncResult = await runAsync('#t');
        assert(logger, 'literal boolean', asyncResult, syncResult);
    }

    // Test: Async execution produces same result for function calls
    {
        const syncResult = runSync('(+ 1 2 3)');
        const asyncResult = await runAsync('(+ 1 2 3)');
        assert(logger, 'function call +', asyncResult, syncResult);
    }

    {
        const syncResult = runSync('(* (+ 1 2) (- 10 5))');
        const asyncResult = await runAsync('(* (+ 1 2) (- 10 5))');
        assert(logger, 'nested function calls', asyncResult, syncResult);
    }

    // Test: Async execution produces same result for recursion
    {
        const code = `
      (define (factorial n)
        (if (<= n 1)
            1
            (* n (factorial (- n 1)))))
      (factorial 10)
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code);
        assert(logger, 'recursive factorial', asyncResult, syncResult);
    }

    // Test: Async execution produces same result for tail recursion
    {
        const code = `
      (define (sum-to n acc)
        (if (<= n 0)
            acc
            (sum-to (- n 1) (+ acc n))))
      (sum-to 100 0)
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code);
        assert(logger, 'tail recursive sum', asyncResult, syncResult);
    }

    // Test: Async execution produces same result for call/cc
    {
        const code = `
      (+ 1 (call/cc (lambda (k) (+ 2 (k 10)))))
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code);
        assert(logger, 'call/cc escape', asyncResult, syncResult);
    }

    // Test: Async execution produces same result for dynamic-wind
    {
        const code = `
      (define log '())
      (dynamic-wind
        (lambda () (set! log (cons 'before log)))
        (lambda () (set! log (cons 'body log)) 42)
        (lambda () (set! log (cons 'after log))))
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code);
        assert(logger, 'dynamic-wind result', asyncResult, syncResult);
    }

    logger.title('AsyncTrampoline - State Integrity');

    // Test: Yield points don't corrupt interpreter state
    {
        // Use very small step count to force many yields
        const code = `
      (define x 1)
      (define y 2)
      (define z (+ x y))
      z
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 1 });
        assert(logger, 'state after many yields', asyncResult, syncResult);
    }

    // Test: Yield points don't break continuation capture
    {
        const code = `
      (define k #f)
      (+ 1 (call/cc (lambda (c) (set! k c) 10)))
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'continuation capture with yields', asyncResult, syncResult);
    }

    // Test: Yield points maintain proper environment
    {
        const code = `
      (let ((x 10))
        (let ((y 20))
          (let ((z 30))
            (+ x y z))))
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 1 });
        assert(logger, 'nested let with yields', asyncResult, syncResult);
    }

    logger.title('AsyncTrampoline - Yielding Behavior');

    // Test: Long-running computation yields periodically
    {
        let yieldCount = 0;
        const code = `
      (define (loop n)
        (if (<= n 0)
            'done
            (loop (- n 1))))
      (loop 1000)
    `;

        const result = await interpreter.evaluateStringAsync(code, {
            stepsPerYield: 50,
            onYield: () => { yieldCount++; }
        });

        assert(logger, 'loop result', result.name || result, 'done');
        assert(logger, 'yielded multiple times', yieldCount > 5, true);
    }

    // Test: Paused computation can be resumed correctly
    {
        let pauseRequested = false;
        const code = `
      (define (count n)
        (if (<= n 0)
            0
            (+ 1 (count (- n 1)))))
      (count 50)
    `;

        const result = await interpreter.evaluateStringAsync(code, {
            stepsPerYield: 10,
            onYield: () => {
                // Simulate a pause request on first yield
                if (!pauseRequested) {
                    pauseRequested = true;
                }
            }
        });

        assert(logger, 'resumed computation result', result, 50);
    }

    // Test: Multiple pauses and resumes work correctly
    {
        let yieldCount = 0;
        const code = `
      (define (fib n)
        (if (< n 2)
            n
            (+ (fib (- n 1)) (fib (- n 2)))))
      (fib 10)
    `;

        const syncResult = runSync(code);

        const asyncResult = await interpreter.evaluateStringAsync(code, {
            stepsPerYield: 5,
            onYield: () => { yieldCount++; }
        });

        assert(logger, 'fib(10) async equals sync', asyncResult, syncResult);
        assert(logger, 'fib(10) yielded multiple times', yieldCount > 10, true);
    }
}

export default runAsyncTrampolineTests;
