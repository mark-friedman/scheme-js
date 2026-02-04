/**
 * @fileoverview Async Mode Functional Tests
 * 
 * Re-runs a subset of critical functional tests using the async execution path
 * to ensure that evaluateStringAsync produces identical results to sync execution.
 * 
 * This verifies that the async trampoline doesn't introduce any regressions
 * in existing functionality.
 */

import { assert, run, createTestLogger } from '../../harness/helpers.js';

/**
 * Runs functional tests in async mode.
 * 
 * Tests selected based on coverage of critical functionality:
 * - Core evaluation patterns
 * - TCO (tail call optimization)
 * - Continuations (call/cc)
 * - Dynamic-wind
 * - Macros and special forms
 * - Error handling
 * 
 * @param {Interpreter} interpreter - The bootstrapped interpreter
 * @param {Object} logger - Test logger
 */
export async function runAsyncModeFunctionalTests(interpreter, logger) {
    const runSync = (code) => run(interpreter, code);
    const runAsync = (code, options = {}) => interpreter.evaluateStringAsync(code, options);

    logger.title('Async Mode - Core Evaluation');

    // Basic arithmetic
    {
        const code = '(+ 1 2 3 4 5)';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'arithmetic', asyncResult, syncResult);
    }

    // Nested expressions
    {
        const code = '(* (+ 1 2) (- 10 5) (/ 20 4))';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 1 });
        assert(logger, 'nested expressions', asyncResult, syncResult);
    }

    // let bindings
    {
        const code = '(let ((x 10) (y 20)) (+ x y))';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'let bindings', asyncResult, syncResult);
    }

    // let* bindings
    {
        const code = '(let* ((x 10) (y (+ x 5))) (* x y))';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'let* bindings', asyncResult, syncResult);
    }

    // letrec bindings
    {
        const code = `
      (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
               (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
        (even? 10))
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 3 });
        assert(logger, 'letrec mutual recursion', asyncResult, syncResult);
    }

    logger.title('Async Mode - Control Flow');

    // if expression
    {
        const code = '(if (> 5 3) "yes" "no")';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'if true branch', asyncResult, syncResult);
    }

    // cond expression
    {
        const code = `
      (cond
        ((< 5 3) "a")
        ((= 5 3) "b")
        (else "c"))
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'cond else branch', asyncResult, syncResult);
    }

    // case expression
    {
        const code = `
      (case (+ 1 2)
        ((1 2) "one-or-two")
        ((3 4) "three-or-four")
        (else "other"))
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'case expression', asyncResult, syncResult);
    }

    // and/or
    {
        const code = '(and (> 5 3) (< 2 10) "success")';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 1 });
        assert(logger, 'and expression', asyncResult, syncResult);
    }

    {
        const code = '(or #f #f "fallback")';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 1 });
        assert(logger, 'or expression', asyncResult, syncResult);
    }

    logger.title('Async Mode - TCO');

    // Tail-recursive loop
    {
        const code = `
      (define (sum-to n acc)
        (if (<= n 0) acc (sum-to (- n 1) (+ acc n))))
      (sum-to 1000 0)
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 50 });
        assert(logger, 'tail-recursive sum', asyncResult, syncResult);
    }

    // Deep tail recursion
    {
        const code = `
      (define (count-down n)
        (if (<= n 0) 'done (count-down (- n 1))))
      (count-down 10000)
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 100 });
        assert(logger, 'deep tail recursion', asyncResult.name || asyncResult,
            syncResult.name || syncResult);
    }

    logger.title('Async Mode - Continuations');

    // call/cc basic escape
    {
        const code = '(+ 1 (call/cc (lambda (k) (+ 2 (k 10)))))';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'call/cc escape', asyncResult, syncResult);
    }

    // call/cc captured for later
    {
        const code = `
      (define saved-k #f)
      (define counter 0)
      (+ 100 (call/cc (lambda (k) (set! saved-k k) 5)))
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'call/cc capture', asyncResult, syncResult);
    }

    logger.title('Async Mode - Dynamic-wind');

    // Basic dynamic-wind
    {
        const code = `
      (define log '())
      (dynamic-wind
        (lambda () (set! log (cons 'before log)))
        (lambda () (set! log (cons 'body log)) 42)
        (lambda () (set! log (cons 'after log))))
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'dynamic-wind result', asyncResult, syncResult);
    }

    // Dynamic-wind with escape
    {
        const code = `
      (define log '())
      (call/cc (lambda (escape)
        (dynamic-wind
          (lambda () (set! log (cons 'before log)))
          (lambda () (escape 99))
          (lambda () (set! log (cons 'after log))))))
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'dynamic-wind escape', asyncResult, syncResult);
    }

    logger.title('Async Mode - Macros');

    // when macro
    {
        const code = '(when (> 5 3) "yes")';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'when macro', asyncResult, syncResult);
    }

    // unless macro
    {
        const code = '(unless (< 5 3) "yes")';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'unless macro', asyncResult, syncResult);
    }

    // do loop
    {
        const code = `
      (do ((i 0 (+ i 1))
           (sum 0 (+ sum i)))
          ((>= i 10) sum))
    `;
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 3 });
        assert(logger, 'do loop', asyncResult, syncResult);
    }

    logger.title('Async Mode - Data Structures');

    // List operations
    {
        const code = '(map (lambda (x) (* x 2)) (list 1 2 3 4 5))';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'map over list', asyncResult, syncResult);
    }

    // append
    {
        const code = '(append (list 1 2) (list 3 4 5))';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'append', asyncResult, syncResult);
    }

    // length
    {
        const code = '(length (list 1 2 3 4 5))';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'length', asyncResult, syncResult);
    }

    // Vector operations
    {
        const code = '(vector-ref (vector 10 20 30) 1)';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'vector-ref', asyncResult, syncResult);
    }

    logger.title('Async Mode - Multiple Values');

    // values and call-with-values
    {
        const code = '(call-with-values (lambda () (values 1 2 3)) +)';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'call-with-values', asyncResult, syncResult);
    }

    // let-values
    {
        const code = '(let-values (((a b) (values 10 20))) (+ a b))';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'let-values', asyncResult, syncResult);
    }

    logger.title('Async Mode - Quasiquotation');

    // quasiquote with unquote
    {
        const code = '(let ((x 42)) `(the answer is ,x))';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'quasiquote unquote', asyncResult, syncResult);
    }

    // quasiquote with unquote-splicing
    {
        const code = '`(1 2 ,@(list 3 4) 5)';
        const syncResult = runSync(code);
        const asyncResult = await runAsync(code, { stepsPerYield: 2 });
        assert(logger, 'quasiquote unquote-splicing', asyncResult, syncResult);
    }

    logger.title('Async Mode - Yield Stress Test');

    // Many yields in complex computation
    {
        let yieldCount = 0;
        const code = `
      (define (complex-compute n)
        (if (<= n 0)
            0
            (+ n (complex-compute (- n 1)))))
      (complex-compute 200)
    `;
        const syncResult = runSync(code);
        const asyncResult = await interpreter.evaluateStringAsync(code, {
            stepsPerYield: 1,  // Yield every single step
            onYield: () => { yieldCount++; }
        });
        assert(logger, 'complex compute with max yields', asyncResult, syncResult);
        assert(logger, 'yielded many times', yieldCount > 100, true);
    }
}

export default runAsyncModeFunctionalTests;
