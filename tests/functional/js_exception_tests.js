/**
 * Tests for JS Exception Integration with Scheme Handlers
 * 
 * Tests that Scheme's exception handling can catch JavaScript exceptions 
 * thrown from primitives and callbacks.
 * 
 * Note: Uses with-exception-handler with call/cc escape pattern since
 * handlers cannot return from non-continuable exceptions per R7RS.
 * The handler must escape via continuation to return a value.
 */

import { run, assert, createTestEnv } from '../harness/helpers.js';

/**
 * Runs JS exception integration tests.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runJsExceptionTests(interpreter, logger) {
    logger.title('JS Exception Integration Tests');

    // === Basic: with-exception-handler + call/cc catches JS type error ===
    {
        const result = run(interpreter, `
            (call/cc (lambda (escape)
              (with-exception-handler
                (lambda (e) (escape "caught"))
                (lambda () (+ "not a number" 1)))))
        `);
        assert(logger, "Basic: handler catches type error via escape", result, "caught");
    }

    // === Error message accessible ===
    {
        const result = run(interpreter, `
            (call/cc (lambda (escape)
              (with-exception-handler
                (lambda (e) (escape (error-object-message e)))
                (lambda () (+ "a" 1)))))
        `);
        // Result should contain "expected number"
        const passed = typeof result === 'string' && result.includes("expected number");
        assert(logger, "Error message: accessible via error-object-message", passed, true);
    }

    // === TCO: Error in tail position caught correctly ===
    {
        const result = run(interpreter, `
            (define (loop n)
              (if (= n 0)
                  (+ "trigger error" 1)  ;; JS error in tail position
                  (loop (- n 1))))
            (call/cc (lambda (escape)
              (with-exception-handler
                (lambda (e) (escape "caught-in-tail"))
                (lambda () (loop 100)))))
        `);
        assert(logger, "TCO: Error in tail position caught", result, "caught-in-tail");
    }

    // === call/cc: Continuation captured, then error ===
    {
        const result = run(interpreter, `
            (define saved-k #f)
            (call/cc (lambda (escape)
              (with-exception-handler
                (lambda (e) (escape "error-caught"))
                (lambda ()
                  (+ 1 (call/cc (lambda (k)
                         (set! saved-k k)
                         (+ "trigger" 1))))))))  ;; Error after capturing k
        `);
        assert(logger, "call/cc: Error after capturing continuation", result, "error-caught");
    }

    // === Nested handlers: Inner catches before outer ===
    {
        const result = run(interpreter, `
            (call/cc (lambda (escape-outer)
              (with-exception-handler
                (lambda (e-outer) (escape-outer "outer"))
                (lambda ()
                  (call/cc (lambda (escape-inner)
                    (with-exception-handler
                      (lambda (e-inner) (escape-inner "inner"))
                      (lambda () (car 5)))))))))  ;; Type error
        `);
        assert(logger, "Nested: Inner handler catches first", result, "inner");
    }

    // === No handler: Error propagates to JS ===
    {
        let caught = false;
        try {
            run(interpreter, `(+ "a" 1)`);
        } catch (e) {
            caught = true;
        }
        assert(logger, "No handler: Error propagates to JS", caught, true);
    }

    // === Dynamic-wind: After thunks run before handler ===
    {
        const result = run(interpreter, `
            (define log '())
            (call/cc (lambda (escape)
              (with-exception-handler
                (lambda (e) (escape log))
                (lambda ()
                  (dynamic-wind
                    (lambda () (set! log (cons 'before log)))
                    (lambda () (+ "error" 1))
                    (lambda () (set! log (cons 'after log))))))))
        `);
        // log should be '(after before) - after runs before handler catches
        // Result is a Cons list, check the structure
        const pass = result && result.car && result.car.name === 'after' &&
            result.cdr && result.cdr.car && result.cdr.car.name === 'before';
        assert(logger, "Dynamic-wind: After thunk runs before handler", pass, true);
    }

    // === JS Interop: Error thrown from JS callback caught by Scheme ===
    {
        // Register a JS function that throws
        interpreter.globalEnv.define('js-throws', () => {
            throw new Error("boom from JS callback");
        });

        const result = run(interpreter, `
            (call/cc (lambda (escape)
              (with-exception-handler
                (lambda (e) (escape "caught-js"))
                (lambda () (js-throws)))))
        `);
        assert(logger, "JS Interop: Error from callback caught", result, "caught-js");
    }
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = {
        title: (msg) => console.log(`\n=== ${msg} ===`),
        pass: (msg) => console.log(`✅ PASS: ${msg}`),
        fail: (msg) => { console.error(`❌ FAIL: ${msg}`); process.exitCode = 1; }
    };
    runJsExceptionTests(interpreter, logger);
}
