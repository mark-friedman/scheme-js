/**
 * Exception Interop Tests
 * 
 * Tests for exception handling across JS/Scheme boundaries:
 * - JS errors propagating to JS callers
 * - Scheme exceptions propagating to JS
 * - Exceptions in JS callbacks
 * - dynamic-wind + JS + exceptions
 * - call/cc + JS + exceptions
 */

import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { SchemeError } from '../../src/core/interpreter/errors.js';
import { assert } from '../helpers.js';

export async function runExceptionInteropTests(interpreter, logger) {
    logger.title('Running Exception Interop Tests...');

    const env = interpreter.globalEnv;

    // Helper to run Scheme code
    const run = (code) => interpreter.run(analyze(parse(code)[0]), env);

    // === Scheme → JS Exception Propagation ===

    // Scheme raise propagates to JS
    {
        let caught = null;
        try {
            run(`(raise 'scheme-exception)`);
        } catch (e) {
            caught = e;
        }
        const pass = caught !== null && caught.message.includes('scheme-exception');
        assert(logger, 'Scheme raise propagates to JS', pass, true);
    }

    // SchemeError accessible in JS
    {
        let caughtError = null;
        try {
            run(`(error "test error" 1 2 3)`);
        } catch (e) {
            caughtError = e;
        }
        const pass = caughtError instanceof SchemeError &&
            caughtError.irritants.length === 3;
        assert(logger, 'SchemeError.irritants accessible in JS', pass, true);
    }

    // JS Error propagates to JS caller
    {
        env.define('throw-js-error', () => { throw new Error('JS boom!'); });
        let caught = null;
        try {
            run(`(throw-js-error)`);
        } catch (e) {
            caught = e;
        }
        assert(logger, 'JS Error propagates to JS caller', caught && caught.message, 'JS boom!');
    }

    // === raise-continuable semantics ===

    // Handler return value used with raise-continuable
    {
        const result = run(`
            (with-exception-handler
                (lambda (e) 42)
                (lambda () (+ 1 (raise-continuable 'ignored))))
        `);
        assert(logger, 'raise-continuable uses handler return', result, 43);
    }

    // Handler mutation works with raise (non-continuable)
    {
        const result = run(`
            (let ((was-handled #f))
                (with-exception-handler
                    (lambda (e) (set! was-handled #t))
                    (lambda () (raise 'test-error)))
                was-handled)
        `);
        // Handler IS called, mutation happens
        // Since raise is non-continuable, the handler returning normally re-raises
        // But the mutation side-effect already happened
        assert(logger, 'raise handler is called (mutation visible)', result, true);
    }

    // === dynamic-wind + exceptions ===

    // dynamic-wind after runs when raise-continuable crosses wind
    {
        const result = run(`
            (let ((log '()))
                (with-exception-handler
                    (lambda (e) 'handler-done)
                    (lambda ()
                        (dynamic-wind
                            (lambda () (set! log (cons 'before log)))
                            (lambda () (raise-continuable 'test))
                            (lambda () (set! log (cons 'after log))))))
                log)
        `);
        // With raise-continuable, handler returns and execution continues
        // Since handler returns 'handler-done, the result of raise-continuable is 'handler-done
        // But log should still have (after before) from dynamic-wind
        const pass = result && result.car && result.car.name === 'after';
        assert(logger, 'dynamic-wind after runs on raise-continuable', pass, true);
    }

    // === call/cc + Exceptions ===

    // call/cc escape before raise
    {
        const result = run(`
            (call/cc
                (lambda (escape)
                    (with-exception-handler
                        (lambda (e) 'handler-called)
                        (lambda ()
                            (escape 'escaped)
                            (raise 'should-not-reach)))))
        `);
        const pass = result && result.name === 'escaped';
        assert(logger, 'call/cc escape before raise', pass, true);
    }

    // raise-continuable returns value  
    {
        const result = run(`
            (+ 5 (with-exception-handler
                   (lambda (e) 100)
                   (lambda () (raise-continuable 'ignored))))
        `);
        assert(logger, 'arithmetic with raise-continuable', result, 105);
    }
}
