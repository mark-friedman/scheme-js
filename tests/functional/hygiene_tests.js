import { run, assert } from '../harness/helpers.js';
import { globalMacroRegistry } from '../../src/core/interpreter/macro_registry.js';

/**
 * Tests for hygienic macro expansion.
 * Verifies that macro-introduced bindings don't capture user variables.
 * Note: Uses pure marks hygiene (no gensyms), so no counter reset needed.
 */
export async function runHygieneTests(interpreter, logger) {
  logger.title('Macro Hygiene Tests');

  try {

    // =================================================================
    // Test 1: Basic Hygiene - swap! macro with user's temp variable
    // =================================================================
    run(interpreter, `
            (define-syntax swap!
              (syntax-rules ()
                ((swap! a b)
                 (let ((temp a))
                   (set! a b)
                   (set! b temp)))))
        `);

    // Define temp and other as global variables
    run(interpreter, `(define temp 5)`);
    run(interpreter, `(define other 10)`);

    // Swap them - the macro's internal 'temp' should NOT capture user's 'temp'
    run(interpreter, `(swap! temp other)`);

    const temp = run(interpreter, `temp`);
    const other = run(interpreter, `other`);

    assert(logger, 'Hygiene: swap! temp value', temp, 10);
    assert(logger, 'Hygiene: swap! other value', other, 5);

    // =================================================================
    // Test 2: my-or macro - classic hygiene test
    // =================================================================
    run(interpreter, `
            (define-syntax my-or
              (syntax-rules ()
                ((my-or) #f)
                ((my-or a) a)
                ((my-or a b ...)
                 (let ((t a))
                   (if t t (my-or b ...))))))
        `);

    // User's variable 't' should NOT be captured by macro's 't'
    run(interpreter, `(define t 'user-t)`);

    // (my-or #f t) should return user's 't value ('user-t), not #f
    const orResult = run(interpreter, `(my-or #f t)`);
    assert(logger, 'Hygiene: my-or with shadowed t', orResult.name, 'user-t');

    // =================================================================
    // Test 3: Nested let bindings
    // =================================================================
    run(interpreter, `
            (define-syntax with-temp
              (syntax-rules ()
                ((with-temp body ...)
                 (let ((x 100))
                   body ...))))
        `);

    // User's 'x' should not be shadowed by macro's 'x'
    run(interpreter, `(define x 42)`);
    const outerX = run(interpreter, `(with-temp x)`);
    assert(logger, 'Hygiene: nested let outer x visible', outerX, 42);

    // =================================================================
    // Test 4: Lambda parameters hygiene
    // =================================================================
    run(interpreter, `
            (define-syntax make-adder
              (syntax-rules ()
                ((make-adder n)
                 (lambda (x) (+ x n)))))
        `);

    run(interpreter, `(define x 1000)`);
    const addResult = run(interpreter, `((make-adder 5) 10)`);
    assert(logger, 'Hygiene: lambda param x', addResult, 15);

    // =================================================================
    // Test 5: Multiple expansions get different gensyms
    // =================================================================
    run(interpreter, `
            (define-syntax inc-temp
              (syntax-rules ()
                ((inc-temp val)
                 (let ((temp val))
                   (+ temp 1)))))
        `);

    // Two expansions should work independently
    const inc1 = run(interpreter, `(inc-temp 10)`);
    const inc2 = run(interpreter, `(inc-temp 20)`);
    assert(logger, 'Hygiene: multiple expansions 1', inc1, 11);
    assert(logger, 'Hygiene: multiple expansions 2', inc2, 21);

  } catch (e) {
    logger.fail(`Hygiene tests crashed: ${e.message}`);
    console.error(e);
  }
}
