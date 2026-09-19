import { assert, run, createTestLogger, createTestEnv } from '../harness/helpers.js';
import { SchemeApplicationError, SchemeUnboundError } from '../../src/core/interpreter/errors.js';

/**
 * Core functional tests for the Scheme interpreter.
 * Tests basic evaluation, TCO, runtime errors, and edge cases.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runCoreTests(interpreter, logger) {

    // --- Basic Evaluation & Native Calls ---
    logger.title("Basic Evaluation & Native Calls");

    let result = run(interpreter, `(+ 2 3)`);
    assert(logger, "Native '+' call", result, 5);

    result = run(interpreter, `(let ((x 10)) (* x 2))`);
    assert(logger, "'let' binding", result, 20);

    result = run(interpreter, `(if (> 10 5) "yes" "no")`);
    assert(logger, "'if' expression (true)", result, 'yes');

    // --- TCO Tests ---
    logger.title("Tail Call Optimization (TCO)");

    const tcoTest = `
        (letrec ((loop (lambda (n acc)
                         (if (= n 0)
                             acc
                             (loop (- n 1) (+ acc n))))))
          (loop 5 0))`;

    result = run(interpreter, tcoTest);
    assert(logger, "TCO sum (1-5)", result, 15);

    const tcoLargeTest = `
        (letrec ((loop (lambda (n acc)
                         (if (= n 0)
                             acc
                             (loop (- n 1) (+ acc n))))))
          (loop 2000000 0))`;

    try {
        result = run(interpreter, tcoLargeTest);
        assert(logger, "TCO deep recursion (n=2000000)", result, 2000001000000);
    } catch (e) {
        logger.fail(`TCO deep recursion (n=2000000) failed: ${e.message}`);
    }

    // --- Runtime Error Tests ---
    logger.title("Runtime Error Tests");

    try {
        run(interpreter, `(1 2)`);
        logger.fail("Runtime: Apply non-function - FAILED to throw");
    } catch (e) {
        // Check error type instead of message string
        assert(logger, "Runtime: Apply non-function", e instanceof SchemeApplicationError, true);
    }

    try {
        run(interpreter, `(undefined-var)`);
        logger.fail("Runtime: Unbound variable - FAILED to throw");
    } catch (e) {
        // Check error type instead of message string
        assert(logger, "Runtime: Unbound variable", e instanceof SchemeUnboundError, true);
    }

    // --- Edge Case Tests ---
    logger.title("Edge Case Tests");

    result = run(interpreter, `(begin)`);
    assert(logger, "Edge: Empty begin", result, null);

    result = run(interpreter, `(if #t 1 2)`);
    assert(logger, "Edge: If #t", result, 1);

    result = run(interpreter, `(if #f 1 2)`);
    assert(logger, "Edge: If #f", result, 2);

    // set! has unspecified return value per R7RS
    result = run(interpreter, `(let ((x 1)) (set! x 2))`);
    assert(logger, "Edge: set! return value", result, undefined);

    // Reading a letrec variable from its own initializer is an error in R7RS
    // 4.2.2, so any answer is permitted; this pins ours so it cannot drift
    // silently. It is now the unspecified value, as `set!` returns. It used to
    // be the *symbol* `undefined`, an artifact of the `letrec` macro binding
    // each variable to `'undefined` -- a value a program could mistake for
    // data. `letrec` is a core form now and the placeholder is a real
    // unspecified value.
    result = run(interpreter, `(letrec ((x x)) x)`);
    assert(logger, "Edge: letrec self-reference is unspecified", result, undefined);
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runCoreTests(interpreter, logger);
}
