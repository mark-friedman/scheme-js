import { assert, run, createTestLogger, createTestEnv } from '../helpers.js';

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
        assert(logger, "Runtime: Apply non-function", e.message.includes("Not a function"), true);
    }

    try {
        run(interpreter, `(undefined-var)`);
        logger.fail("Runtime: Unbound variable - FAILED to throw");
    } catch (e) {
        assert(logger, "Runtime: Unbound variable", e.message.includes("Unbound variable"), true);
    }

    // --- Edge Case Tests ---
    logger.title("Edge Case Tests");

    result = run(interpreter, `(begin)`);
    assert(logger, "Edge: Empty begin", result, null);

    result = run(interpreter, `(if #t 1 2)`);
    assert(logger, "Edge: If #t", result, 1);

    result = run(interpreter, `(if #f 1 2)`);
    assert(logger, "Edge: If #f", result, 2);

    // set! returns the value set
    result = run(interpreter, `(let ((x 1)) (set! x 2))`);
    assert(logger, "Edge: set! return value", result, 2);

    // letrec self-reference: macro initializes to 'undefined symbol
    result = run(interpreter, `(letrec ((x x)) x)`);
    // After core.scm macro expansion, the placeholder is the symbol 'undefined
    assert(logger, "Edge: letrec self-reference", result && result.name, 'undefined');
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runCoreTests(interpreter, logger);
}
