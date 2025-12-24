/**
 * Vector Expansion Tests
 * 
 * Tests R7RS §6.8 vector operations beyond the basics.
 */

import { assert, run, createTestLogger, createTestEnv } from '../harness/helpers.js';

/**
 * Runs vector expansion tests.
 * @param {Interpreter} interpreter
 * @param {object} logger
 */
export function runVectorExpansionTests(interpreter, logger) {
    logger.title("Vector Tests - Fill");

    let result = run(interpreter, `
        (let ((v (vector 1 2 3 4 5)))
          (vector-fill! v 0)
          v)`);
    assert(logger, "vector-fill! all zeros", JSON.stringify(result), JSON.stringify([0, 0, 0, 0, 0]));

    result = run(interpreter, `
        (let ((v (vector 1 2 3 4 5)))
          (vector-fill! v 9 1 4)
          v)`);
    assert(logger, "vector-fill! with range", JSON.stringify(result), JSON.stringify([1, 9, 9, 9, 5]));

    result = run(interpreter, `
        (let ((v (vector 1 2 3)))
          (vector-fill! v 0 0 0)
          v)`);
    assert(logger, "vector-fill! empty range", JSON.stringify(result), JSON.stringify([1, 2, 3]));

    // -------------------------------------------------------------------------
    logger.title("Vector Tests - Copy");

    result = run(interpreter, `(vector-copy (vector 1 2 3 4 5))`);
    assert(logger, "vector-copy full", JSON.stringify(result), JSON.stringify([1, 2, 3, 4, 5]));

    result = run(interpreter, `(vector-copy (vector 1 2 3 4 5) 1 4)`);
    assert(logger, "vector-copy with range", JSON.stringify(result), JSON.stringify([2, 3, 4]));

    result = run(interpreter, `(vector-copy (vector 1 2 3) 0 0)`);
    assert(logger, "vector-copy empty range", JSON.stringify(result), JSON.stringify([]));

    // Test that copy creates a new vector
    result = run(interpreter, `
        (let ((v1 (vector 1 2 3)))
          (let ((v2 (vector-copy v1)))
            (vector-set! v2 0 99)
            (vector-ref v1 0)))`);
    assert(logger, "vector-copy is independent", result, 1);

    // -------------------------------------------------------------------------
    logger.title("Vector Tests - Copy!");

    result = run(interpreter, `
        (let ((to (vector 1 2 3 4 5))
              (from (vector 10 20 30)))
          (vector-copy! to 1 from)
          to)`);
    assert(logger, "vector-copy! basic", JSON.stringify(result), JSON.stringify([1, 10, 20, 30, 5]));

    result = run(interpreter, `
        (let ((to (vector 1 2 3 4 5))
              (from (vector 10 20 30 40 50)))
          (vector-copy! to 0 from 2 4)
          to)`);
    assert(logger, "vector-copy! with source range", JSON.stringify(result), JSON.stringify([30, 40, 3, 4, 5]));

    // Overlapping copy (forward)
    result = run(interpreter, `
        (let ((v (vector 1 2 3 4 5)))
          (vector-copy! v 0 v 1 4)
          v)`);
    assert(logger, "vector-copy! overlap forward", JSON.stringify(result), JSON.stringify([2, 3, 4, 4, 5]));

    // Overlapping copy (backward - tricky case)
    result = run(interpreter, `
        (let ((v (vector 1 2 3 4 5)))
          (vector-copy! v 2 v 0 3)
          v)`);
    assert(logger, "vector-copy! overlap backward", JSON.stringify(result), JSON.stringify([1, 2, 1, 2, 3]));

    // -------------------------------------------------------------------------
    logger.title("Vector Tests - Append");

    result = run(interpreter, `(vector-append (vector 1 2) (vector 3 4))`);
    assert(logger, "vector-append two", JSON.stringify(result), JSON.stringify([1, 2, 3, 4]));

    result = run(interpreter, `(vector-append (vector 1) (vector 2) (vector 3))`);
    assert(logger, "vector-append three", JSON.stringify(result), JSON.stringify([1, 2, 3]));

    result = run(interpreter, `(vector-append)`);
    assert(logger, "vector-append empty", JSON.stringify(result), JSON.stringify([]));

    result = run(interpreter, `(vector-append (vector 1 2 3))`);
    assert(logger, "vector-append single", JSON.stringify(result), JSON.stringify([1, 2, 3]));

    // -------------------------------------------------------------------------
    logger.title("Vector Tests - String Conversion");

    result = run(interpreter, `(vector->string (vector #\\h #\\i))`);
    assert(logger, "vector->string", result, 'hi');

    result = run(interpreter, `(vector->string (vector #\\a #\\b #\\c) 1 3)`);
    assert(logger, "vector->string with range", result, 'bc');

    result = run(interpreter, `(string->vector "hello")`);
    assert(logger, "string->vector", JSON.stringify(result), JSON.stringify(['h', 'e', 'l', 'l', 'o']));

    result = run(interpreter, `(string->vector "hello" 1 4)`);
    assert(logger, "string->vector with range", JSON.stringify(result), JSON.stringify(['e', 'l', 'l']));

    // -------------------------------------------------------------------------
    logger.title("Vector Tests - List Conversion with Range");

    result = run(interpreter, `(length (vector->list (vector 1 2 3 4 5) 1 4))`);
    assert(logger, "vector->list with range length", result, 3);

    result = run(interpreter, `(car (vector->list (vector 1 2 3 4 5) 2))`);
    assert(logger, "vector->list from index", result, 3);

    // -------------------------------------------------------------------------
    logger.title("Vector Tests - Error Cases");

    try {
        run(interpreter, `(vector-copy! (vector 1 2) 0 (vector 1 2 3 4 5))`);
        logger.fail("vector-copy! overflow - should throw");
    } catch (e) {
        assert(logger, "vector-copy! overflow check", e.message.includes("range"), true);
    }

    try {
        run(interpreter, `(vector->string (vector 1 2 3))`);
        logger.fail("vector->string non-char - should throw");
    } catch (e) {
        assert(logger, "vector->string type check", e.message.includes("character"), true);
    }

    try {
        run(interpreter, `(vector-fill! (vector 1 2 3) 0 5)`);
        logger.fail("vector-fill! out of bounds - should throw");
    } catch (e) {
        assert(logger, "vector-fill! bounds check", e.message.includes("range") || e.message.includes("start"), true);
    }
}

// Allow running directly via node
if (typeof process !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
    const { interpreter } = createTestEnv();
    const logger = createTestLogger();
    runVectorExpansionTests(interpreter, logger);
}
