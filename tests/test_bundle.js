import { schemeEval, schemeEvalAsync } from '../dist/scheme.js';
import { assert } from './harness/helpers.js';

/**
 * Runs integration tests for the bundled Scheme interpreter.
 * @param {object} logger - The test logger.
 */
export async function runBundleTests(logger) {
    logger.title("Bundle Tests");

    /**
     * Helper to run synchronous Scheme code.
     * @param {string} code - Scheme code.
     * @returns {*} Result.
     */
    function runSync(code) {
        return schemeEval(code);
    }

    // Test 1: Basic Math (Sync)
    try {
        const result = runSync('(+ 1 2)');
        assert(logger, "Basic Math (Sync)", result, 3);
    } catch (e) {
        logger.fail(`Basic Math (Sync) failed: ${e.message}`);
    }

    // Test 2: Basic Math (Async)
    try {
        const result = await schemeEvalAsync('(* 10 20)');
        assert(logger, "Basic Math (Async)", result, 200);
    } catch (e) {
        logger.fail(`Basic Math (Async) failed: ${e.message}`);
    }

    // Test 3: Shared Environment
    try {
        runSync('(define x 42)');
        const result = runSync('x');
        assert(logger, "Shared Environment (Define/Ref)", result, 42);
    } catch (e) {
        logger.fail(`Shared Environment (Define/Ref) failed: ${e.message}`);
    }

    // Test 4: Shared Environment (Async)
    try {
        runSync('(define y 100)');
        const result = await schemeEvalAsync('(+ x y)'); // 42 + 100
        assert(logger, "Shared Environment (Async Access)", result, 142);
    } catch (e) {
        logger.fail(`Shared Environment (Async Access) failed: ${e.message}`);
    }
}
