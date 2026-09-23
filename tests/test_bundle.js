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

    // A library loaded after start-up must import the compiled standard library,
    // not the interpreted closures the compiled code replaced. Before that was
    // fixed, the library's `map` was not even `eq?` to the user's.
    try {
        runSync(`(define-library (bundle-probe) (import (scheme base))
                   (export probe-map probe-equal)
                   (begin (define probe-map map) (define probe-equal equal?)))`);
        runSync('(import (bundle-probe))');
        assert(logger, "A later library imports the compiled standard library",
            runSync('(and (eq? probe-map map) (eq? probe-equal equal?))'), true);
    } catch (e) {
        logger.fail(`Later library imports failed: ${e.message}`);
    }

    // SRFI 125 from the bundle. An `equal?` table is the case that depends on
    // the library recognising the user's `equal?`.
    try {
        runSync('(import (srfi 125))');
        const result = runSync(`
          (let ((ht (make-hash-table equal?)))
            (hash-table-set! ht (list 1 2) 'found)
            (hash-table-update!/default ht "count" (lambda (n) (+ n 1)) 41)
            (list (hash-table-ref/default ht (list 1 2) #f)
                  (hash-table-ref ht "count")))`);
        assert(logger, "SRFI 125 hash tables from the bundle", result, ['found', 42]);
    } catch (e) {
        logger.fail(`SRFI 125 from the bundle failed: ${e.message}`);
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
