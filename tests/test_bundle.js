import {
    schemeEval, schemeEvalAsync, loadCompiler, isCompilerLoaded, libraryInstallation,
    setUserCodeCompilation, env, interpreter, parse, analyze,
    callSchemeProcedure, schemeToJs, schemeToJsDeep, jsToScheme, jsToSchemeDeep
} from '../dist/scheme.js';
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
        // Imported after start-up, so its prebuilt table is installed by the
        // library-load hook as it loads; interpreted, every lookup costs ~17x.
        assert(logger, "A library imported after start-up is compiled",
            [runSync('hash-table-ref/default').$compiled, runSync('make-hash-table').$compiled],
            [true, true]);
    } catch (e) {
        logger.fail(`SRFI 125 from the bundle failed: ${e.message}`);
    }

    // The bundle does not carry the compiler. Every library it ships was
    // compiled at build time and installed as it loaded, so nothing so far --
    // start-up, SRFI 125 -- has needed it.
    try {
        const outcomes = [...libraryInstallation.values()];
        assert(logger, "Every shipped library loaded so far installed its whole table",
            outcomes.filter((o) => o.stale || o.skipped.length > 0).length, 0);
        assert(logger, "The standard library was installed from its table",
            libraryInstallation.get('scheme core').installed.length > 20, true);
    } catch (e) {
        logger.fail(`Prebuilt libraries in the bundle failed: ${e.message}`);
    }

    // The bundle loads the compiler itself once it has started, as a file of
    // its own, and from then on compiles the page's own procedures.
    try {
        const compiler = await loadCompiler();
        assert(logger, "The bundle loads the compiler", isCompilerLoaded(), true);
        assert(logger, "And attaches the compiler tier to the page's interpreter", interpreter.tier !== null, true);
        runSync('(define (bundle-loop n) (let loop ((i 0) (acc 0)) (if (= i n) acc (loop (+ i 1) (+ acc i)))))');
        assert(logger, "A page's procedure that loops is compiled when defined",
            [runSync('bundle-loop').$compiled === true, interpreter.tier.outcomes.get('bundle-loop')], [true, 'compiled']);
        assert(logger, "And answers", runSync('(bundle-loop 10)'), 45);
        setUserCodeCompilation(false);
        runSync('(define (bundle-off n) (let loop ((i 0)) (if (= i n) i (loop (+ i 1)))))');
        assert(logger, "Turned off, the page's procedures stay interpreted", runSync('bundle-off').$compiled === true, false);
        setUserCodeCompilation(true);
        assert(logger, "Turned on again, the tier attaches", interpreter.tier !== null, true);
        const asts = parse('(define (bundle-square x) (* x x))').map((form) => analyze(form));
        const outcome = compiler.compileProgram(asts, env, interpreter);
        assert(logger, "The loaded compiler compiles a definition", outcome.compiled, ['bundle-square']);
        assert(logger, "And the compiled procedure runs",
            [runSync('(bundle-square 7)'), runSync('bundle-square').$compiled], [49, true]);
        assert(logger, "Loading it again returns the same module", await loadCompiler(), compiler);
    } catch (e) {
        logger.fail(`Loading the compiler failed: ${e.message}`);
    }

    // What the split is for: the compiler's code is in its own file, not in the
    // one every page loads. Read from disk, so Node only.
    if (typeof process !== 'undefined') {
        try {
            const fs = await import('fs');
            const read = (file) => fs.readFileSync(new URL(`../dist/${file}`, import.meta.url), 'utf8');
            assert(logger, "The bundle every page loads does not contain the compiler",
                read('scheme.js').includes('"generate-unit"'), false);
            assert(logger, "The compiler's own file does",
                read('scheme_compiler.js').includes('"generate-unit"'), true);
        } catch (e) {
            logger.fail(`Reading the bundle failed: ${e.message}`);
        }
    }

    // JavaScript calling Scheme through the bundle's public interop: the parts
    // of a plain call, which convert, and the call that converts nothing.
    try {
        runSync('(define (bundle-describe x) (vector (exact? x) (+ x 1)))');
        const describe = runSync('bundle-describe');
        const result = callSchemeProcedure(describe, [jsToScheme(41)]);
        assert(logger, "The call that converts nothing gives a Scheme value",
            [result[0], typeof result[1]], [true, 'bigint']);
        assert(logger, "The conversions convert in each direction, shallow and deep",
            [schemeToJs(42n), jsToScheme(1), schemeToJsDeep([1n, [2n]]), jsToSchemeDeep([1, [2]])],
            [42, 1n, [1, [2]], [1n, [2n]]]);
        assert(logger, "A plain call is the conversions around the call that converts nothing",
            JSON.stringify(describe(41)) === JSON.stringify(
                schemeToJsDeep(callSchemeProcedure(describe, [41].map(jsToScheme)))), true);
    } catch (e) {
        logger.fail(`The bundle's public interop failed: ${e.message}`);
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
