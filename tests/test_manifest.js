/**
 * Test Manifest - Single source of truth for all test suites.
 *
 * Paths are relative to the `tests/` directory.
 * Each runner adds its environment-specific prefix:
 * - Node.js (run_all.js): "./"
 * - Browser (core/interpreter/tests.js): "../"
 */

// Unit Tests (in tests/core/interpreter/ and tests/unit/)
export const unitTests = [
    { path: 'core/interpreter/data_tests.js', fn: 'runDataTests', needsInterpreter: false },
    { path: 'core/interpreter/primitives_tests.js', fn: 'runPrimitiveTests', needsInterpreter: false },
    { path: 'core/interpreter/unit_tests.js', fn: 'runUnitTests', needsInterpreter: true },
    { path: 'core/interpreter/winders_tests.js', fn: 'runWindersTests', needsInterpreter: true },
    { path: 'core/interpreter/reader_tests.js', fn: 'runReaderTests', needsInterpreter: false },
    { path: 'core/interpreter/syntax_rules_tests.js', fn: 'runSyntaxRulesUnitTests', needsInterpreter: false },
    { path: 'core/interpreter/error_tests.js', fn: 'runErrorTests', needsInterpreter: false },
    { path: 'core/interpreter/syntax_object_tests.js', fn: 'runSyntaxObjectTests', needsInterpreter: false },
];

// Functional Tests (all need interpreter)
export const functionalTests = [
    { path: 'functional/core_tests.js', fn: 'runCoreTests', async: false },
    { path: 'extras/primitives/interop_tests.js', fn: 'runInteropTests', async: false },
    { path: 'functional/quasiquote_tests.js', fn: 'runQuasiquoteTests', async: false },
    { path: 'functional/quote_tests.js', fn: 'runQuoteTests', async: false },
    { path: 'functional/macro_tests.js', fn: 'runMacroTests', async: true },
    { path: 'functional/syntax_rules_tests.js', fn: 'runSyntaxRulesTests', async: true },
    { path: 'functional/hygiene_tests.js', fn: 'runHygieneTests', async: true },
    { path: 'functional/define_tests.js', fn: 'runDefineTests', async: false },
    { path: 'functional/eval_apply_tests.js', fn: 'runEvalApplyTests', async: true },
    { path: 'functional/record_interop_tests.js', fn: 'runRecordInteropTests', async: true, needsLoader: true },
    { path: 'functional/multiple_values_tests.js', fn: 'runMultipleValuesTests', async: false },
    { path: 'functional/exception_interop_tests.js', fn: 'runExceptionInteropTests', async: true },
    { path: 'functional/js_exception_tests.js', fn: 'runJsExceptionTests', async: true },
    { path: 'functional/char_tests.js', fn: 'runCharTests', async: false },
    { path: 'functional/string_tests.js', fn: 'runStringTests', async: false },
    { path: 'functional/vector_tests.js', fn: 'runVectorExpansionTests', async: false },
    { path: 'functional/io_tests.js', fn: 'runIOTests', async: false },
    { path: 'functional/scope_marking_tests.js', fn: 'runScopeMarkingTests', async: true },
];

// Integration Tests
export const integrationTests = [
    { path: 'integration/library_loader_tests.js', fn: 'runLibraryLoaderTests', async: true, needsInterpreter: false },
    { path: 'test_bundle.js', fn: 'runBundleTests', async: true, needsInterpreter: false },
    { path: 'functional/callable_closures_tests.js', fn: 'runCallableClosuresTests', async: true },
];

// Scheme Test Files (paths relative to project root, used by file loader)
export const schemeTestFiles = [
    'tests/test_script.scm',
    'tests/core/scheme/primitive_tests.scm',
    'tests/core/scheme/test_harness_tests.scm',
    'tests/core/scheme/boot_tests.scm',
    'tests/core/scheme/record_tests.scm',
    'tests/core/scheme/tco_tests.scm',
    'tests/core/scheme/dynamic_wind_tests.scm',
    'tests/core/scheme/dynamic_wind_interop_tests.scm',
    'tests/core/scheme/control_tests.scm',
    'tests/core/scheme/exception_tests.scm',
    'tests/core/scheme/error_tests.scm',
    'tests/core/scheme/repl_tests.scm',
    'tests/core/scheme/parameter_tests.scm',
    'tests/core/scheme/hygiene_tests.scm',
    'tests/core/scheme/case_lambda_tests.scm',
    'tests/core/scheme/lazy_tests.scm',
    'tests/core/scheme/time_tests.scm',
    'tests/core/scheme/eval_tests.scm',
    'tests/core/scheme/process_context_tests.scm',
    'tests/core/scheme/bytevector_tests.scm',
    'tests/core/scheme/features_tests.scm',
    'tests/core/scheme/iteration_tests.scm',
    'tests/core/scheme/write_tests.scm',
    'tests/core/scheme/binary_io_tests.scm',
    'tests/core/scheme/number_tests.scm',
    'tests/core/scheme/rational_tests.scm',
    'tests/core/scheme/complex_tests.scm',
    'tests/core/scheme/base_prefix_tests.scm',
    'tests/core/scheme/reader_tests.scm',
    'tests/core/scheme/reader_syntax_tests.scm',
    'tests/core/scheme/define_values_tests.scm',
    'tests/core/scheme/macro_hygiene_tests.scm',
    'tests/core/scheme/nested_macro_tests.scm',
    'tests/core/scheme/bigint_exactness_tests.scm',
    // Extension library tests
    'tests/extras/scheme/promise_tests.scm',
    'tests/extras/scheme/promise_interop_tests.scm',
    'tests/extras/scheme/jsref_tests.scm',
];

/**
 * Runs a single test module with the given path prefix.
 * @param {string} pathPrefix - Path prefix for imports (e.g., "./" or "../")
 * @param {Object} test - Test descriptor from manifest
 * @param {Object} interpreter - Interpreter instance
 * @param {Object} logger - Logger instance
 * @param {Function} loader - File loader (for tests that need it)
 */
export async function runTestModule(pathPrefix, test, interpreter, logger, loader) {
    // Skip Node.js-only tests in the browser
    const inBrowser = typeof process === 'undefined';
    if (test.nodeOnly && inBrowser) {
        logger.skip(`${test.path} (Node.js only)`);
        return;
    }

    const fullPath = pathPrefix + test.path;
    const module = await import(fullPath);
    const testFn = module[test.fn];

    if (!testFn) {
        logger.fail(`Test function ${test.fn} not found in ${fullPath}`);
        return;
    }

    const args = [];
    if (test.needsInterpreter !== false) {
        args.push(interpreter);
    }
    args.push(logger);
    if (test.needsLoader) {
        args.push(loader);
    }

    if (test.async) {
        await testFn(...args);
    } else {
        testFn(...args);
    }
}

/**
 * Runs all tests from the manifest with the given path prefix.
 * @param {string} pathPrefix - Path prefix for imports
 * @param {Object} interpreter - Interpreter instance
 * @param {Object} logger - Logger instance
 * @param {Function} loader - File loader for Scheme tests
 * @param {Function} runSchemeTests - Scheme test runner function
 */
export async function runAllFromManifest(pathPrefix, interpreter, logger, loader, runSchemeTests, loadBootstrap) {
    // Unit tests (run BEFORE core.scm is loaded to test raw analyzer)
    try {
        for (const test of unitTests) {
            await runTestModule(pathPrefix, test, interpreter, logger, loader);
        }
    } catch (e) {
        logger.fail(`Unit test suite crashed: ${e.message}`);
    }

    // Load core.scm and control.scm AFTER unit tests, BEFORE functional tests
    // This ensures unit tests test the raw interpreter, while functional tests
    // have access to Scheme-defined procedures and macros.
    if (loadBootstrap) {
        await loadBootstrap();
    }

    // Functional tests
    try {
        for (const test of functionalTests) {
            await runTestModule(pathPrefix, test, interpreter, logger, loader);
        }
    } catch (e) {
        logger.fail(`Functional test suite crashed: ${e.message}`);
    }

    // Integration tests
    try {
        for (const test of integrationTests) {
            await runTestModule(pathPrefix, test, interpreter, logger, loader);
        }
    } catch (e) {
        logger.fail(`Integration test suite crashed: ${e.message}`);
    }

    // Scheme tests
    if (loader && runSchemeTests) {
        try {
            await runSchemeTests(interpreter, logger, schemeTestFiles, loader);
        } catch (e) {
            logger.fail(`Scheme test suite crashed: ${e.message}`);
        }
    }

    // Summary
    if (logger.summary) {
        logger.summary();
    }
}
