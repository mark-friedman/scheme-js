/**
 * Test Manifest - Single source of truth for all test suites.
 * 
 * Both Node.js (run_all.js) and browser (runtime/tests.js) runners
 * use this manifest to ensure they run the same tests.
 * 
 * NOTE: All paths are absolute from server root for browser compatibility.
 */

// Unit Tests (no interpreter needed for some)
export const unitTests = [
    { path: '/tests/unit/data_tests.js', fn: 'runDataTests', needsInterpreter: false },
    { path: '/tests/unit/primitives_tests.js', fn: 'runPrimitiveTests', needsInterpreter: false },
    { path: '/tests/unit/unit_tests.js', fn: 'runUnitTests', needsInterpreter: true },
    { path: '/tests/unit/winders_tests.js', fn: 'runWindersTests', needsInterpreter: true },
    { path: '/tests/unit/analyzer_tests.js', fn: 'runAnalyzerTests', needsInterpreter: true },
    { path: '/tests/unit/reader_tests.js', fn: 'runReaderTests', needsInterpreter: false },
    { path: '/tests/unit/syntax_rules_tests.js', fn: 'runSyntaxRulesUnitTests', needsInterpreter: false },
];

// Functional Tests (all need interpreter)
export const functionalTests = [
    { path: '/tests/functional/functional_tests.js', fn: 'runFunctionalTests', async: true },
    { path: '/tests/functional/interop_tests.js', fn: 'runInteropTests', async: false },
    { path: '/tests/functional/quasiquote_tests.js', fn: 'runQuasiquoteTests', async: false },
    { path: '/tests/functional/quote_tests.js', fn: 'runQuoteTests', async: false },
    { path: '/tests/functional/macro_tests.js', fn: 'runMacroTests', async: true },
    { path: '/tests/functional/syntax_rules_tests.js', fn: 'runSyntaxRulesTests', async: true },
    { path: '/tests/functional/define_tests.js', fn: 'runDefineTests', async: false },
    { path: '/tests/functional/eval_apply_tests.js', fn: 'runEvalApplyTests', async: true },
    { path: '/tests/functional/record_interop_tests.js', fn: 'runRecordInteropTests', async: true, needsLoader: true },
];

// Integration Tests
export const integrationTests = [
    { path: '/tests/integration/library_loader_tests.js', fn: 'runLibraryLoaderTests', async: true, needsInterpreter: false },
];

// Scheme Test Files (run via runSchemeTests)
export const schemeTestFiles = [
    'tests/runtime/scheme/primitive_tests.scm',
    'tests/scheme/test_harness_tests.scm',
    'tests/runtime/scheme/boot_tests.scm',
    'tests/runtime/scheme/record_tests.scm',
    'tests/runtime/scheme/tco_tests.scm',
    'tests/runtime/scheme/dynamic_wind_tests.scm',
    'tests/runtime/scheme/dynamic_wind_interop_tests.scm',
];

/**
 * Runs a single test module.
 * @param {Object} test - Test descriptor from manifest
 * @param {Object} interpreter - Interpreter instance
 * @param {Object} logger - Logger instance
 * @param {Function} loader - File loader (for tests that need it)
 */
export async function runTestModule(test, interpreter, logger, loader) {
    const module = await import(test.path);
    const testFn = module[test.fn];

    if (!testFn) {
        logger.fail(`Test function ${test.fn} not found in ${test.path}`);
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
 * Runs all tests from the manifest.
 * @param {Object} interpreter - Interpreter instance
 * @param {Object} logger - Logger instance  
 * @param {Function} loader - File loader for Scheme tests
 * @param {Function} runSchemeTests - Scheme test runner function
 */
export async function runAllFromManifest(interpreter, logger, loader, runSchemeTests) {
    // Unit tests
    try {
        for (const test of unitTests) {
            await runTestModule(test, interpreter, logger, loader);
        }
    } catch (e) {
        logger.fail(`Unit test suite crashed: ${e.message}`);
    }

    // Functional tests
    try {
        for (const test of functionalTests) {
            await runTestModule(test, interpreter, logger, loader);
        }
    } catch (e) {
        logger.fail(`Functional test suite crashed: ${e.message}`);
    }

    // Integration tests
    try {
        for (const test of integrationTests) {
            await runTestModule(test, interpreter, logger, loader);
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
