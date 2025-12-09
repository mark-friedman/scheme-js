/**
 * Test Manifest - Single source of truth for all test suites.
 * 
 * Paths are relative to the `tests/` directory.
 * Each runner adds its environment-specific prefix:
 * - Node.js (run_all.js): "./" 
 * - Browser (runtime/tests.js): "../"
 */

// Unit Tests
export const unitTests = [
    { path: 'unit/data_tests.js', fn: 'runDataTests', needsInterpreter: false },
    { path: 'unit/primitives_tests.js', fn: 'runPrimitiveTests', needsInterpreter: false },
    { path: 'unit/unit_tests.js', fn: 'runUnitTests', needsInterpreter: true },
    { path: 'unit/winders_tests.js', fn: 'runWindersTests', needsInterpreter: true },
    { path: 'unit/analyzer_tests.js', fn: 'runAnalyzerTests', needsInterpreter: true },
    { path: 'unit/reader_tests.js', fn: 'runReaderTests', needsInterpreter: false },
    { path: 'unit/syntax_rules_tests.js', fn: 'runSyntaxRulesUnitTests', needsInterpreter: false },
];

// Functional Tests (all need interpreter)
export const functionalTests = [
    { path: 'functional/core_tests.js', fn: 'runCoreTests', async: false },
    { path: 'functional/interop_tests.js', fn: 'runInteropTests', async: false },
    { path: 'functional/quasiquote_tests.js', fn: 'runQuasiquoteTests', async: false },
    { path: 'functional/quote_tests.js', fn: 'runQuoteTests', async: false },
    { path: 'functional/macro_tests.js', fn: 'runMacroTests', async: true },
    { path: 'functional/syntax_rules_tests.js', fn: 'runSyntaxRulesTests', async: true },
    { path: 'functional/define_tests.js', fn: 'runDefineTests', async: false },
    { path: 'functional/eval_apply_tests.js', fn: 'runEvalApplyTests', async: true },
    { path: 'functional/record_interop_tests.js', fn: 'runRecordInteropTests', async: true, needsLoader: true },
    { path: 'functional/multiple_values_tests.js', fn: 'runMultipleValuesTests', async: false },
];

// Integration Tests
export const integrationTests = [
    { path: 'integration/library_loader_tests.js', fn: 'runLibraryLoaderTests', async: true, needsInterpreter: false },
];

// Scheme Test Files (paths relative to project root, used by file loader)
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
 * Runs a single test module with the given path prefix.
 * @param {string} pathPrefix - Path prefix for imports (e.g., "./" or "../")
 * @param {Object} test - Test descriptor from manifest
 * @param {Object} interpreter - Interpreter instance
 * @param {Object} logger - Logger instance
 * @param {Function} loader - File loader (for tests that need it)
 */
export async function runTestModule(pathPrefix, test, interpreter, logger, loader) {
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
export async function runAllFromManifest(pathPrefix, interpreter, logger, loader, runSchemeTests) {
    // Unit tests
    try {
        for (const test of unitTests) {
            await runTestModule(pathPrefix, test, interpreter, logger, loader);
        }
    } catch (e) {
        logger.fail(`Unit test suite crashed: ${e.message}`);
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
