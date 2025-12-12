/**
 * Test Manifest - Single source of truth for all test suites.
 * 
 * Paths are relative to the `tests/` directory.
 * Each runner adds its environment-specific prefix:
 * - Node.js (run_all.js): "./" 
 * - Browser (core/interpreter/tests.js): "../"
 */

// Unit Tests (in tests/core/interpreter/)
export const unitTests = [
    { path: 'core/interpreter/data_tests.js', fn: 'runDataTests', needsInterpreter: false },
    { path: 'core/interpreter/primitives_tests.js', fn: 'runPrimitiveTests', needsInterpreter: false },
    { path: 'core/interpreter/unit_tests.js', fn: 'runUnitTests', needsInterpreter: true },
    { path: 'core/interpreter/winders_tests.js', fn: 'runWindersTests', needsInterpreter: true },
    { path: 'core/interpreter/analyzer_tests.js', fn: 'runAnalyzerTests', needsInterpreter: true },
    { path: 'core/interpreter/reader_tests.js', fn: 'runReaderTests', needsInterpreter: false },
    { path: 'core/interpreter/syntax_rules_tests.js', fn: 'runSyntaxRulesUnitTests', needsInterpreter: false },
];

// Functional Tests (all need interpreter)
export const functionalTests = [
    { path: 'functional/core_tests.js', fn: 'runCoreTests', async: false },
    { path: 'functional/interop_tests.js', fn: 'runInteropTests', async: false },
    { path: 'functional/quasiquote_tests.js', fn: 'runQuasiquoteTests', async: false },
    { path: 'functional/quote_tests.js', fn: 'runQuoteTests', async: false },
    { path: 'functional/macro_tests.js', fn: 'runMacroTests', async: true },
    { path: 'functional/syntax_rules_tests.js', fn: 'runSyntaxRulesTests', async: true },
    { path: 'functional/hygiene_tests.js', fn: 'runHygieneTests', async: true },
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
    'tests/core/scheme/primitive_tests.scm',
    'tests/core/scheme/test_harness_tests.scm',
    'tests/core/scheme/boot_tests.scm',
    'tests/core/scheme/record_tests.scm',
    'tests/core/scheme/tco_tests.scm',
    'tests/core/scheme/dynamic_wind_tests.scm',
    'tests/core/scheme/dynamic_wind_interop_tests.scm',
    'tests/core/scheme/control_tests.scm',
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
