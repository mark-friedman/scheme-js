/**
 * Node.js Test Runner
 * 
 * Runs all tests in the Node.js environment.
 * The browser uses tests/runtime/tests.js which has an identical test list.
 */

// Mock window for interop/async tests
global.window = {
    globalK: null,
    fetchData: (cb) => setTimeout(() => cb("Fetched data from JS"), 1000),
    setTimeout: setTimeout
};

async function runAll() {
    const { createTestLogger, createTestEnv } = await import('./helpers.js');

    // Unit tests
    const { runDataTests } = await import('./unit/data_tests.js');
    const { runPrimitiveTests } = await import('./unit/primitives_tests.js');
    const { runUnitTests } = await import('./unit/unit_tests.js');
    const { runWindersTests } = await import('./unit/winders_tests.js');
    const { runAnalyzerTests } = await import('./unit/analyzer_tests.js');
    const { runReaderTests } = await import('./unit/reader_tests.js');
    const { runSyntaxRulesUnitTests } = await import('./unit/syntax_rules_tests.js');

    // Functional tests
    const { runFunctionalTests } = await import('./functional/functional_tests.js');
    const { runInteropTests } = await import('./functional/interop_tests.js');
    const { runQuasiquoteTests } = await import('./functional/quasiquote_tests.js');
    const { runQuoteTests } = await import('./functional/quote_tests.js');
    const { runMacroTests } = await import('./functional/macro_tests.js');
    const { runSyntaxRulesTests } = await import('./functional/syntax_rules_tests.js');
    const { runDefineTests } = await import('./functional/define_tests.js');
    const { runEvalApplyTests } = await import('./functional/eval_apply_tests.js');
    const { runRecordInteropTests } = await import('./functional/record_interop_tests.js');

    // Integration tests
    const { runLibraryLoaderTests } = await import('./integration/library_loader_tests.js');

    // Scheme tests
    const { runSchemeTests } = await import('./run_scheme_tests_lib.js');

    const { interpreter } = createTestEnv();
    const logger = createTestLogger();

    // === Unit Tests ===
    try {
        runDataTests(logger);
        runPrimitiveTests(logger);
        runUnitTests(interpreter, logger);
        runWindersTests(interpreter, logger);
        runAnalyzerTests(interpreter, logger);
        runReaderTests(logger);
        runSyntaxRulesUnitTests(logger);
    } catch (e) {
        logger.fail(`Unit test suite crashed: ${e.message}`);
    }

    // === Functional Tests ===
    try {
        await runFunctionalTests(interpreter, logger);
        runInteropTests(interpreter, logger);
        runQuasiquoteTests(interpreter, logger);
        runQuoteTests(interpreter, logger);
        await runMacroTests(interpreter, logger);
        await runSyntaxRulesTests(interpreter, logger);
        runDefineTests(interpreter, logger);
        await runEvalApplyTests(interpreter, logger);

        // File loader for record tests
        const fs = await import('fs');
        const path = await import('path');
        const nodeFileLoader = async (relativePath) => {
            const filePath = path.join(process.cwd(), relativePath);
            return fs.readFileSync(filePath, 'utf8');
        };
        await runRecordInteropTests(interpreter, logger, nodeFileLoader);
    } catch (e) {
        logger.fail(`Functional test suite crashed: ${e.message}`);
    }

    // === Integration Tests ===
    try {
        await runLibraryLoaderTests(logger);
    } catch (e) {
        logger.fail(`Integration test suite crashed: ${e.message}`);
    }

    // === Scheme Tests ===
    try {
        const fs = await import('fs');
        const path = await import('path');
        const nodeFileLoader = async (relativePath) => {
            const filePath = path.join(process.cwd(), relativePath);
            return fs.readFileSync(filePath, 'utf8');
        };

        await runSchemeTests(interpreter, logger, [
            'tests/runtime/scheme/primitive_tests.scm',
            'tests/scheme/test_harness_tests.scm',
            'tests/runtime/scheme/boot_tests.scm',
            'tests/runtime/scheme/record_tests.scm',
            'tests/runtime/scheme/tco_tests.scm',
            'tests/runtime/scheme/dynamic_wind_tests.scm',
            'tests/runtime/scheme/dynamic_wind_interop_tests.scm'
        ], nodeFileLoader);
    } catch (e) {
        logger.fail(`Scheme test suite crashed: ${e.message}`);
    }

    // Print summary
    logger.summary();
}

runAll();
