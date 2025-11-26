// Mock window for interop/async tests
global.window = {
    globalK: null,
    fetchData: (cb) => setTimeout(() => cb("Fetched data from JS"), 1000),
    setTimeout: setTimeout
};

async function runAll() {
    // Dynamic imports to ensure globals are set first
    const { createTestLogger, createTestEnv } = await import('./helpers.js');
    const { runUnitTests } = await import('./unit/unit_tests.js');
    const { runFunctionalTests } = await import('./functional/functional_tests.js');
    const { runInteropTests } = await import('./functional/interop_tests.js');
    const { runRecordInteropTests } = await import('./functional/record_interop_tests.js');
    const { runSchemeTests } = await import('./run_scheme_tests.js');
    const { runQuasiquoteTests } = await import('./functional/quasiquote_tests.js');
    const { runQuoteTests } = await import('./functional/quote_tests.js');
    const { runMacroTests } = await import('./functional/macro_tests.js');
    const { runSyntaxRulesTests } = await import('./functional/syntax_rules_tests.js');

    const { runDataTests } = await import('./unit/data_tests.js');
    const { runPrimitiveTests } = await import('./unit/primitives_tests.js');

    const { interpreter } = createTestEnv();
    const logger = createTestLogger();

    try {
        runDataTests(logger);
        runPrimitiveTests(logger);
        runUnitTests(interpreter, logger);
    } catch (e) {
        logger.fail(`Unit test suite crashed: ${e.message}`);
    }

    try {
        await runFunctionalTests(interpreter, logger);
        runInteropTests(interpreter, logger);
        await runRecordInteropTests(interpreter, logger);
        runQuasiquoteTests(interpreter, logger);
        runQuoteTests(interpreter, logger);
        await runMacroTests(interpreter, logger);
        await runSyntaxRulesTests(interpreter, logger);

        // Define Node.js file loader
        const fs = await import('fs');
        const path = await import('path');
        const nodeFileLoader = async (relativePath) => {
            const filePath = path.join(process.cwd(), relativePath);
            return fs.readFileSync(filePath, 'utf8');
        };

        await runSchemeTests(interpreter, logger, [
            'primitive_tests.scm',
            'test_harness_tests.scm',
            'record_tests.scm'
        ], nodeFileLoader);
        logger.title('All Tests Complete.');
    } catch (e) {
        logger.fail(`Functional test suite crashed: ${e.message}`);
    }
}

runAll();
