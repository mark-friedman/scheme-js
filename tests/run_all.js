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
    const { runQuasiquoteTests } = await import('./functional/quasiquote_tests.js');
    const { runQuoteTests } = await import('./functional/quote_tests.js');

    const { interpreter } = createTestEnv();
    const logger = createTestLogger();

    try {
        runUnitTests(interpreter, logger);
    } catch (e) {
        logger.fail(`Unit test suite crashed: ${e.message}`);
    }

    try {
        await runFunctionalTests(interpreter, logger);
        runInteropTests(interpreter, logger);
        runQuasiquoteTests(interpreter, logger);
        runQuoteTests(interpreter, logger);
        logger.title('All Tests Complete.');
    } catch (e) {
        logger.fail(`Functional test suite crashed: ${e.message}`);
    }
}

runAll();
