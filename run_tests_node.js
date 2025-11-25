
// Mock window for environment.js
global.window = {
    fetchData: () => { },
    setTimeout: setTimeout
};

// Mock document for main.js (if needed, though we don't import main.js)
global.document = {
    getElementById: () => ({ appendChild: () => { } }),
    createElement: () => ({})
};

// Dynamic imports to ensure globals are set first
const { createGlobalEnvironment } = await import('./environment.js');
const { Interpreter } = await import('./interpreter.js');
const { runUnitTests, runAllTests } = await import('./tests/tests.js');

// Mock logger for console output
const logger = {
    log: (message, type = 'info') => console.log(`[${type.toUpperCase()}] ${message}`),
    title: (message) => console.log(`\n=== ${message} ===`),
    pass: (message) => console.log(`✅ PASS: ${message}`),
    fail: (message) => {
        console.log("CALLED FAIL");
        console.error(`❌ FAIL: ${message}`);
        process.exitCode = 1;
    },
};

async function run() {
    const interpreter = new Interpreter();
    const globalEnv = createGlobalEnvironment(interpreter);
    interpreter.setGlobalEnv(globalEnv);

    try {
        runUnitTests(interpreter, logger);
    } catch (e) {
        logger.fail(`Unit test suite crashed: ${e.message}`);
    }

    try {
        await runAllTests(interpreter, logger);
        logger.title('All Tests Complete.');
    } catch (e) {
        logger.fail(`Functional test suite crashed: ${e.message}`);
    }
    console.log(`DEBUG: process.exitCode = ${process.exitCode}`);
}

run();
