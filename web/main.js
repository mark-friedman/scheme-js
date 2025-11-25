import { Environment } from '../src/core/environment.js';
import { createGlobalEnvironment } from '../src/primitives/index.js';
import { Interpreter } from '../src/core/interpreter.js';
import { parse } from '../src/syntax/reader.js';
import { analyze } from '../src/syntax/analyzer.js';
import { setupRepl, prettyPrint } from './repl.js';
import { runUnitTests, runAllTests } from '../tests/tests.js';

// --- Main Entry Point ---

// Simple logger to write to the page
const outputEl = document.getElementById('test-output');
const logger = {
    log: (message, type = 'info') => {
        console.log(message);
        const el = document.createElement('div');
        el.textContent = message;
        el.className = `log-item log-${type}`;
        outputEl.appendChild(el);
    },
    title: (message) => {
        console.log(`\n%c${message}`, 'font-weight: bold; font-size: 1.2em;');
        const el = document.createElement('h3');
        el.textContent = message;
        el.className = 'text-xl font-semibold mt-4 mb-2 text-gray-700 log-title';
        outputEl.appendChild(el);
    },
    pass: (message) => logger.log(`✅ PASS: ${message}`, 'pass'),
    fail: (message) => logger.log(`❌ FAIL: ${message}`, 'fail'),
};

// Create the interpreter instance
const interpreter = new Interpreter();
const globalEnv = createGlobalEnvironment(interpreter);
interpreter.setGlobalEnv(globalEnv);

// Setup REPL UI
setupRepl(interpreter, globalEnv);

// --- Run Tests ---

// Run unit tests first
try {
    runUnitTests(interpreter, logger);
} catch (e) {
    logger.fail(`Unit test suite crashed: ${e.message}`);
}

// Run all functional tests
logger.title('Running Functional Tests...');
// Make the test runner async to handle the new test
(async () => {
    try {
        await runAllTests(interpreter, logger);
        logger.title('All Tests Complete.');
    } catch (e) {
        logger.fail(`Functional test suite crashed: ${e.message}`);
    }
})();
