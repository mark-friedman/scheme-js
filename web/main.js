import { Environment } from '../src/core/environment.js';
import { createGlobalEnvironment } from '../src/primitives/index.js';
import { Interpreter } from '../src/core/interpreter.js';
import { parse } from '../src/syntax/reader.js';
import { analyze } from '../src/syntax/analyzer.js';
import { setupRepl, prettyPrint } from './repl.js';

// Import test runners
import { runUnitTests } from '../tests/unit/unit_tests.js';
import { runFunctionalTests } from '../tests/functional/functional_tests.js';
import { runInteropTests } from '../tests/functional/interop_tests.js';
import { runQuasiquoteTests } from '../tests/functional/quasiquote_tests.js';
import { runQuoteTests } from '../tests/functional/quote_tests.js';
import { runMacroTests } from '../tests/functional/macro_tests.js';
import { runSyntaxRulesTests } from '../tests/functional/syntax_rules_tests.js';
import { runDataTests } from '../tests/unit/data_tests.js';
import { runPrimitiveTests } from '../tests/unit/primitives_tests.js';
import { runVectorTests } from '../tests/unit/vector_tests.js';

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

(async () => {
    try {
        // Run Unit Tests
        runDataTests(logger);
        runPrimitiveTests(logger);
        runVectorTests(logger);
        runUnitTests(interpreter, logger);

        // Run Functional Tests
        logger.title('Running Functional Tests...');
        await runFunctionalTests(interpreter, logger);
        runInteropTests(interpreter, logger);
        runQuasiquoteTests(interpreter, logger);
        runQuoteTests(interpreter, logger);
        await runMacroTests(interpreter, logger);
        await runSyntaxRulesTests(interpreter, logger);

        logger.title('All Tests Complete.');
    } catch (e) {
        logger.fail(`Test suite crashed: ${e.message}`);
        console.error(e);
    }
})();
