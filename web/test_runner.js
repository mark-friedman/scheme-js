import { createInterpreter } from '../src/core/interpreter/index.js';
import { run } from '../tests/core/interpreter/tests.js';

// Simple logger to write to the page with pass/fail tracking
const outputEl = document.getElementById('test-output');
let passCount = 0;
let failCount = 0;
const failures = [];

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
    pass: (message) => {
        passCount++;
        logger.log(`✅ PASS: ${message}`, 'pass');
    },
    fail: (message) => {
        failCount++;
        failures.push(message);
        logger.log(`❌ FAIL: ${message}`, 'fail');
    },
    summary: () => {
        const el = document.createElement('div');
        el.className = 'mt-4 p-4 rounded ' + (failCount > 0 ? 'bg-red-100' : 'bg-green-100');
        el.innerHTML = `
            <h2 class="text-2xl font-bold mb-2">TEST SUMMARY</h2>
            <p class="text-lg">${passCount} passed, ${failCount} failed</p>
            ${failCount > 0 ? `<ul class="mt-2 text-red-700">${failures.map((f, i) => `<li>${i + 1}. ${f}</li>`).join('')}</ul>` : ''}
        `;
        outputEl.appendChild(el);
        console.log(`\nTEST SUMMARY: ${passCount} passed, ${failCount} failed`);
        return { passCount, failCount, failures };
    },
    getStats: () => ({ passCount, failCount, failures }),
};


(async () => {
    try {
        // Create the interpreter instance using Layer 1 factory
        const { interpreter, env } = createInterpreter();

        // File loader for Scheme tests in browser
        const browserFileLoader = async (relativePath) => {
            // relativePath is like "lib/boot.scm" or "tests/scheme/primitive_tests.scm"
            // Fetch from parent directory since test_runner.js is in web/
            const response = await fetch('../' + relativePath);
            if (!response.ok) {
                throw new Error(`Failed to fetch ${relativePath}: ${response.statusText}`);
            }
            return response.text();
        };

        // Run all tests - summary is called inside run()
        await run(interpreter, env, browserFileLoader, logger);
    } catch (e) {
        logger.fail(`Test suite crashed: ${e.message}`);
        console.error(e);
    }
})();

