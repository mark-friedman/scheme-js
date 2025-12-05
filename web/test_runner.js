import { createLayer1 } from '../src/layer-1-kernel/index.js';
import { run } from '../tests/layer-1/tests.js';

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

(async () => {
    try {
        // Create the interpreter instance using Layer 1 factory
        const { interpreter, env } = createLayer1();

        // File loader for Scheme tests in browser
        const browserFileLoader = async (relativePath) => {
            // relativePath is like "lib/boot.scm" or "tests/scheme/primitive_tests.scm"
            // We need to fetch from root.
            // Since test_runner.js is in web/, and we serve from root,
            // fetching "../" + relativePath should work if served from root.
            // But usually web servers serve root as /.
            // If we are at /web/tests.html, then ../ points to root.
            const response = await fetch('../' + relativePath);
            if (!response.ok) {
                throw new Error(`Failed to fetch ${relativePath}: ${response.statusText}`);
            }
            return response.text();
        };

        // Run all tests
        await run(interpreter, env, browserFileLoader, logger);

        logger.title('All Tests Complete.');
    } catch (e) {
        logger.fail(`Test suite crashed: ${e.message}`);
        console.error(e);
    }
})();
