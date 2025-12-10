/**
 * Browser/Node Core Test Runner
 * 
 * Uses the shared test manifest.
 * Called by web/test_runner.js (browser).
 * 
 * NOTE: Dynamic imports in test_manifest.js resolve relative to the manifest
 * file (tests/), so we use "./" prefix for both Node and browser.
 */

import { runAllFromManifest, schemeTestFiles } from '../../test_manifest.js';
import { runSchemeTests } from '../../run_scheme_tests_lib.js';
import { createTestLogger } from '../../helpers.js';

/**
 * Main test runner.
 * @param {Object} interpreter - Interpreter instance
 * @param {Object} env - Global environment (unused, kept for compatibility)
 * @param {Function} schemeFileLoader - File loader for Scheme tests
 * @param {Object} customLogger - Optional custom logger
 */
export async function run(interpreter, env, schemeFileLoader, customLogger) {
    console.log("Running All Tests...");

    const logger = customLogger || createTestLogger();

    // Mock window for interop tests if running in Node
    if (typeof global !== 'undefined' && !global.window) {
        global.window = {
            globalK: null,
            fetchData: (cb) => setTimeout(() => cb("Fetched data from JS"), 1000),
            setTimeout: setTimeout
        };
    }

    // Initialize file loader
    let loader = schemeFileLoader;
    if (!loader && typeof process !== 'undefined') {
        const fs = await import('fs');
        const path = await import('path');
        loader = async (relativePath) => {
            const filePath = path.join(process.cwd(), relativePath);
            return fs.readFileSync(filePath, 'utf8');
        };
    }

    // Dynamic imports in manifest resolve relative to tests/, so use "./"
    await runAllFromManifest('./', interpreter, logger, loader, runSchemeTests);
}

