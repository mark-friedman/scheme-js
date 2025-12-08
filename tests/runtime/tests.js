/**
 * Browser/Node Runtime Test Runner
 * 
 * Runs all tests. Used by web/test_runner.js (browser).
 * 
 * NOTE: This file has the SAME test list as run_all.js (Node runner).
 * When adding tests, update BOTH files.
 */

// Unit tests
import { runDataTests } from '../unit/data_tests.js';
import { runPrimitiveTests } from '../unit/primitives_tests.js';
import { runUnitTests } from '../unit/unit_tests.js';
import { runWindersTests } from '../unit/winders_tests.js';
import { runAnalyzerTests } from '../unit/analyzer_tests.js';
import { runReaderTests } from '../unit/reader_tests.js';
import { runSyntaxRulesUnitTests } from '../unit/syntax_rules_tests.js';

// Functional tests
import { runFunctionalTests } from '../functional/functional_tests.js';
import { runInteropTests } from '../functional/interop_tests.js';
import { runQuasiquoteTests } from '../functional/quasiquote_tests.js';
import { runQuoteTests } from '../functional/quote_tests.js';
import { runMacroTests } from '../functional/macro_tests.js';
import { runSyntaxRulesTests } from '../functional/syntax_rules_tests.js';
import { runDefineTests } from '../functional/define_tests.js';
import { runEvalApplyTests } from '../functional/eval_apply_tests.js';
import { runRecordInteropTests } from '../functional/record_interop_tests.js';

// Integration tests  
import { runLibraryLoaderTests } from '../integration/library_loader_tests.js';

// Scheme tests
import { runSchemeTests } from '../run_scheme_tests_lib.js';

// Helpers
import { createTestLogger } from '../helpers.js';

// Scheme test files
const schemeTestFiles = [
    'tests/runtime/scheme/primitive_tests.scm',
    'tests/scheme/test_harness_tests.scm',
    'tests/runtime/scheme/boot_tests.scm',
    'tests/runtime/scheme/record_tests.scm',
    'tests/runtime/scheme/tco_tests.scm',
    'tests/runtime/scheme/dynamic_wind_tests.scm',
    'tests/runtime/scheme/dynamic_wind_interop_tests.scm',
];

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

    // === Unit Tests ===
    try {
        runDataTests(logger);
        runPrimitiveTests(logger);
        await runUnitTests(interpreter, logger);
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
        await runRecordInteropTests(interpreter, logger, loader);
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
    if (loader) {
        try {
            await runSchemeTests(interpreter, logger, schemeTestFiles, loader);
        } catch (e) {
            logger.fail(`Scheme test suite crashed: ${e.message}`);
        }
    } else {
        console.warn("Skipping Scheme tests (No file loader provided)");
    }

    // Print summary
    if (logger.summary) {
        logger.summary();
    }
}
