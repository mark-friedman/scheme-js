import { runUnitTests } from '../unit/unit_tests.js';
import { runFunctionalTests } from '../functional/functional_tests.js';
import { runInteropTests } from '../functional/interop_tests.js';
import { runQuasiquoteTests } from '../functional/quasiquote_tests.js';
import { runDefineTests } from '../functional/define_tests.js';
import { runDataTests } from '../unit/data_tests.js';
import { runPrimitiveTests } from '../unit/primitives_tests.js';
import { runRecordInteropTests } from '../functional/record_interop_tests.js';
import { runQuoteTests } from '../functional/quote_tests.js';
import { runMacroTests } from '../functional/macro_tests.js';
import { runSyntaxRulesTests } from '../functional/syntax_rules_tests.js';
import { runEvalApplyTests } from '../functional/eval_apply_tests.js';
import { runSchemeTests } from '../run_scheme_tests_lib.js';
import { createTestLogger } from '../helpers.js';

// New Unit Tests
import { runWindersTests } from '../unit/winders_tests.js';
import { runAnalyzerTests } from '../unit/analyzer_tests.js';
import { runReaderTests } from '../unit/reader_tests.js';
import { runSyntaxRulesUnitTests } from '../unit/syntax_rules_tests.js';

export async function run(interpreter, env, schemeFileLoader, customLogger) {
    console.log("Running Runtime Tests...");

    // Use provided logger or create default
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

    // Run Unit Tests
    await runUnitTests(interpreter, logger);
    runDataTests(logger);
    runPrimitiveTests(logger);

    // New Unit Tests
    runWindersTests(interpreter, logger);
    runAnalyzerTests(interpreter, logger);
    runReaderTests(logger);
    runSyntaxRulesUnitTests(logger);

    // Run Functional Tests

    await runFunctionalTests(interpreter, logger);
    runInteropTests(interpreter, logger);
    runQuasiquoteTests(interpreter, logger);
    runDefineTests(interpreter, logger);
    await runRecordInteropTests(interpreter, logger, loader);
    runQuoteTests(interpreter, logger);
    await runMacroTests(interpreter, logger);
    await runSyntaxRulesTests(interpreter, logger);
    await runEvalApplyTests(interpreter, logger);

    // Run Scheme Tests
    if (loader) {
        await runSchemeTests(interpreter, logger, [
            'tests/runtime/scheme/primitive_tests.scm',
            'tests/scheme/test_harness_tests.scm',
            'tests/runtime/scheme/boot_tests.scm',
            'tests/runtime/scheme/record_tests.scm',
            'tests/runtime/scheme/tco_tests.scm',
            'tests/runtime/scheme/dynamic_wind_tests.scm',
            'tests/runtime/scheme/dynamic_wind_interop_tests.scm'
        ], loader);
    } else {
        console.warn("Skipping Scheme tests (No file loader provided)");
    }
}
