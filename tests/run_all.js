/**
 * Node.js Test Runner
 * 
 * Uses the shared test manifest with "./" prefix.
 */

// Mock window for interop/async tests
global.window = {
    globalK: null,
    fetchData: (cb) => setTimeout(() => cb("Fetched data from JS"), 1000),
    setTimeout: setTimeout
};

async function runAll() {
    const fs = await import('fs');
    const path = await import('path');

    const { createTestLogger, createTestEnv, run } = await import('./harness/helpers.js');
    const { runAllFromManifest } = await import('./test_manifest.js');
    const { runSchemeTests } = await import('./run_scheme_tests_lib.js');

    const { interpreter } = createTestEnv();

    // Parse arguments
    const verbose = process.argv.includes('--verbose') || process.argv.includes('-v');
    const logger = createTestLogger({ verbose });

    if (verbose) {
        console.log('Verbose mode enabled');
    }

    // File loader for tests that need it
    const nodeFileLoader = async (relativePath) => {
        const filePath = path.join(process.cwd(), relativePath);
        return fs.readFileSync(filePath, 'utf8');
    };

    // Callback to load Scheme bootstrap files AFTER unit tests
    // This ensures unit tests test the raw interpreter without macros
    const loadBootstrap = async () => {
        // Load Scheme core files in dependency order
        // These files are organized by type but loaded directly to populate
        // the global environment and macro registry
        const schemeFiles = [
            'src/core/scheme/macros.scm',     // Core macros: and, let, letrec, cond
            'src/core/scheme/equality.scm',   // equal?
            'src/core/scheme/cxr.scm',        // caar, cadr, etc.
            'src/core/scheme/numbers.scm',    // =, <, >, zero?, max, gcd, round
            'src/core/scheme/list.scm',       // map, for-each, memq, assq, length
            'src/core/scheme/control.scm',    // when, unless, or, let*, do, case
            'src/core/scheme/case_lambda.scm', // case-lambda
        ];

        for (const file of schemeFiles) {
            const code = await nodeFileLoader(file);
            run(interpreter, code);
        }

        // With SCHEME_AOT_STDLIB=1 the library is compiled after loading, and
        // the whole suite then runs against compiled implementations of `map`,
        // `assq`, `member` and the rest. It is off by default because the unit
        // tests are meant to exercise the interpreter; running the suite both
        // ways is what checks the two tiers agree on the library itself.
        if (process.env.SCHEME_AOT_STDLIB === '1') {
            const { compileEnvironment } = await import('../src/compiler/index.js');
            const outcome = compileEnvironment(interpreter.globalEnv);
            console.log(`\nAOT: compiled ${outcome.compiled.length} standard library `
                + `procedures, declined ${outcome.declined.length}`);
            for (const d of outcome.declined) console.log(`  ${d.name}: ${d.reason}`);
        }
        // Note: Promise primitives (js-promise-chain, js-promise-map, etc.)
        // are always available as they're registered as primitives in primitives/index.js
    };

    // Run all tests with "./" prefix (we're already in tests/)
    await runAllFromManifest('./', interpreter, logger, nodeFileLoader, runSchemeTests, loadBootstrap);
}

runAll();
