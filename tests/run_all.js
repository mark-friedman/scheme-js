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

    const { createTestLogger, createTestEnv, run } = await import('./helpers.js');
    const { runAllFromManifest } = await import('./test_manifest.js');
    const { runSchemeTests } = await import('./run_scheme_tests_lib.js');

    const { interpreter } = createTestEnv();
    const logger = createTestLogger();

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
        ];

        for (const file of schemeFiles) {
            const code = await nodeFileLoader(file);
            run(interpreter, code);
        }
    };

    // Run all tests with "./" prefix (we're already in tests/)
    await runAllFromManifest('./', interpreter, logger, nodeFileLoader, runSchemeTests, loadBootstrap);
}

runAll();
