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

    const { createTestLogger, createTestEnv } = await import('./helpers.js');
    const { runAllFromManifest } = await import('./test_manifest.js');
    const { runSchemeTests } = await import('./run_scheme_tests_lib.js');

    const { interpreter } = createTestEnv();
    const logger = createTestLogger();

    // File loader for tests that need it
    const nodeFileLoader = async (relativePath) => {
        const filePath = path.join(process.cwd(), relativePath);
        return fs.readFileSync(filePath, 'utf8');
    };

    // Run all tests with "./" prefix (we're already in tests/)
    await runAllFromManifest('./', interpreter, logger, nodeFileLoader, runSchemeTests);
}

runAll();
