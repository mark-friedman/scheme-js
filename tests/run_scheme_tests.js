import { createTestEnv, run, createTestLogger } from './helpers.js';
import fs from 'fs';
import path from 'path';

export async function runSchemeTests(interpreter, logger, testFiles) {
    logger.title('Running Scheme Tests...');

    // 1. Load boot.scm
    const bootPath = path.join(process.cwd(), 'lib', 'boot.scm');
    const bootCode = fs.readFileSync(bootPath, 'utf8');
    run(interpreter, bootCode);

    // 2. Load test.scm (Harness)
    const testLibPath = path.join(process.cwd(), 'lib', 'test.scm');
    const testLibCode = fs.readFileSync(testLibPath, 'utf8');
    run(interpreter, testLibCode);

    let allPassed = true;

    // 3. Run each test file
    for (const file of testFiles) {
        logger.log(`Running ${file}...`);
        const filePath = path.join(process.cwd(), 'tests', 'scheme', file);
        const code = fs.readFileSync(filePath, 'utf8');

        // Run the test file
        run(interpreter, code);

        // Check results by inspecting the global variables *test-failures*
        // We need to reset them for each file? Or accumulate?
        // Let's accumulate for now, or check the result of (test-report)

        const result = run(interpreter, '(test-report)');
        if (result !== true) {
            allPassed = false;
            logger.fail(`${file} FAILED`);
        } else {
            logger.pass(`${file} PASSED`);
        }

        // Reset counters for next file?
        run(interpreter, '(set! *test-failures* 0)');
        run(interpreter, '(set! *test-passes* 0)');
    }

    if (!allPassed) {
        throw new Error("Some Scheme tests failed");
    }
}

// Allow running directly
if (process.argv[1] === import.meta.url.slice(7)) { // Remove file://
    const logger = createTestLogger();
    const { interpreter } = createTestEnv();

    // Default test files if not specified
    const tests = [
        'primitive_tests.scm',
        'test_harness_tests.scm',
        'record_tests.scm'
    ];

    runSchemeTests(interpreter, logger, tests)
        .then(() => console.log("All Scheme tests passed"))
        .catch(e => {
            console.error(e);
            process.exit(1);
        });
}
