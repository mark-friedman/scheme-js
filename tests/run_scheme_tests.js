import { createTestEnv, run, createTestLogger } from './helpers.js';

export async function runSchemeTests(interpreter, logger, testFiles, fileLoader) {
    logger.title('Running Scheme Tests...');

    // 1. Load boot.scm
    const bootCode = await fileLoader('src/layer-1-kernel/scheme/boot.scm');
    run(interpreter, bootCode);

    // Inject native reporter (after boot.scm to avoid overwrite)
    interpreter.globalEnv.bindings.set('native-report-test-result', (name, passed, expected, actual) => {
        if (passed) {
            logger.pass(`${name} (Expected: ${expected}, Got: ${actual})`);
        } else {
            logger.fail(`${name} (Expected: ${expected}, Got: ${actual})`);
        }
    });

    // 2. Load test.scm (Harness)
    const testLibCode = await fileLoader('tests/scheme/test.scm');
    run(interpreter, testLibCode);

    let allPassed = true;

    // 3. Run each test file
    for (const file of testFiles) {
        logger.log(`Running ${file}...`);
        const code = await fileLoader(`tests/scheme/${file}`);

        // Run the test file
        run(interpreter, code);

        const result = run(interpreter, '(test-report)');
        if (result !== true) {
            allPassed = false;
            logger.fail(`${file} FAILED`);
        } else {
            logger.pass(`${file} PASSED`);
        }

        // Reset counters for next file
        run(interpreter, '(set! *test-failures* 0)');
        run(interpreter, '(set! *test-passes* 0)');
    }

    if (!allPassed) {
        throw new Error("Some Scheme tests failed");
    }
}

// Allow running directly
// Allow running directly
if (typeof process !== 'undefined' && process.argv[1] === import.meta.url.slice(7)) { // Remove file://
    const logger = createTestLogger();
    const { interpreter } = createTestEnv();

    // Default test files if not specified
    const tests = [
        '../layer-1/scheme/primitive_tests.scm',
        'test_harness_tests.scm',
        '../layer-1/scheme/boot_tests.scm',
        '../layer-1/scheme/record_tests.scm',
        '../layer-1/scheme/tco_tests.scm'
    ];

    runSchemeTests(interpreter, logger, tests)
        .then(() => console.log("All Scheme tests passed"))
        .catch(e => {
            console.error(e);
            process.exit(1);
        });
}
