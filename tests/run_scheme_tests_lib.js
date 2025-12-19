import { run } from './helpers.js';

export async function runSchemeTests(interpreter, logger, testFiles, fileLoader) {
    logger.title('Running Scheme Tests...');

    // Reload macros because hygiene_tests.js clears globalMacroRegistry
    // which destroys macros like 'and', 'let', 'letrec', 'cond'
    const macrosCode = await fileLoader('src/core/scheme/macros.scm');
    run(interpreter, macrosCode);

    // And control macros: 'when', 'unless', 'or', etc.
    const controlCode = await fileLoader('src/core/scheme/control.scm');
    run(interpreter, controlCode);

    // Inject native reporter
    interpreter.globalEnv.bindings.set('native-report-test-result', (name, passed, expected, actual) => {
        if (passed) {
            logger.pass(`${name} (Expected: ${expected}, Got: ${actual})`);
        } else {
            logger.fail(`${name} (Expected: ${expected}, Got: ${actual})`);
        }
    });

    // 2. Load test.scm (Harness)
    const testLibCode = await fileLoader('tests/core/scheme/test.scm');
    run(interpreter, testLibCode);

    let allPassed = true;

    // 3. Run each test file
    for (const file of testFiles) {
        logger.log(`Running ${file}...`);

        // Use the loader to read the file content
        const code = await fileLoader(file); // Changed: pass file directly to loader, loader handles relativity

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
