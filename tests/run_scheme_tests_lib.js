import { run } from './helpers.js';
import { loadLibrary, applyImports } from '../src/core/interpreter/library_loader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';

export async function runSchemeTests(interpreter, logger, testFiles, fileLoader) {
    logger.title('Running Scheme Tests...');

    // 1. Bootstrap standard libraries
    const baseExports = await loadLibrary(['scheme', 'base'], analyze, interpreter, interpreter.globalEnv);
    const replExports = await loadLibrary(['scheme', 'repl'], analyze, interpreter, interpreter.globalEnv);
    applyImports(interpreter.globalEnv, baseExports, { libraryName: ['scheme', 'base'] });
    applyImports(interpreter.globalEnv, replExports, { libraryName: ['scheme', 'repl'] });

    // Reload macros because hygiene_tests.js clears globalMacroRegistry
    // which destroys macros like 'and', 'let', 'letrec', 'cond'
    // Note: (scheme base) already loads these, but hygiene tests might clear them later.
    // For now, let's rely on the bootstrap.

    // And control macros: 'when', 'unless', 'or', etc. are now in (scheme base) via (scheme control)

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
