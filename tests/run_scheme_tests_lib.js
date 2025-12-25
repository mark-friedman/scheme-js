import { run } from './harness/helpers.js';
import { loadLibrary, applyImports, setFileResolver, registerBuiltinLibrary, createPrimitiveExports } from '../src/core/interpreter/library_loader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';

export async function runSchemeTests(interpreter, logger, testFiles, fileLoader) {
    logger.title('Running Scheme Tests...');

    // Re-register (scheme primitives) in case previous tests cleared the registry
    const primitiveExports = createPrimitiveExports(interpreter.globalEnv);
    registerBuiltinLibrary(['scheme', 'primitives'], primitiveExports, interpreter.globalEnv);

    // Set up the file resolver for the library loader
    // Handles both library names and include file paths
    setFileResolver(async (pathParts) => {
        // pathParts is an array like:
        // - ['scheme', 'base'] for library imports -> base.sld
        // - ['scheme', 'macros.scm'] for include directives -> macros.scm
        const fileName = pathParts[pathParts.length - 1];

        let libPath;
        if (fileName.endsWith('.scm') || fileName.endsWith('.sld')) {
            // Include directive - fileName already has extension
            libPath = `src/core/scheme/${fileName}`;
        } else {
            // Library import - add .sld extension
            libPath = `src/core/scheme/${fileName}.sld`;
        }

        return await fileLoader(libPath);
    });

    // 1. Bootstrap standard libraries
    const baseExports = await loadLibrary(['scheme', 'base'], analyze, interpreter, interpreter.globalEnv);
    const replExports = await loadLibrary(['scheme', 'repl'], analyze, interpreter, interpreter.globalEnv);
    const caseLambdaExports = await loadLibrary(['scheme', 'case-lambda'], analyze, interpreter, interpreter.globalEnv);
    applyImports(interpreter.globalEnv, baseExports, { libraryName: ['scheme', 'base'] });
    applyImports(interpreter.globalEnv, replExports, { libraryName: ['scheme', 'repl'] });
    applyImports(interpreter.globalEnv, caseLambdaExports, { libraryName: ['scheme', 'case-lambda'] });

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

    interpreter.globalEnv.bindings.set('native-report-test-skip', (name, reason) => {
        logger.skip(`${name} (Reason: ${reason})`);
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
