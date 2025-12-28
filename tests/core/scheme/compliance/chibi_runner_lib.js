/**
 * Chibi Compliance Test Runner Library
 * 
 * Shared library for running the R7RS compliance tests in both
 * Node.js and browser environments.
 */

import { createInterpreter } from '../../../../src/core/interpreter/index.js';
import { run } from '../../../harness/helpers.js';
import { loadLibrary, applyImports, setFileResolver, registerBuiltinLibrary, createPrimitiveExports } from '../../../../src/core/interpreter/library_loader.js';
import { analyze } from '../../../../src/core/interpreter/analyzer.js';
import { resetGlobalMacroRegistry, snapshotMacroRegistry } from '../../../../src/core/interpreter/macro_registry.js';
import { writeString } from '../../../../src/core/primitives/io.js';

// Section files in order
const sectionFiles = [
    '4.1-primitives.scm',
    '4.2-derived.scm',
    '4.3-macros.scm',
    '5-program-structure.scm',
    '6.1-equivalence.scm',
    '6.2-numbers.scm',
    '6.3-booleans.scm',
    '6.4-lists.scm',
    '6.5-symbols.scm',
    '6.6-characters.scm',
    '6.7-strings.scm',
    '6.8-vectors.scm',
    '6.9-bytevectors.scm',
    '6.10-control.scm',
    '6.11-exceptions.scm',
    '6.12-environments.scm',
    '6.13-io.scm',
    '6.14-system.scm',
    '7.1-read-syntax.scm',
    '7.1-numeric-syntax.scm'
];

/**
 * Creates and configures an interpreter for compliance testing.
 * @param {Function} fileLoader - Function to load files by path
 * @param {Object} logger - Logger object with pass/fail methods
 * @returns {Object} - { interpreter, run, runSectionTest, runAllSections }
 */
export async function createComplianceRunner(fileLoader, logger) {
    // Reset global macro registry for test isolation
    resetGlobalMacroRegistry();

    const { interpreter } = createInterpreter();

    // Register (scheme primitives)
    const primitiveExports = createPrimitiveExports(interpreter.globalEnv);
    registerBuiltinLibrary(['scheme', 'primitives'], primitiveExports, interpreter.globalEnv);

    // Set up file resolver for library loader
    setFileResolver(async (pathParts) => {
        const fileName = pathParts[pathParts.length - 1];
        let libPath;
        if (fileName.endsWith('.scm') || fileName.endsWith('.sld')) {
            libPath = `src/core/scheme/${fileName}`;
        } else {
            libPath = `src/core/scheme/${fileName}.sld`;
        }
        return await fileLoader(libPath);
    });

    // Bootstrap standard libraries - load all available R7RS libraries
    const baseExports = await loadLibrary(['scheme', 'base'], analyze, interpreter, interpreter.globalEnv);
    const replExports = await loadLibrary(['scheme', 'repl'], analyze, interpreter, interpreter.globalEnv);
    const caseLambdaExports = await loadLibrary(['scheme', 'case-lambda'], analyze, interpreter, interpreter.globalEnv);
    const lazyExports = await loadLibrary(['scheme', 'lazy'], analyze, interpreter, interpreter.globalEnv);
    const charExports = await loadLibrary(['scheme', 'char'], analyze, interpreter, interpreter.globalEnv);
    const cxrExports = await loadLibrary(['scheme', 'cxr'], analyze, interpreter, interpreter.globalEnv);
    const readExports = await loadLibrary(['scheme', 'read'], analyze, interpreter, interpreter.globalEnv);
    const writeExports = await loadLibrary(['scheme', 'write'], analyze, interpreter, interpreter.globalEnv);
    const evalExports = await loadLibrary(['scheme', 'eval'], analyze, interpreter, interpreter.globalEnv);
    const timeExports = await loadLibrary(['scheme', 'time'], analyze, interpreter, interpreter.globalEnv);
    const processContextExports = await loadLibrary(['scheme', 'process-context'], analyze, interpreter, interpreter.globalEnv);
    const fileExports = await loadLibrary(['scheme', 'file'], analyze, interpreter, interpreter.globalEnv);

    applyImports(interpreter.globalEnv, baseExports, { libraryName: ['scheme', 'base'] });
    applyImports(interpreter.globalEnv, replExports, { libraryName: ['scheme', 'repl'] });
    applyImports(interpreter.globalEnv, caseLambdaExports, { libraryName: ['scheme', 'case-lambda'] });
    applyImports(interpreter.globalEnv, lazyExports, { libraryName: ['scheme', 'lazy'] });
    applyImports(interpreter.globalEnv, charExports, { libraryName: ['scheme', 'char'] });
    applyImports(interpreter.globalEnv, cxrExports, { libraryName: ['scheme', 'cxr'] });
    applyImports(interpreter.globalEnv, readExports, { libraryName: ['scheme', 'read'] });
    applyImports(interpreter.globalEnv, writeExports, { libraryName: ['scheme', 'write'] });
    applyImports(interpreter.globalEnv, evalExports, { libraryName: ['scheme', 'eval'] });
    applyImports(interpreter.globalEnv, timeExports, { libraryName: ['scheme', 'time'] });
    applyImports(interpreter.globalEnv, processContextExports, { libraryName: ['scheme', 'process-context'] });
    applyImports(interpreter.globalEnv, fileExports, { libraryName: ['scheme', 'file'] });

    // Inject native reporter
    interpreter.globalEnv.bindings.set('native-report-test-result', (name, passed, expected, actual) => {
        const expectedStr = writeString(expected);
        const actualStr = writeString(actual);

        if (passed) {
            logger.pass(`${name} (Expected: ${expectedStr}, Got: ${actualStr})`);
        } else {
            logger.fail(`${name} (Expected: ${expectedStr}, Got: ${actualStr})`);
        }
    });

    // Inject skip reporter
    interpreter.globalEnv.bindings.set('native-report-test-skip', (name, reason) => {
        if (logger.skip) {
            logger.skip(`${name} (Reason: ${reason})`);
        } else {
            console.log(`⏭️ SKIP: ${name} - ${reason}`);
        }
    });

    // Load the test harness
    const harnessCode = await fileLoader('tests/core/scheme/test.scm');
    run(interpreter, harnessCode);

    // Snapshot macro registry state after loading standard libraries and test harness
    // This establishes the baseline for resetting between test sections
    snapshotMacroRegistry();

    /**
     * Run a single section test file
     */
    async function runSectionTest(sectionFile) {
        const path = `tests/core/scheme/compliance/chibi_revised/sections/${sectionFile}`;
        try {
            const testCode = await fileLoader(path);
            run(interpreter, testCode);
            const result = run(interpreter, '(test-report)');

            // Get counts before resetting
            const passes = run(interpreter, '*test-passes*');
            const failures = run(interpreter, '*test-failures*');
            const skips = run(interpreter, '*test-skips*');

            // Reset counters for next sections
            run(interpreter, '(set! *test-failures* 0)');
            run(interpreter, '(set! *test-passes* 0)');
            run(interpreter, '(set! *test-skips* 0)');

            return {
                success: (failures || 0) === 0,
                file: sectionFile,
                passes: passes || 0,
                failures: failures || 0,
                skips: skips || 0
            };
        } catch (error) {
            return { success: false, file: sectionFile, error: error.message, passes: 0, failures: 0, skips: 0 };
        }
    }

    /**
     * Run all section tests
     */
    async function runAllSections() {
        const results = [];
        for (const sectionFile of sectionFiles) {
            const result = await runSectionTest(sectionFile);
            results.push(result);
        }
        return results;
    }

    return {
        interpreter,
        run: (code) => run(interpreter, code),
        runSectionTest,
        runAllSections,
        sectionFiles
    };
}

export { sectionFiles };
