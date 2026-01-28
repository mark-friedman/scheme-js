/**
 * Chapter-based R7RS Compliance Test Runner Library
 * 
 * Shared library for running the chapter-based R7RS compliance tests
 * (chapter_3.scm through chapter_6.scm) in both Node.js and browser environments.
 */

import { createInterpreter } from '../../../../src/core/interpreter/index.js';
import { run, deepEqual, safeStringify, toJS } from '../../../harness/helpers.js';
import { loadLibrary, applyImports, setFileResolver, registerBuiltinLibrary, createPrimitiveExports } from '../../../../src/core/interpreter/library_loader.js';
import { analyze } from '../../../../src/core/interpreter/analyzer.js';

// Chapter files in order
const chapterFiles = [
    'chapter_3.scm',
    'chapter_4.scm',
    'chapter_5.scm',
    'chapter_6.scm'
];

/**
 * Creates and configures an interpreter for chapter-based compliance testing.
 * @param {Function} fileLoader - Function to load files by path
 * @param {Object} logger - Logger object with pass/fail methods
 * @returns {Object} - { interpreter, run, runChapterTest, runAllChapters }
 */
export async function createChapterComplianceRunner(fileLoader, logger) {
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
        // Double check with deepEqual in JS to catch false negatives (e.g. BigInt vs Number)
        const trulyPassed = passed || deepEqual(toJS(expected), toJS(actual));

        if (trulyPassed) {
            logger.pass(`${name}`);
        } else {
            logger.fail(`${name} (Expected: ${safeStringify(expected)}, Got: ${safeStringify(actual)})`);
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

    // Inject title reporter for test-group
    interpreter.globalEnv.bindings.set('native-log-title', (title) => {
        if (logger.title) {
            logger.title(title);
        } else {
            console.log(`\n=== ${title} ===`);
        }
    });

    const harnessCode = await fileLoader('tests/core/scheme/test.scm');
    run(interpreter, harnessCode);

    /**
     * Run a single chapter test file
     */
    async function runChapterTest(chapterFile) {
        const path = `tests/core/scheme/compliance/${chapterFile}`;
        try {
            const testCode = await fileLoader(path);
            run(interpreter, testCode);
            const result = run(interpreter, '(test-report)');

            // Get counts before resetting
            // Cast to Number as Scheme integers are now BigInt
            const passes = Number(run(interpreter, '*test-passes*'));
            const failures = Number(run(interpreter, '*test-failures*'));
            const skips = Number(run(interpreter, '*test-skips*'));

            // Reset counters for next chapter
            run(interpreter, '(set! *test-failures* 0)');
            run(interpreter, '(set! *test-passes* 0)');
            run(interpreter, '(set! *test-skips* 0)');

            return {
                success: (failures || 0) === 0,
                file: chapterFile,
                passes: passes || 0,
                failures: failures || 0,
                skips: skips || 0
            };
        } catch (error) {
            return { success: false, file: chapterFile, error: error.message, passes: 0, failures: 0, skips: 0 };
        }
    }

    /**
     * Run all chapter tests
     */
    async function runAllChapters() {
        const results = [];
        for (const chapterFile of chapterFiles) {
            const result = await runChapterTest(chapterFile);
            results.push(result);
        }
        return results;
    }

    return {
        interpreter,
        run: (code) => run(interpreter, code),
        runChapterTest,
        runAllChapters,
        chapterFiles
    };
}

export { chapterFiles };
