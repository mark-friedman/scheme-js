/**
 * @fileoverview Runs an R7RS conformance suite with the standard library
 * interpreted or compiled.
 *
 * The two suites -- the chapter tests (`chapter_3.scm` to `chapter_6.scm`) and
 * Chibi's R7RS tests, split into sections -- are run by the Node runners
 * beside this file, by the two UI pages, and inside `npm test` in both library
 * configurations (`compliance_tests.js`). Every browser page installs the
 * standard library compiled, from the prebuilt tables, so conformance measured
 * only with the library interpreted says nothing about what a browser runs.
 *
 * A run is synchronous and loads its libraries apart from every other library
 * in the process (`withPrivateLibraries`): the suites import libraries while
 * they run -- `(environment '(scheme base))` -- and loading them into the
 * registry the rest of `npm test` shares, or installing compiled tables into
 * it, would leak one configuration into the next and into other tests. So the
 * test files are read first, and libraries come from the bundled sources the
 * browser loads from, which need no asynchronous read.
 */

import { createInterpreter } from '../../../../src/core/interpreter/index.js';
import { run } from '../../../harness/helpers.js';
import { writeString } from '../../../../src/core/primitives/io/printer.js';
import { SCHEME_PRIMITIVE } from '../../../../src/core/interpreter/values.js';
import { loadLibrarySync, applyImports } from '../../../../src/core/interpreter/library_loader.js';
import { withPrivateLibraries } from '../../../../src/core/interpreter/library_registry.js';
import { analyze } from '../../../../src/core/interpreter/expand.js';
import {
    globalMacroRegistry, resetGlobalMacroRegistry, snapshotMacroRegistry
} from '../../../../src/core/interpreter/macro_registry.js';
import { BUNDLED_SOURCES } from '../../../../src/packaging/bundled_libraries.js';
import { pageLibraries } from '../../../harness/page_libraries.js';

/**
 * The libraries a suite runs with, all imported into its global environment.
 * @type {Array<Array<string>>}
 */
const LIBRARIES = [
    ['scheme', 'base'], ['scheme', 'repl'], ['scheme', 'case-lambda'], ['scheme', 'lazy'],
    ['scheme', 'char'], ['scheme', 'cxr'], ['scheme', 'read'], ['scheme', 'write'],
    ['scheme', 'eval'], ['scheme', 'time'], ['scheme', 'process-context'], ['scheme', 'file']
];

/**
 * The two suites.
 *
 * `isolateMacros` starts from an empty macro registry, as the Chibi runner
 * always has. `prelude`, where there is one, runs after the harness: Chibi's
 * tests are compared as Chibi compares them, an inexact value within an
 * epsilon of the one expected (`chibi_revised/test-equal.scm`). A test passes when the Scheme harness says it does: the Chibi
 * runner also counted a failure as a pass when the two values agreed once
 * converted to JavaScript, which hid an exact integer against an inexact one.
 *
 * @type {Object<string, {title: string, dir: string, files: Array<string>, isolateMacros: boolean,
 *   prelude?: string}>}
 */
export const SUITES = {
    chapters: {
        title: 'R7RS chapter tests',
        dir: 'tests/core/scheme/compliance/',
        files: ['chapter_3.scm', 'chapter_4.scm', 'chapter_5.scm', 'chapter_6.scm'],
        isolateMacros: false
    },
    chibi: {
        title: "Chibi's R7RS tests",
        dir: 'tests/core/scheme/compliance/chibi_revised/sections/',
        files: [
            '4.1-primitives.scm', '4.2-derived.scm', '4.3-macros.scm', '5-program-structure.scm',
            '6.1-equivalence.scm', '6.2-numbers.scm', '6.3-booleans.scm', '6.4-lists.scm',
            '6.5-symbols.scm', '6.6-characters.scm', '6.7-strings.scm', '6.8-vectors.scm',
            '6.9-bytevectors.scm', '6.10-control.scm', '6.11-exceptions.scm', '6.12-environments.scm',
            '6.13-io.scm', '6.14-system.scm', '7.1-read-syntax.scm', '7.1-numeric-syntax.scm'
        ],
        isolateMacros: true,
        prelude: 'tests/core/scheme/compliance/chibi_revised/test-equal.scm'
    }
};

/**
 * Reads the test harness and a suite's files.
 * @param {Object} suite - One of `SUITES`.
 * @param {Function} fileLoader - Reads a file by its path from the project
 *   root, returning a promise of its text.
 * @returns {Promise<{harness: string, files: Map<string, string>}>} The sources,
 *   the harness followed by the suite's prelude, if it has one.
 */
export async function loadSuiteSources(suite, fileLoader) {
    let harness = await fileLoader('tests/core/scheme/test.scm');
    if (suite.prelude) harness += '\n' + await fileLoader(suite.prelude);
    const files = new Map();
    for (const file of suite.files) files.set(file, await fileLoader(suite.dir + file));
    return { harness, files };
}

/**
 * Finds a library's source among the bundled sources, as the browser does.
 * @param {Array<string>} libraryName - The library's name.
 * @returns {string} Its source.
 * @throws {Error} If it is not bundled.
 */
function bundledSource(libraryName) {
    const fileName = libraryName[libraryName.length - 1];
    const source = BUNDLED_SOURCES[`${fileName}.sld`] ?? BUNDLED_SOURCES[`${fileName}.scm`] ?? BUNDLED_SOURCES[fileName];
    if (source === undefined) throw new Error(`Library not found in bundled sources: ${libraryName.join('/')}`);
    return source;
}

/**
 * Runs a suite.
 *
 * @param {Object} suite - One of `SUITES`.
 * @param {{harness: string, files: Map<string, string>}} sources - From
 *   `loadSuiteSources`.
 * @param {Object} logger - Receives `pass`, `fail`, and where it has them
 *   `skip` and `title`, for each test.
 * @param {Object} [options] - Options.
 * @param {boolean} [options.compiledLibraries=false] - Restore each shipped
 *   library from its prebuilt table, its source never read, as the browser
 *   bundle does (`src/packaging/scheme_entry.js`, `harness/page_libraries.js`),
 *   rather than read it and run it interpreted.
 * @param {Array<string>} [options.files] - The files to run, in order; all of
 *   the suite's by default.
 * @returns {{results: Array<Object>, installation: Map<string, Object>,
 *   fromSource: Set<string>}} For each file, its name and counts, and `error`
 *   if it crashed; and, with the libraries compiled, what installing each
 *   library's table did and the shipped libraries read from their source all
 *   the same, their tables stale, each by key, `scheme.core`.
 */
export function runSuite(suite, sources, logger, { compiledLibraries = false, files = suite.files } = {}) {
    const libraries = compiledLibraries
        ? pageLibraries({
            resolve: bundledSource,
            isShipped: (name) => BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] !== undefined
        })
        : { resolve: bundledSource, hook: null, restorer: null, installation: new Map(), fromSource: new Set() };

    // Macros defined by the suite go into the registry the whole process
    // shares, so it is given back as it was.
    const savedMacros = new Map(globalMacroRegistry.macros);
    try {
        const results = withPrivateLibraries(
            { resolver: libraries.resolve, hook: libraries.hook, restorer: libraries.restorer },
            () => runInPrivate(suite, sources, files, logger));
        return { results, installation: libraries.installation, fromSource: libraries.fromSource };
    } finally {
        globalMacroRegistry.macros.clear();
        for (const [name, macro] of savedMacros) globalMacroRegistry.macros.set(name, macro);
    }
}

/**
 * The body of `runSuite`, inside its private library registry.
 * @param {Object} suite - One of `SUITES`.
 * @param {{harness: string, files: Map<string, string>}} sources - The sources.
 * @param {Array<string>} files - The files to run.
 * @param {Object} logger - The logger.
 * @returns {Array<Object>} Each file's name and counts, and `error` if it
 *   crashed.
 */
function runInPrivate(suite, sources, files, logger) {
    if (suite.isolateMacros) resetGlobalMacroRegistry();
    const { interpreter } = createInterpreter();
    const env = interpreter.globalEnv;
    for (const name of LIBRARIES) {
        applyImports(env, loadLibrarySync(name, analyze, interpreter, env), { libraryName: name });
    }

    // The reporter takes Scheme values as they are, and writes them as Scheme
    // does: an exact integer past 2^53, which Chibi's tests compute, has no
    // JavaScript number, and converting one to report it would throw.
    const nameOf = (name) => (typeof name === 'string' ? name : writeString(name));
    const reportTestResult = (name, passed, expected, actual) => {
        if (passed) {
            logger.pass(`${nameOf(name)}`);
        } else {
            logger.fail(`${nameOf(name)} (Expected: ${writeString(expected)}, Got: ${writeString(actual)})`);
        }
    };
    reportTestResult[SCHEME_PRIMITIVE] = true;
    env.bindings.set('native-report-test-result', reportTestResult);
    env.bindings.set('native-report-test-skip', (name, reason) => {
        if (logger.skip) logger.skip(`${name} (Reason: ${reason})`);
        else console.log(`⏭️ SKIP: ${name} - ${reason}`);
    });
    env.bindings.set('native-log-title', (title) => {
        if (logger.title) logger.title(title);
        else console.log(`\n=== ${title} ===`);
    });

    run(interpreter, sources.harness);
    if (suite.isolateMacros) snapshotMacroRegistry();

    const results = [];
    for (const file of files) {
        try {
            run(interpreter, sources.files.get(file));
            run(interpreter, '(test-report)');
            // Scheme integers are BigInt.
            const passes = Number(run(interpreter, '*test-passes*'));
            const failures = Number(run(interpreter, '*test-failures*'));
            const skips = Number(run(interpreter, '*test-skips*'));
            run(interpreter, '(set! *test-failures* 0)');
            run(interpreter, '(set! *test-passes* 0)');
            run(interpreter, '(set! *test-skips* 0)');
            results.push({ file, passes: passes || 0, failures: failures || 0, skips: skips || 0 });
        } catch (error) {
            results.push({ file, error: error.message, passes: 0, failures: 0, skips: 0 });
        }
    }
    return results;
}
