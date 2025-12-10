/**
 * Tests for the R7RS Library Loader
 */

import { assert, createTestLogger } from '../helpers.js';
import {
    parseDefineLibrary,
    parseImportSet,
    libraryNameToKey,
    clearLibraryRegistry,
    isLibraryLoaded,
    loadLibrary,
    setFileResolver
} from '../../src/core/interpreter/library_loader.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { Interpreter } from '../../src/core/interpreter/interpreter.js';
import { createGlobalEnvironment } from '../../src/core/primitives/index.js';

export async function runLibraryLoaderTests(logger) {
    logger.title('Running Library Loader Tests...');

    // Clean slate
    clearLibraryRegistry();

    // 1. Test libraryNameToKey
    const key1 = libraryNameToKey(['scheme', 'base']);
    assert(logger, "libraryNameToKey simple", key1, "scheme.base");

    // 2. Test parseDefineLibrary
    const libSource = `
        (define-library (test example)
          (export foo bar)
          (import (scheme base))
          (begin
            (define foo 42)
            (define bar "hello")))
    `;
    const libForm = parse(libSource)[0];
    const libDef = parseDefineLibrary(libForm);

    assert(logger, "parseDefineLibrary name",
        libraryNameToKey(libDef.name), "test.example");
    assert(logger, "parseDefineLibrary export count",
        libDef.exports.length, 2);
    assert(logger, "parseDefineLibrary export 1",
        libDef.exports[0].external, "foo");
    assert(logger, "parseDefineLibrary import count",
        libDef.imports.length, 1);
    assert(logger, "parseDefineLibrary body count",
        libDef.body.length, 2);

    // 3. Test parseImportSet
    const simpleImport = parse("(scheme base)")[0];
    const simpleSpec = parseImportSet(simpleImport);
    assert(logger, "parseImportSet simple",
        simpleSpec.libraryName.join('.'), "scheme.base");

    // 4. Test import with only
    const onlyImport = parse("(only (scheme base) cons car)")[0];
    const onlySpec = parseImportSet(onlyImport);
    assert(logger, "parseImportSet only library",
        onlySpec.libraryName.join('.'), "scheme.base");
    assert(logger, "parseImportSet only filter",
        onlySpec.only.length, 2);

    // 5. Test import with prefix
    const prefixImport = parse("(prefix (scheme base) base:)")[0];
    const prefixSpec = parseImportSet(prefixImport);
    assert(logger, "parseImportSet prefix",
        prefixSpec.prefix, "base:");

    // 6. Test minimal library loading (no dependencies)
    const interpreter = new Interpreter();
    const globalEnv = createGlobalEnvironment(interpreter);
    interpreter.setGlobalEnv(globalEnv);

    // Create a mock file resolver for testing
    const testLibraries = {
        'test.simple': `
            (define-library (test simple)
              (export add1 value)
              (begin
                (define value 100)
                (define (add1 x) (+ x 1))))
        `
    };

    setFileResolver(async (name) => {
        const key = name.join('.');
        if (testLibraries[key]) {
            return testLibraries[key];
        }
        throw new Error(`Test library not found: ${key}`);
    });

    try {
        const exports = await loadLibrary(
            ['test', 'simple'],
            analyze,
            interpreter,
            globalEnv
        );

        assert(logger, "loadLibrary returns exports",
            exports instanceof Map, true);
        assert(logger, "loadLibrary export value",
            exports.get('value'), 100);
        assert(logger, "loadLibrary export function",
            typeof exports.get('add1'), 'object'); // Closure
        assert(logger, "loadLibrary caches library",
            isLibraryLoaded('test.simple'), true);
    } catch (e) {
        logger.fail(`loadLibrary failed: ${e.message}`);
    }

    clearLibraryRegistry();

    // 7. Test library with (scheme base) dependency
    const { registerBuiltinLibrary, createSchemeBaseExports } =
        await import('../../src/core/interpreter/library_loader.js');

    // Register (scheme base) first
    const schemeBaseExports = createSchemeBaseExports(globalEnv);
    registerBuiltinLibrary(['scheme', 'base'], schemeBaseExports, globalEnv);

    assert(logger, "registerBuiltinLibrary works",
        isLibraryLoaded('scheme.base'), true);
    assert(logger, "scheme.base has cons",
        schemeBaseExports.has('cons'), true);

    // Now test loading a library that imports (scheme base)
    const testLibWithDeps = {
        'test.withdeps': `
            (define-library (test withdeps)
              (export double)
              (import (scheme base))
              (begin
                (define (double x) (+ x x))))
        `
    };

    setFileResolver(async (name) => {
        const key = name.join('.');
        if (testLibWithDeps[key]) {
            return testLibWithDeps[key];
        }
        throw new Error(`Test library not found: ${key}`);
    });

    try {
        const depsExports = await loadLibrary(
            ['test', 'withdeps'],
            analyze,
            interpreter,
            globalEnv
        );

        assert(logger, "loadLibrary with deps returns exports",
            depsExports instanceof Map, true);
        assert(logger, "loadLibrary with deps has double",
            depsExports.has('double'), true);

        // Call the exported function through the interpreter
        const doubleFn = depsExports.get('double');
        assert(logger, "exported function is closure",
            doubleFn !== undefined, true);
    } catch (e) {
        logger.fail(`loadLibrary with deps failed: ${e.message}`);
    }

    clearLibraryRegistry();
}

