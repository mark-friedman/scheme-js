/**
 * Tests for the R7RS Library Loader
 */

import { assert, createTestLogger } from '../harness/helpers.js';
import {
    parseDefineLibrary,
    parseImportSet,
    libraryNameToKey,
    clearLibraryRegistry,
    isLibraryLoaded,
    loadLibrary,
    setFileResolver,
    hasFeature,
    addFeature,
    getFeatures,
    evaluateFeatureRequirement,
    importLibraries,
    loadLibrarySync,
    getLoadedLibraries
} from '../../src/core/interpreter/library_loader.js';
import { registerLibrary } from '../../src/core/interpreter/library_registry.js';
import { seedLibrarySystem } from '../../src/core/interpreter/library_seed.js';
import { callSchemeProcedure } from '../../src/core/interpreter/values.js';
import { list } from '../../src/core/interpreter/cons.js';
import { Environment } from '../../src/core/interpreter/environment.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { Interpreter } from '../../src/core/interpreter/interpreter.js';
import { createGlobalEnvironment } from '../../src/core/primitives/index.js';
import { intern } from '../../src/core/interpreter/symbol.js';

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
        onlySpec.steps[0].names.length, 2);

    // 5. Test import with prefix
    const prefixImport = parse("(prefix (scheme base) base:)")[0];
    const prefixSpec = parseImportSet(prefixImport);
    assert(logger, "parseImportSet prefix",
        prefixSpec.steps[0].prefix, "base:");

    // 5a. Import sets nest, each filtering the names the set inside it
    // provides (R7RS 5.6.1); rename takes (from to) pairs.
    registerLibrary('lib', new Map([['car', 1], ['cdr', 2], ['cons', 3]]), null);
    const importedBy = (text) => {
        const env = new Environment(null);
        importLibraries([parse(text)[0]], analyze, null, env);
        return [...env.bindings.keys()].sort().join(' ');
    };
    assert(logger, "parseImportSet rename pairs",
        JSON.stringify(parseImportSet(parse("(rename (scheme base) (car first) (cdr rest))")[0]).steps),
        JSON.stringify([{ kind: 'rename', renames: [{ from: 'car', to: 'first' }, { from: 'cdr', to: 'rest' }] }]));
    assert(logger, "rename renames only the names it lists",
        importedBy("(rename (lib) (car first) (cdr rest))"), "cons first rest");
    assert(logger, "only sees the names a prefix inside it made",
        importedBy("(only (prefix (lib) p:) p:car)"), "p:car");
    assert(logger, "prefix applies to what an only inside it kept",
        importedBy("(prefix (only (lib) car) p:)"), "p:car");
    assert(logger, "rename sees the names a prefix inside it made",
        importedBy("(rename (prefix (lib) p:) (p:car first))"), "first p:cdr p:cons");
    assert(logger, "except sees the names a rename inside it made",
        importedBy("(except (rename (lib) (car first)) first)"), "cdr cons");
    assert(logger, "prefix renames what a rename inside it made",
        importedBy("(prefix (rename (lib) (car first)) p:)"), "p:cdr p:cons p:first");

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
            typeof exports.get('add1'), 'function'); // Callable Scheme closure
        assert(logger, "loadLibrary caches library",
            isLibraryLoaded('test.simple'), true);
    } catch (e) {
        logger.fail(`loadLibrary failed: ${e.message}`);
    }

    clearLibraryRegistry();

    // 7. Test library with (scheme base) dependency
    const { registerBuiltinLibrary, createPrimitiveExports } =
        await import('../../src/core/interpreter/library_loader.js');

    // Register (scheme base) first (mocking it with primitives for this test)
    const schemeBaseExports = createPrimitiveExports(globalEnv);
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

    // 7a. The library system runs apart from programs, on an interpreter of
    // its own: a program that binds, at its own top level, names the library
    // system's Scheme uses changes nothing about how libraries load, and the
    // library system's own libraries are not the program's.
    clearLibraryRegistry();
    registerBuiltinLibrary(['scheme', 'base'], schemeBaseExports, globalEnv);
    for (const name of ['car', 'cdr', 'cons', 'assoc', 'append', 'map', 'for-each']) {
        globalEnv.define(name, () => { throw new Error(`the program's own ${name}`); });
    }
    setFileResolver((name) => ({
        'test.apart': '(define-library (test apart) (export seven) (import (prefix (scheme base) b:)) (include "apart.scm"))',
        'test.apart.scm': '(define seven (b:+ 3 4))'
    })[name.join('.')]);
    try {
        const apartExports = loadLibrarySync(['test', 'apart'], analyze, interpreter, globalEnv);
        assert(logger, "a program's own bindings do not change loading", apartExports.get('seven'), 7);
        assert(logger, "and the library system's libraries are not the program's",
            getLoadedLibraries(), ['scheme.base', 'test.apart']);
    } catch (e) {
        logger.fail(`loading beside a program's own bindings failed: ${e.message}`);
    } finally {
        for (const name of ['car', 'cdr', 'cons', 'assoc', 'append', 'map', 'for-each']) globalEnv.bindings.delete(name);
    }

    // 7b. The seed installs the prebuilt tables of the libraries it loads, so
    // that the library system runs compiled; given none, it runs interpreted,
    // and works the same.
    const keyBy = (system) => String(callSchemeProcedure(system.get('library-key'), [list(intern('srfi'), 1n)]));
    const compiledSystem = seedLibrarySystem();
    assert(logger, "the library system runs compiled", compiledSystem.get('load-library').$compiled === true, true);
    assert(logger, "and works", keyBy(compiledSystem), 'srfi.1');
    const interpretedSystem = seedLibrarySystem({});
    assert(logger, "given no tables, it runs interpreted", interpretedSystem.get('load-library').$compiled === true, false);
    assert(logger, "and works the same", keyBy(interpretedSystem), 'srfi.1');

    clearLibraryRegistry();

    // 8. Test cond-expand feature detection
    logger.title('Testing cond-expand...');

    // Test hasFeature
    assert(logger, "hasFeature r7rs", hasFeature('r7rs'), true);
    assert(logger, "hasFeature scheme-js", hasFeature('scheme-js'), true);
    assert(logger, "hasFeature unknown", hasFeature('unknown-feature'), false);

    // Test getFeatures
    const allFeatures = getFeatures();
    assert(logger, "getFeatures includes r7rs", allFeatures.includes('r7rs'), true);

    // Test evaluateFeatureRequirement with simple symbol
    assert(logger, "evalFeature simple r7rs",
        evaluateFeatureRequirement(intern('r7rs')), true);
    assert(logger, "evalFeature simple unknown",
        evaluateFeatureRequirement(intern('unknown')), false);

    // Test evaluateFeatureRequirement with (and ...)
    const andReq = parse("(and r7rs scheme-js)")[0];
    assert(logger, "evalFeature (and r7rs scheme-js)",
        evaluateFeatureRequirement(andReq), true);

    const andReqFail = parse("(and r7rs unknown)")[0];
    assert(logger, "evalFeature (and r7rs unknown)",
        evaluateFeatureRequirement(andReqFail), false);

    // Test evaluateFeatureRequirement with (or ...)
    const orReq = parse("(or unknown r7rs)")[0];
    assert(logger, "evalFeature (or unknown r7rs)",
        evaluateFeatureRequirement(orReq), true);

    const orReqFail = parse("(or unknown missing)")[0];
    assert(logger, "evalFeature (or unknown missing)",
        evaluateFeatureRequirement(orReqFail), false);

    // Test evaluateFeatureRequirement with (not ...)
    const notReq = parse("(not unknown)")[0];
    assert(logger, "evalFeature (not unknown)",
        evaluateFeatureRequirement(notReq), true);

    const notReqFail = parse("(not r7rs)")[0];
    assert(logger, "evalFeature (not r7rs)",
        evaluateFeatureRequirement(notReqFail), false);

    // 9. Test parseDefineLibrary with cond-expand
    const libWithCondExpand = `
        (define-library (test cond)
          (export common-export)
          (cond-expand
            (r7rs
              (export r7rs-only)
              (begin (define r7rs-only 'yes)))
            (else
              (begin (define r7rs-only 'no))))
          (begin
            (define common-export 42)))
    `;
    const condLibForm = parse(libWithCondExpand)[0];
    const condLibDef = parseDefineLibrary(condLibForm);

    assert(logger, "cond-expand export count",
        condLibDef.exports.length, 2);  // common-export + r7rs-only
    assert(logger, "cond-expand body count",
        condLibDef.body.length, 2);     // define r7rs-only + define common-export

    // 10. Test cond-expand with else clause (when first clause fails)
    const libWithElse = `
        (define-library (test else)
          (cond-expand
            (unknown-feature
              (begin (define result 'unknown)))
            (else
              (begin (define result 'else-branch))))
          (export result))
    `;
    const elseLibForm = parse(libWithElse)[0];
    const elseLibDef = parseDefineLibrary(elseLibForm);

    assert(logger, "cond-expand else body count",
        elseLibDef.body.length, 1);  // Only else branch should match

    // 11. Test parse with case-folding
    logger.title('Testing include-ci (case-folding)...');

    const caseFoldedSymbols = parse("(DEFINE FOO bar)", { caseFold: true })[0];
    const caseFoldedArr = caseFoldedSymbols.toArray();
    assert(logger, "caseFold: DEFINE becomes define",
        caseFoldedArr[0].name, 'define');
    assert(logger, "caseFold: FOO becomes foo",
        caseFoldedArr[1].name, 'foo');
    assert(logger, "caseFold: bar stays bar",
        caseFoldedArr[2].name, 'bar');

    // Normal parse should NOT case-fold
    const normalSymbols = parse("(DEFINE FOO bar)")[0];
    const normalArr = normalSymbols.toArray();
    assert(logger, "normal: DEFINE stays DEFINE",
        normalArr[0].name, 'DEFINE');

    // 12. Test parseDefineLibrary with include-ci
    const libWithIncludeCi = `
        (define-library (test ci)
          (include-ci "legacy.scm")
          (export result))
    `;
    const ciLibForm = parse(libWithIncludeCi)[0];
    const ciLibDef = parseDefineLibrary(ciLibForm);

    assert(logger, "include-ci parsed",
        ciLibDef.includesCi.length, 1);
    assert(logger, "include-ci filename",
        ciLibDef.includesCi[0], "legacy.scm");

    // 13. Test parseDefineLibrary with include-library-declarations
    const libWithIncludeDecl = `
        (define-library (test decl)
          (include-library-declarations "common-exports.scm")
          (begin (define x 1)))
    `;
    const declLibForm = parse(libWithIncludeDecl)[0];
    const declLibDef = parseDefineLibrary(declLibForm);

    assert(logger, "include-library-declarations parsed",
        declLibDef.includeLibraryDeclarations.length, 1);
    assert(logger, "include-library-declarations filename",
        declLibDef.includeLibraryDeclarations[0], "common-exports.scm");

    clearLibraryRegistry();
}

