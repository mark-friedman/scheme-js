/**
 * R7RS Library Loader
 *
 * Loading libraries, defining them and importing them, from JavaScript: from
 * the analyzer's `import` and `define-library` forms, and from whatever starts
 * a program. The library system that does it is Scheme
 * (src/core/scheme/library_system.scm); this hands it what only the caller has
 * -- the interpreter to run a library's body on, and the environment its own
 * is made inside -- and converts what it returns.
 *
 * A file resolver that fetches files answers with promises, which a load
 * cannot wait for; `loadLibrary` fetches every file a library will read first,
 * and loads it from them.
 */

import { parse } from './reader.js';
import { toArray, list } from './cons.js';
import { Symbol } from './symbol.js';
import { globalContext } from './context.js';
import { SCHEME_PRIMITIVE } from './values.js';
import { SchemeLibraryError } from './errors.js';
import { stringValue } from '../primitives/string_class.js';
import { resolveNow } from '../primitives/library.js';

// Import from focused modules
import {
    libraryNameToKey,
    getFileResolver,
    getLibraryExports as _getLibraryExports,
    callLibrarySystem,
    currentLibraryRegistry,
    schemeLibraryName,
    exportsMap,
    exportsAlist,
    SYNTAX_KEYWORDS
} from './library_registry.js';
import { parseDefineLibrary, parseImportSet } from './library_parser.js';

// =============================================================================
// Re-exports for backwards compatibility
// =============================================================================

export {
    // Feature registry
    hasFeature,
    addFeature,
    getFeatures,
    evaluateFeatureRequirement,
    // Library registry
    setFileResolver,
    setLibraryLoadHook,
    withPrivateLibraries,
    libraryNameToKey,
    isLibraryLoaded,
    getLibraryExports,
    getLoadedLibraries,
    clearLibraryRegistry,
    registerBuiltinLibrary,
    SYNTAX_KEYWORDS
} from './library_registry.js';

export { parseDefineLibrary, parseImportSet } from './library_parser.js';

// =============================================================================
// Loaders
// =============================================================================

/**
 * The procedure the library system runs a library's body with: each form
 * analyzed and run on the caller's interpreter, in the library's environment,
 * with the library's scope the one its definitions are made in.
 * @param {Function} analyze - The analyze function.
 * @param {Object} interpreter - The interpreter.
 * @returns {Function} From a form and a library's environment.
 */
function evaluator(analyze, interpreter) {
    const evaluate = (form, env) => {
        globalContext.pushDefiningScope(env.libraryScope);
        try {
            interpreter.run(analyze(form), env);
        } finally {
            globalContext.popDefiningScope();
        }
        return undefined;
    };
    evaluate[SCHEME_PRIMITIVE] = true;
    return evaluate;
}

/**
 * A loader for the current registry (`make-loader` in library_system.scm).
 * @param {Function} analyze - The analyze function.
 * @param {Object} interpreter - The interpreter to run libraries' bodies on.
 * @param {Environment} baseEnv - The environment libraries' own are made in.
 * @param {Map<string, string>|null} [files=null] - Files fetched already, by
 *   path joined with `/`, read before asking the resolver; null to read
 *   every file through the resolver.
 * @returns {Object} The loader.
 */
function loaderFor(analyze, interpreter, baseEnv, files = null) {
    const registry = currentLibraryRegistry();
    const evaluate = evaluator(analyze, interpreter);
    if (files === null) return callLibrarySystem('registry-loader', registry, baseEnv, evaluate);
    const resolver = getFileResolver();
    const resolve = (path) => {
        const parts = toArray(path).map(stringValue);
        const key = parts.join('/');
        if (files.has(key)) return files.get(key);
        return resolver === null ? false : resolveNow(resolver, parts);
    };
    resolve[SCHEME_PRIMITIVE] = true;
    return callLibrarySystem('make-loader', registry, resolve, baseEnv, evaluate);
}

/**
 * A library's name as the resolver is given it.
 * @param {Array} name - The name, its parts strings, symbols or numbers.
 * @returns {string[]}
 */
function namePath(name) {
    return name.map(p => p instanceof Symbol ? p.name : String(p));
}

// =============================================================================
// Library Loading
// =============================================================================

/**
 * Loads a library by name, fetching first the files it and the libraries it
 * imports will read.
 * 
 * @param {string[]} libraryName - Library name parts
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} baseEnv - Base environment for primitives
 * @returns {Promise<Map>} The library's exports
 */
export async function loadLibrary(libraryName, analyze, interpreter, baseEnv) {
    const key = libraryNameToKey(libraryName);
    const cached = _getLibraryExports(key);
    if (cached) {
        return cached;
    }

    const fileResolver = getFileResolver();
    if (!fileResolver) {
        throw new SchemeLibraryError('no file resolver set - call setFileResolver first');
    }

    const path = namePath(libraryName);
    const source = await fileResolver(path);
    const forms = parse(source, { filename: path.join('/') });
    if (forms.length === 0) {
        throw new SchemeLibraryError('empty library file', key);
    }

    const files = await fetchDefinitionFiles(parseDefineLibrary(forms[0]), analyze, interpreter, baseEnv);
    files.set(path.join('/'), source);
    return exportsMap(callLibrarySystem('load-library', loaderFor(analyze, interpreter, baseEnv, files),
        schemeLibraryName(libraryName)));
}

/**
 * Loads a library by name synchronously, with a resolver that returns files
 * at once.
 * 
 * @param {string[]} libraryName - Library name parts
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} baseEnv - Base environment for primitives
 * @returns {Map} The library's exports
 */
export function loadLibrarySync(libraryName, analyze, interpreter, baseEnv) {
    return exportsMap(callLibrarySystem('load-library', loaderFor(analyze, interpreter, baseEnv),
        schemeLibraryName(libraryName)));
}

/**
 * Fetches, through a resolver that may answer with promises, the files a
 * library definition will read, and loads the libraries it imports.
 *
 * @param {Object} libDef - The parsed library definition (`parseDefineLibrary`)
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} baseEnv - Base environment for primitives
 * @returns {Promise<Map<string, string>>} The files, by path joined with `/`.
 */
async function fetchDefinitionFiles(libDef, analyze, interpreter, baseEnv) {
    const fileResolver = getFileResolver();
    const files = new Map();

    async function fetchFile(path) {
        const pathKey = path.join('/');
        if (!files.has(pathKey)) files.set(pathKey, await fileResolver(path));
        return files.get(pathKey);
    }

    for (const importSpec of libDef.imports) {
        await loadLibrary(importSpec.libraryName, analyze, interpreter, baseEnv);
    }

    if (fileResolver) {
        const directory = namePath(libDef.name.slice(0, -1));
        for (const includeFile of [...libDef.includes, ...libDef.includesCi]) {
            await fetchFile([...directory, includeFile]);
        }
        for (const declFile of libDef.includeLibraryDeclarations) {
            const declForms = parse(await fetchFile([...directory, declFile]), { filename: declFile });

            // Imports within library declarations
            for (const decl of declForms) {
                const declArr = toArray(decl);
                if (declArr.length === 0) continue;
                const declTag = declArr[0];
                if (declTag instanceof Symbol && declTag.name === 'import') {
                    for (let j = 1; j < declArr.length; j++) {
                        await loadLibrary(parseImportSet(declArr[j]).libraryName, analyze, interpreter, baseEnv);
                    }
                }
            }
        }
    }
    return files;
}

/**
 * Evaluates a parsed library definition and registers it, with a resolver
 * that may answer with promises.
 * 
 * @param {Object} libDef - The parsed library definition (`parseDefineLibrary`)
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} baseEnv - Base environment for primitives
 * @returns {Promise<Map>} The library's exports
 */
export async function evaluateLibraryDefinition(libDef, analyze, interpreter, baseEnv) {
    const files = await fetchDefinitionFiles(libDef, analyze, interpreter, baseEnv);
    return exportsMap(callLibrarySystem('define-library!', loaderFor(analyze, interpreter, baseEnv, files), libDef.form));
}

/**
 * Evaluates a parsed library definition synchronously.
 * 
 * @param {Object} libDef - The parsed library definition (`parseDefineLibrary`)
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} baseEnv - Base environment for primitives
 * @returns {Map} The library's exports
 */
export function evaluateLibraryDefinitionSync(libDef, analyze, interpreter, baseEnv) {
    return defineLibrary(libDef.form, analyze, interpreter, baseEnv);
}

/**
 * Defines a library from a `define-library` form, synchronously: a program's
 * own, which the load hook is not called with.
 *
 * @param {Cons} form - The form.
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} env - The environment the library's own is made in.
 * @returns {Map} The library's exports
 */
export function defineLibrary(form, analyze, interpreter, env) {
    return exportsMap(callLibrarySystem('define-library!', loaderFor(analyze, interpreter, env), form));
}

// =============================================================================
// Import Application
// =============================================================================

/**
 * Imports import sets, as an `import` form writes them, into an environment,
 * loading their libraries synchronously.
 *
 * @param {Array} specs - The import sets.
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} env - The environment.
 */
export function importLibraries(specs, analyze, interpreter, env) {
    callLibrarySystem('import-sets!', loaderFor(analyze, interpreter, env), env, list(...specs));
}

/**
 * Binds every export of a library in an environment, under its own name: a
 * syntactic keyword in the analyzer's tables, anything else in the
 * environment. An import set's filters are applied by importing it
 * (`importLibraries`).
 *
 * @param {Environment} env - Target environment
 * @param {Map} exports - Source library exports
 * @param {Object} [importSpec] - Which library they are, `{ libraryName }`.
 */
export function applyImports(env, exports, importSpec) {
    if (importSpec?.steps?.length > 0) {
        throw new SchemeLibraryError('applyImports imports every export; import an import set with importLibraries');
    }
    callLibrarySystem('import-into!', env, exportsAlist(exports), null);
}

// =============================================================================
// Primitive Exports
// =============================================================================

/**
 * Creates (scheme primitives) exports from the global environment.
 * 
 * @param {Environment} globalEnv - The global environment with primitives
 * @returns {Map<string, *>} The exports map
 */
export function createPrimitiveExports(globalEnv) {
    const exports = new Map();

    // List of (scheme primitives) exports
    // This MUST match what is actually installed in the global environment by primitives/index.js
    const primitiveExports = [
        // Equivalence
        'eq?', 'eqv?', 'not', 'boolean?', 'boolean=?', 'symbol?',
        // Math - basic arithmetic
        '+', '-', '*', '/',
        // Math - binary comparisons (used by core.scm to build variadic versions)
        '%num=', '%num<', '%num>', '%num<=', '%num>=',
        // Math - integer division
        'modulo', 'quotient', 'remainder',
        // Math - type predicates
        'number?', 'real?', 'rational?', 'integer?', 'exact-integer?',
        'finite?', 'infinite?', 'nan?',
        // Math - functions requiring Math.*
        'abs', 'floor', 'ceiling', 'truncate', 'round',
        'expt', 'sqrt', 'square', 'exact-integer-sqrt',
        'sin', 'cos', 'tan', 'asin', 'acos', 'atan', 'log', 'exp',
        'exact', 'inexact', 'inexact->exact',
        'numerator', 'denominator',
        'make-rectangular', 'make-polar', 'real-part', 'imag-part', 'magnitude', 'angle',
        // Pairs and lists
        'cons', 'car', 'cdr', 'pair?', 'null?', 'list?',
        'set-car!', 'set-cdr!',
        'list', 'append',
        // Symbols
        'symbol->string', 'string->symbol',
        // Strings
        'string?', 'string-append', 'number->string',
        // Vectors
        'vector?', 'make-vector', 'vector', 'vector-length',
        'vector-ref', 'vector-set!',
        'vector->list', 'list->vector',
        // Records (Low-level primitives for define-record-type)
        'make-record-type', 'record-constructor', 'record-predicate',
        'record-accessor', 'record-modifier',
        // Control
        'apply', 'values', 'call-with-values',
        'eval', 'interaction-environment',
        'dynamic-wind',
        'call-with-current-continuation', 'call/cc',
        'procedure?',
        // Exceptions
        'raise', 'raise-continuable', 'with-exception-handler',
        'error', 'error-object?', 'error-object-message', 'error-object-irritants',
        // JS Interop
        'scheme->js', 'scheme->js-deep',
        'js->scheme', 'js->scheme-deep',
        'register-js-object-record',
        'js-ref', 'js-set!',
    ];

    for (const name of primitiveExports) {
        // Use findEnv to check if binding exists without throwing
        if (globalEnv.findEnv(name) !== null) {
            exports.set(name, globalEnv.lookup(name));
        } else {
            // Warn about missing primitives that we expect to be there
            console.warn(`Warning: (scheme primitives) claims export '${name}' but it is not in the global environment.`);
        }
    }

    return exports;
}
