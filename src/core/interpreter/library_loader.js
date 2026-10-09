/**
 * R7RS Library Loader
 *
 * Loading libraries, defining them and importing them, from JavaScript: from
 * the expander's `import` and `define-library` forms, and from whatever starts
 * a program. The library system that does it is Scheme
 * (src/core/scheme/library_system.scm); this hands it what only the caller has
 * -- the interpreter to run a library's body on, and the environment its own
 * is made inside -- and converts what it returns.
 *
 * A file resolver that fetches files answers with promises, which a load
 * cannot wait for; `loadLibrary` fetches every file a library will read first,
 * asking the library system which, and loads it from them.
 */

import { toArray, list } from './cons.js';
import { globalContext } from './context.js';
import { SCHEME_PRIMITIVE } from './values.js';
import { SchemeLibraryError } from './errors.js';
import { stringValue } from '../primitives/string_class.js';
import { resolveNow, makeScopedEnvironment } from '../primitives/library.js';

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
    setLibraryRestorer,
    withPrivateLibraries,
    libraryNameToKey,
    isLibraryLoaded,
    getLibraryExports,
    getLoadedLibraries,
    clearLibraryRegistry,
    registerBuiltinLibrary,
    SYNTAX_KEYWORDS
} from './library_registry.js';

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
    const resolve = (path, library) => {
        const parts = toArray(path).map(stringValue);
        const key = parts.join('/');
        if (files.has(key)) return files.get(key);
        if (resolver === null) return false;
        return resolveNow(resolver, parts, library === undefined ? undefined : toArray(library).map(stringValue));
    };
    resolve[SCHEME_PRIMITIVE] = true;
    return callLibrarySystem('make-loader', registry, resolve, baseEnv, evaluate);
}

// =============================================================================
// Library Loading
// =============================================================================

/**
 * Loads a library by name, with a resolver that may answer with promises:
 * every file the load will read fetched first.
 * 
 * @param {string[]} libraryName - Library name parts
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} baseEnv - Base environment for primitives
 * @returns {Promise<Map>} The library's exports
 */
export async function loadLibrary(libraryName, analyze, interpreter, baseEnv) {
    const name = schemeLibraryName(libraryName);
    const files = await fetchWanted((loader) => callLibrarySystem('files-wanted', loader, name));
    return exportsMap(callLibrarySystem('load-library', loaderFor(analyze, interpreter, baseEnv, files), name));
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
 * Fetches, through the resolver, the files a load will read: asks the
 * library system which it lacks of those it has (`files-wanted` in
 * library_system.scm), fetches them together, and asks again, until it lacks
 * none.
 *
 * @param {(loader: Object) => Cons} wantedBy - The paths still wanted, given
 *   a loader that has the files fetched so far.
 * @returns {Promise<Map<string, string>>} The files, by path joined with `/`.
 */
async function fetchWanted(wantedBy) {
    const files = new Map();
    const resolver = getFileResolver();
    const atHand = (path) => files.get(toArray(path).map(stringValue).join('/')) ?? false;
    atHand[SCHEME_PRIMITIVE] = true;
    const loader = callLibrarySystem('make-loader', currentLibraryRegistry(), atHand, false, false);
    for (let wanted = toArray(wantedBy(loader)); wanted.length > 0; wanted = toArray(wantedBy(loader))) {
        if (resolver === null) {
            throw new SchemeLibraryError('no file resolver set - call setFileResolver first');
        }
        await Promise.all(wanted.map(async (path) => {
            const parts = toArray(path).map(stringValue);
            const source = await resolver(parts);
            if (typeof source !== 'string') {
                throw new SchemeLibraryError(`the file resolver gave no text for ${parts.join('/')}`);
            }
            files.set(parts.join('/'), source);
        }));
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
    const files = await fetchWanted((loader) => callLibrarySystem('definition-files-wanted', loader, libDef.form));
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
 * A `define-library` form's parts, its `cond-expand` declarations decided
 * (`define-library-parts` in library_system.scm), for tools that read which
 * files a library is made of.
 *
 * @param {Cons} form - The form.
 * @returns {{name: Array, exports: Array<{internal: string, external: string}>,
 *   imports: Array<{libraryName: string[]}>, body: Array, includes: string[],
 *   includesCi: string[], includeLibraryDeclarations: string[], form: Cons}}
 *   Its name as written, its exports, the libraries it imports, its body's
 *   forms, the files of its three kinds of `include`, and the form itself.
 */
export function parseDefineLibrary(form) {
    const [name, exports, imports, body, includes, includesCi, declarationFiles] =
        toArray(callLibrarySystem('define-library-parts', currentLibraryRegistry(), form));
    const names = (parts) => toArray(parts).map((part) => part.name ?? String(part));
    return {
        name: toArray(name),
        exports: toArray(exports).map((pair) => ({ internal: pair.car.name, external: pair.cdr.name })),
        imports: toArray(imports).map((library) => ({ libraryName: names(library) })),
        body: toArray(body),
        includes: toArray(includes).map(stringValue),
        includesCi: toArray(includesCi).map(stringValue),
        includeLibraryDeclarations: toArray(declarationFiles).map(stringValue),
        form
    };
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
 * An environment of import sets and nothing else (R7RS 5.6.1): what
 * `environment` returns, and what a program that begins with import
 * declarations runs in.
 *
 * @param {Cons|null} sets - The import sets, as written.
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} base - An environment of the interpreter's, which
 *   libraries' own are made beside.
 * @returns {Environment} The environment.
 */
export function importEnvironment(sets, analyze, interpreter, base) {
    const env = makeScopedEnvironment(base);
    importLibraries(toArray(sets), analyze, interpreter, env);
    return env;
}

// =============================================================================
// Programs
// =============================================================================

/**
 * The environment a program runs in, and the forms to run there (R7RS 5.1):
 * a program that begins with import declarations runs in an environment of
 * what they import and nothing else, as a library's body does; one with none
 * runs in `env`, which sees everything, so that a page or a quick script with
 * no imports keeps working (`program-parts` in library_system.scm).
 *
 * @param {Array} forms - The program's forms, as read.
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} env - Where a program with no import declarations runs.
 * @returns {{env: Environment, forms: Array}} The environment, and the forms
 *   after the import declarations, to run with `runProgramForm`.
 */
export function programEnvironment(forms, analyze, interpreter, env) {
    const parts = callLibrarySystem('program-parts', list(...forms));
    if (parts.car === null) return { env, forms };
    return { env: importEnvironment(parts.car, analyze, interpreter, env), forms: toArray(parts.cdr) };
}

/**
 * Runs one of a program's forms, as read, in the environment
 * `programEnvironment` gave: through `runTopLevel`, so that the compiler tier
 * sees it, and analyzed and run under the environment's scope, if it has one,
 * as a library's body is (`evaluator`), so that the names it defines and the
 * keywords it imported are the program's own.
 *
 * @param {*} form - The form.
 * @param {Function} analyze - The analyze function
 * @param {Object} interpreter - The interpreter instance
 * @param {Environment} env - The program's environment.
 * @param {Object} [options] - For `runTopLevel`.
 * @returns {*} The form's value.
 */
export function runProgramForm(form, analyze, interpreter, env, options) {
    const scope = env.libraryScope;
    if (scope === undefined) return interpreter.runTopLevel(analyze(form, env), env, options);
    globalContext.pushDefiningScope(scope);
    try {
        return interpreter.runTopLevel(analyze(form, env), env, options);
    } finally {
        globalContext.popDefiningScope();
    }
}

/**
 * Binds every export of a library in an environment, under its own name: a
 * syntactic keyword in the expander's tables, anything else in the
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
 * Creates (scheme primitives) exports from the global environment: every
 * binding it holds, which, made as an interpreter is, is every primitive. A
 * library sees only what it imports (R7RS 5.6.1), so a primitive missing from
 * here would be unreachable from Scheme but by a program's top level.
 *
 * @param {Environment} globalEnv - The global environment with primitives
 * @returns {Map<string, *>} The exports map
 */
export function createPrimitiveExports(globalEnv) {
    return new Map(globalEnv.bindings);
}
