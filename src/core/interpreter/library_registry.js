/**
 * The library system's door from JavaScript.
 *
 * The library system is Scheme: `(scheme-js library-system)`, in
 * src/core/scheme/library_system.scm, loaded at first use by its seed
 * (library_seed.js). It holds the registries of loaded libraries, the
 * features `cond-expand` finds, and how libraries are loaded and imported.
 * What is here is the JavaScript API, which only calls it: it holds the
 * current registry, which a tool swaps for one of its own for a while
 * (`withPrivateLibraries`), and converts what crosses -- a library's name, as
 * an array of strings, to a list of symbols, and a library's exports, a list
 * of `(name . value)`, to a `Map` -- so that JavaScript callers see what they
 * always have.
 */

import { list, cons, toArray } from './cons.js';
import { Symbol, intern } from './symbol.js';
import { callSchemeProcedure } from './values.js';
import { globalContext } from './context.js';
import { seedLibrarySystem } from './library_seed.js';
import { stringValue } from '../primitives/string_class.js';

// =============================================================================
// The library system, and the current registry
// =============================================================================

/**
 * The procedures `(scheme-js library-system)` exports, once it is loaded.
 * @type {Map<string, Function>|null}
 */
let librarySystem = null;

/**
 * The registry libraries are loaded into and found in now.
 * @type {Object|null}
 */
let libraryRegistry = null;

/**
 * Calls one of the library system's procedures, loading the library system
 * first if this is the first call.
 * @param {string} name - The procedure's name, as the library exports it.
 * @param {...*} args - Its arguments, Scheme values.
 * @returns {*} Its result.
 */
export function callLibrarySystem(name, ...args) {
    if (librarySystem === null) librarySystem = seedLibrarySystem();
    return callSchemeProcedure(librarySystem.get(name), args);
}

/**
 * The registry libraries are loaded into now, made at first use with no
 * libraries, no resolver, and the features of this implementation on this
 * host.
 * @returns {Object} The registry.
 */
export function currentLibraryRegistry() {
    if (libraryRegistry === null) {
        const host = typeof process !== 'undefined' && process.versions?.node != null ? 'node' : 'browser';
        libraryRegistry = callLibrarySystem('make-library-registry', false, false,
            callLibrarySystem('standard-features', intern(host)));
    }
    return libraryRegistry;
}

/**
 * A library's name as the library system takes it: a list of symbols.
 * @param {Array|Cons} name - The name, its parts strings, symbols or numbers.
 * @returns {Cons}
 */
export function schemeLibraryName(name) {
    const parts = Array.isArray(name) ? name : toArray(name);
    return list(...parts.map(p => p instanceof Symbol ? p : intern(String(p))));
}

/**
 * A library's exports as JavaScript is given them.
 * @param {Cons|null|boolean} exports - The exports, `(name . value)`, or #f.
 * @returns {Map<string, *>|null} The exports by name, or null for #f.
 */
export function exportsMap(exports) {
    if (exports === false) return null;
    const map = new Map();
    for (let rest = exports; rest !== null; rest = rest.cdr) map.set(rest.car.car.name, rest.car.cdr);
    return map;
}

/**
 * A library's exports as the library system holds them.
 * @param {Map<string, *>|Object} exports - The exports by name.
 * @returns {Cons|null} The exports, `(name . value)`.
 */
export function exportsAlist(exports) {
    const entries = exports instanceof Map ? [...exports] : Object.entries(exports);
    return list(...entries.map(([name, value]) => cons(intern(name), value)));
}

// =============================================================================
// Features (for cond-expand)
// =============================================================================

/**
 * Checks if a feature is supported.
 * @param {string} featureName - Feature identifier
 * @returns {boolean}
 */
export function hasFeature(featureName) {
    return getFeatures().includes(featureName);
}

/**
 * Adds a feature to the registry.
 * @param {string} featureName - Feature identifier
 */
export function addFeature(featureName) {
    callLibrarySystem('add-feature!', currentLibraryRegistry(), intern(featureName));
}

/**
 * Gets all supported features.
 * @returns {string[]}
 */
export function getFeatures() {
    return toArray(callLibrarySystem('registry-features', currentLibraryRegistry())).map(f => f.name);
}

/**
 * Evaluates a cond-expand feature requirement.
 *
 * @param {Symbol|Cons} requirement - Feature requirement expression
 * @returns {boolean} True if requirement is satisfied
 */
export function evaluateFeatureRequirement(requirement) {
    return callLibrarySystem('registry-requirement-met?', currentLibraryRegistry(), requirement);
}

// =============================================================================
// Library Registry
// =============================================================================

/**
 * Sets the file resolver for loading library files.
 * @param {Function|null} resolver - (libraryName: string[]) => string, or a
 *   promise of it
 */
export function setFileResolver(resolver) {
    callLibrarySystem('set-registry-resolver!', currentLibraryRegistry(), resolver ?? false);
}

/**
 * Gets the current file resolver.
 * @returns {Function|null}
 */
export function getFileResolver() {
    const resolver = callLibrarySystem('registry-resolver', currentLibraryRegistry());
    return resolver === false ? null : resolver;
}

/**
 * Sets what runs on each library loaded from a file.
 *
 * Whatever sets up the process at start-up can compile the environment it
 * has, but not a library loaded afterwards; this is how it reaches those. The
 * hook runs for libraries found by name through the file resolver, not for a
 * `define-library` written inline, which is the program's own code.
 *
 * @param {((libraryName: string[], env: Environment) => void)|null} hook - Called
 *   with the library's name and its own environment, or null for none.
 */
export function setLibraryLoadHook(hook) {
    callLibrarySystem('set-registry-load-hook!', currentLibraryRegistry(), hook ?? false);
}

/**
 * Loads libraries apart from every library loaded so far, and from every one
 * loaded afterwards.
 *
 * The registry, the file resolver and the load hook are shared by everything
 * in the process, which is right for a program and its libraries and wrong for
 * a tool that runs Scheme on the program's behalf. The compiler is one: it is
 * written with `(scheme base)` and SRFI 1, and if it shared the program's
 * instances of those, a program that redefined one of their procedures would
 * change the compiler, and a build step compiling those very libraries would
 * find them already loaded -- by the compiler -- and never see them load.
 *
 * Inside `fn`, the registry starts empty and the resolver and hook are the
 * ones given, with the features of the registry outside; afterwards the
 * registry outside is current again, whether `fn` returned or threw.
 * Libraries loaded inside stay alive through whatever holds them, and are
 * found by nothing outside: not by name, and not by scope, since the entries
 * made inside in the analyzer's tables keyed by scope go too
 * (`leavePrivateLibraries` in context.js). Loading is synchronous, so nothing
 * else can observe the swap.
 *
 * @param {Object} loader - How to load inside.
 * @param {Function} loader.resolver - The file resolver to use, which must be
 *   synchronous for `loadLibrarySync`.
 * @param {Function|null} [loader.hook=null] - The load hook to use.
 * @param {() => *} fn - What to run.
 * @returns {*} What `fn` returned.
 */
export function withPrivateLibraries({ resolver, hook = null }, fn) {
    const saved = currentLibraryRegistry();
    const registry = callLibrarySystem('make-library-registry', resolver ?? false, hook ?? false,
        callLibrarySystem('registry-features', saved));
    globalContext.enterPrivateLibraries();
    libraryRegistry = registry;
    try {
        return fn();
    } finally {
        globalContext.leavePrivateLibraries();
        libraryRegistry = saved;
    }
}


/**
 * Converts a library name to a string key, as the library system keys its
 * registries (`library-key`).
 * (scheme base) -> "scheme.base"
 * @param {Array|Cons} name - Library name as list or array
 * @returns {string}
 */
export function libraryNameToKey(name) {
    const parts = Array.isArray(name) ? name : toArray(name);
    return parts.map(p => p instanceof Symbol ? p.name : String(p)).join('.');
}

/**
 * The key of a library given by name parts or by key.
 * @param {string|Array} library - Library name parts or library key.
 * @returns {string}
 */
function keyOf(library) {
    return typeof library === 'string' ? library : libraryNameToKey(library);
}

/**
 * Checks if a library is already loaded.
 * @param {string} key - Library key
 * @returns {boolean}
 */
export function isLibraryLoaded(key) {
    return callLibrarySystem('registered-exports', currentLibraryRegistry(), key) !== false;
}

/**
 * Gets a loaded library's exports.
 * @param {string|string[]} library - Library name parts or library key
 * @returns {Map|null}
 */
export function getLibraryExports(library) {
    return exportsMap(callLibrarySystem('registered-exports', currentLibraryRegistry(), keyOf(library)));
}

/**
 * Gets a loaded library's environment.
 * @param {string|string[]} library - Library name parts or library key
 * @returns {Environment|null}
 */
export function getLibraryEnv(library) {
    const env = callLibrarySystem('registered-environment', currentLibraryRegistry(), keyOf(library));
    return env === false ? null : env;
}

/**
 * Registers a library in the registry.
 * @param {string} key - Library key
 * @param {Map} exports - Library exports
 * @param {Environment} env - Library environment
 */
export function registerLibrary(key, exports, env) {
    callLibrarySystem('register-exports!', currentLibraryRegistry(), key, exportsAlist(exports), env);
}

/**
 * Substitutes values throughout every loaded library: in their exports, their
 * environments, and the values those hold (`substitute-library-values!` in
 * library_system.scm, which says why).
 *
 * @param {Map<*, *>} replacements - Each replaced value, mapped to its
 *   replacement.
 * @param {Object} [registry] - The registry whose libraries to change; the
 *   current one by default.
 */
export function substituteLibraryValues(replacements, registry = currentLibraryRegistry()) {
    callLibrarySystem('substitute-library-values!', registry, pairsOf(replacements));
}

/**
 * A map's entries as a list of pairs.
 * @param {Map<*, *>} map - The map.
 * @returns {Cons|null} Each `(key . value)`.
 */
function pairsOf(map) {
    return list(...[...map].map(([key, value]) => cons(key, value)));
}

// =============================================================================
// Running compiled procedures as their interpreted closures, for a debugger
// =============================================================================

/**
 * The programs being debugged, each mapped to the registry its libraries were
 * switched in (`make-debugged-programs` in library_system.scm); one for the
 * process, made at first use.
 * @type {Object|null}
 */
let debuggedPrograms = null;

/**
 * The programs being debugged.
 * @returns {Object}
 */
function debugged() {
    if (debuggedPrograms === null) debuggedPrograms = callLibrarySystem('make-debugged-programs');
    return debuggedPrograms;
}

/**
 * Records compiled procedures just installed over interpreted closures, so a
 * debugger can switch back to the closures (`record-compiled-over!` in
 * library_system.scm).
 *
 * @param {Map<Function, Function>} replaced - Each interpreted closure, mapped
 *   to the compiled procedure installed over it.
 * @param {Object} env - The environment they were installed into.
 */
export function recordCompiledOver(replaced, env) {
    callLibrarySystem('record-compiled-over!', currentLibraryRegistry(), debugged(), pairsOf(replaced), env);
}

/**
 * Whether a compiled procedure was installed over an interpreted closure it
 * can run as instead.
 * @param {Function} procedure - The procedure.
 * @returns {boolean}
 */
export function isCompiledOver(procedure) {
    return callLibrarySystem('compiled-over?', currentLibraryRegistry(), procedure);
}

/**
 * Switches every recorded compiled procedure to its interpreted closure, or
 * back, for one program, while it is debugged (`interpret-compiled-over!` in
 * library_system.scm).
 *
 * @param {boolean} interpreted - Whether to run the closures.
 * @param {Object} globalEnv - The program's global environment.
 */
export function interpretCompiledOver(interpreted, globalEnv) {
    callLibrarySystem('interpret-compiled-over!', currentLibraryRegistry(), debugged(), interpreted, globalEnv);
}

/**
 * Switches one compiled procedure back to the interpreted closure it was
 * compiled from, for good (`switch-back-to-closure!` in library_system.scm).
 *
 * @param {Function} twin - The procedure's resumable form.
 * @returns {boolean} Whether a procedure was switched back.
 */
export function switchBackToClosure(twin) {
    return callLibrarySystem('switch-back-to-closure!', currentLibraryRegistry(), debugged(), twin);
}


/**
 * Gets all loaded library keys (for debugging).
 * @returns {string[]}
 */
export function getLoadedLibraries() {
    return toArray(callLibrarySystem('registered-keys', currentLibraryRegistry())).map(stringValue);
}

/**
 * Clears the library registry (for testing).
 */
export function clearLibraryRegistry() {
    callLibrarySystem('clear-registry!', currentLibraryRegistry());
}

/**
 * Registers a builtin library with exports from JavaScript.
 * Used for (scheme base) and other libraries that need runtime primitives.
 * 
 * @param {string[]} libraryName - Library name parts (e.g., ['scheme', 'base'])
 * @param {Map<string, *>|Object} exports - Exports as Map or object
 * @param {Environment} env - The environment containing the bindings
 */
export function registerBuiltinLibrary(libraryName, exports, env) {
    registerLibrary(libraryNameToKey(libraryName), exports, env);
}

// =============================================================================
// Syntax Keywords and Special Forms
// =============================================================================

/**
 * Standard Scheme syntax keywords.
 * These are handled by the analyzer as special forms; a library exporting one
 * exports it as a keyword (`%special-keyword?` in primitives/library.js).
 */
export const SYNTAX_KEYWORDS = new Set([
    'define', 'set!', 'lambda', 'if', 'begin', 'quote',
    'quasiquote', 'unquote', 'unquote-splicing',
    'define-syntax', 'let-syntax', 'letrec-syntax',
    'syntax-rules', '...', '_', 'else', '=>', 'import', 'export',
    'define-library', 'include', 'include-ci', 'include-library-declarations',
    'cond-expand', 'let', 'letrec', 'call/cc', 'call-with-current-continuation',
    'define-macro'
]);

/**
 * Special forms recognized by the analyzer.
 * These have dedicated analysis functions and are NOT treated as macro calls.
 * Also used by syntax_rules to prevent renaming during macro expansion.
 */
export const SPECIAL_FORMS = new Set([
    // Core special forms
    'if', 'let', 'letrec', 'lambda', 'set!', 'define', 'begin',
    'quote', 'quasiquote', 'unquote', 'unquote-splicing',
    // Macro-related
    'define-syntax', 'let-syntax', 'letrec-syntax', 'define-macro',
    // Control flow
    'call/cc', 'call-with-current-continuation',
    // Module system
    'import', 'cond-expand'
]);
