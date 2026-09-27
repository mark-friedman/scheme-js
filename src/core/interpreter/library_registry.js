/**
 * Library Registry Module
 * 
 * Manages the registry of loaded libraries and feature detection.
 * This module is pure data management - no loading or parsing.
 */

import { toArray } from './cons.js';
import { Symbol } from './symbol.js';
import { SchemeSyntaxError } from './errors.js';

// =============================================================================
// Feature Registry (for cond-expand)
// =============================================================================

/**
 * Feature registry for cond-expand and (features) primitive.
 * Standard R7RS features plus implementation-specific ones.
 * This is the single source of truth for all features.
 */
const features = new Set([
    'r7rs',           // R7RS Scheme
    'scheme-js',      // This implementation
    'exact-closed',   // Rationals not implemented, but we can claim this for integers
    'ratios',         // Rational number support
    'ieee-float',     // JavaScript uses IEEE 754
    'full-unicode',   // Full Unicode support in strings
]);

// Detect Node.js vs browser and add appropriate feature
const isNode = typeof process !== 'undefined' &&
    process.versions != null &&
    process.versions.node != null;

if (isNode) {
    features.add('node');
} else {
    features.add('browser');
}

/**
 * Checks if a feature is supported.
 * @param {string} featureName - Feature identifier
 * @returns {boolean}
 */
export function hasFeature(featureName) {
    return features.has(featureName);
}

/**
 * Adds a feature to the registry.
 * @param {string} featureName - Feature identifier
 */
export function addFeature(featureName) {
    features.add(featureName);
}

/**
 * Gets all supported features.
 * @returns {string[]}
 */
export function getFeatures() {
    return Array.from(features);
}

/**
 * Evaluates a cond-expand feature requirement.
 * 
 * @param {Symbol|Cons} requirement - Feature requirement expression
 * @returns {boolean} True if requirement is satisfied
 */
export function evaluateFeatureRequirement(requirement) {
    // Simple feature identifier
    if (requirement instanceof Symbol) {
        return features.has(requirement.name);
    }

    // Compound requirement: (and ...), (or ...), (not ...), (library ...)
    const arr = toArray(requirement);
    if (arr.length === 0) return false;

    const tag = arr[0];
    if (!(tag instanceof Symbol)) return false;

    switch (tag.name) {
        case 'and':
            // All requirements must be true
            for (let i = 1; i < arr.length; i++) {
                if (!evaluateFeatureRequirement(arr[i])) return false;
            }
            return true;

        case 'or':
            // At least one requirement must be true
            for (let i = 1; i < arr.length; i++) {
                if (evaluateFeatureRequirement(arr[i])) return true;
            }
            return false;

        case 'not':
            // Negation
            if (arr.length !== 2) {
                throw new SchemeSyntaxError('(not) requires exactly one argument', requirement, 'cond-expand');
            }
            return !evaluateFeatureRequirement(arr[1]);

        case 'library':
            // Check if library is available (loaded or loadable)
            if (arr.length !== 2) {
                throw new SchemeSyntaxError('(library) requires a library name', requirement, 'cond-expand');
            }
            const libName = toArray(arr[1]);
            const libKey = libraryNameToKey(libName);
            return libraryRegistry.has(libKey);

        default:
            // Unknown tag - treat as false
            return false;
    }
}

// =============================================================================
// Library Registry
// =============================================================================

/**
 * Registry of loaded libraries.
 * Key: stringified library name (e.g., "scheme.base")
 * Value: { exports: Map<string, value>, env: Environment }
 */
let libraryRegistry = new Map();

/**
 * File resolver function (set by runtime).
 * @type {(libraryName: string[]) => Promise<string>}
 */
let fileResolver = null;

/**
 * Sets the file resolver for loading library files.
 * @param {Function} resolver - (libraryName: string[]) => Promise<string>
 */
export function setFileResolver(resolver) {
    fileResolver = resolver;
}

/**
 * Gets the current file resolver.
 * @returns {Function|null}
 */
export function getFileResolver() {
    return fileResolver;
}

/**
 * Called with each library loaded from a file, once it has been evaluated.
 * @type {((libraryName: string[], env: Environment) => void)|null}
 */
let libraryLoadHook = null;

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
    libraryLoadHook = hook;
}

/**
 * Runs the library-load hook, if one is set.
 * @param {string[]} libraryName - The library's name.
 * @param {Environment} env - The library's own environment.
 */
export function runLibraryLoadHook(libraryName, env) {
    if (libraryLoadHook !== null) libraryLoadHook(libraryName, env);
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
 * ones given; afterwards all three are as they were, whether `fn` returned or
 * threw. Libraries loaded inside stay alive through whatever holds them, and
 * are found by nothing outside. Loading is synchronous, so nothing else can
 * observe the swap.
 *
 * @param {Object} loader - How to load inside.
 * @param {Function} loader.resolver - The file resolver to use, which must be
 *   synchronous for `loadLibrarySync`.
 * @param {Function|null} [loader.hook=null] - The load hook to use.
 * @param {() => *} fn - What to run.
 * @returns {*} What `fn` returned.
 */
export function withPrivateLibraries({ resolver, hook = null }, fn) {
    const saved = { registry: libraryRegistry, resolver: fileResolver, hook: libraryLoadHook };
    libraryRegistry = new Map();
    fileResolver = resolver;
    libraryLoadHook = hook;
    try {
        return fn();
    } finally {
        libraryRegistry = saved.registry;
        fileResolver = saved.resolver;
        libraryLoadHook = saved.hook;
    }
}


/**
 * Converts a library name to a string key.
 * (scheme base) -> "scheme.base"
 * @param {Array|Cons} name - Library name as list or array
 * @returns {string}
 */
export function libraryNameToKey(name) {
    const parts = Array.isArray(name) ? name : toArray(name);
    return parts.map(p => p instanceof Symbol ? p.name : String(p)).join('.');
}

/**
 * Checks if a library is already loaded.
 * @param {string} key - Library key
 * @returns {boolean}
 */
export function isLibraryLoaded(key) {
    return libraryRegistry.has(key);
}

/**
 * Gets a loaded library's exports.
 * @param {string|string[]} library - Library name parts or library key
 * @returns {Map|null}
 */
export function getLibraryExports(library) {
    const key = Array.isArray(library) ? libraryNameToKey(library) : library;
    const lib = libraryRegistry.get(key);
    return lib ? lib.exports : null;
}

/**
 * Gets a loaded library's environment.
 * @param {string|string[]} library - Library name parts or library key
 * @returns {Environment|null}
 */
export function getLibraryEnv(library) {
    const key = Array.isArray(library) ? libraryNameToKey(library) : library;
    const lib = libraryRegistry.get(key);
    return lib ? lib.env : null;
}

/**
 * Registers a library in the registry.
 * @param {string} key - Library key
 * @param {Map} exports - Library exports
 * @param {Environment} env - Library environment
 */
export function registerLibrary(key, exports, env) {
    libraryRegistry.set(key, { exports, env });
}

/**
 * Substitutes values throughout every loaded library.
 *
 * Importing copies values, so a procedure exported by one library lives on in
 * that library's export map and in the environment of every library that
 * imported it. Replacing the global binding -- which is how the standard
 * library is compiled after it has been loaded -- reaches none of those copies,
 * and a library loaded afterwards would import the procedure that was replaced.
 * Whatever replaces a binding in place calls this with what it replaced.
 *
 * Only values identical to a replaced one change; a library that bound the
 * same name to something else keeps it.
 *
 * @param {Map<*, *>} replacements - Each replaced value, mapped to its
 *   replacement.
 * @param {Map<string, Object>} [registry] - The registry whose libraries to
 *   change; the current one by default.
 */
export function substituteLibraryValues(replacements, registry = libraryRegistry) {
    if (replacements.size === 0) return;
    for (const { exports, env } of registry.values()) {
        for (const [name, value] of exports) {
            const replacement = replacements.get(value);
            if (replacement !== undefined) exports.set(name, replacement);
        }
        if (env && env.bindings instanceof Map) {
            for (const [name, value] of env.bindings) {
                const replacement = replacements.get(value);
                // Through the frame, so a cell compiled code reads the
                // binding through follows the replacement.
                if (replacement !== undefined) {
                    if (typeof env.rebind === 'function') env.rebind(name, replacement);
                    else env.bindings.set(name, replacement);
                }
            }
        }
    }
}

// =============================================================================
// Running compiled procedures as their interpreted closures, for a debugger
// =============================================================================
//
// A debugger pauses only between the interpreter's steps, which compiled code
// never takes: a breakpoint inside a compiled procedure cannot fire, and one
// inside an interpreted procedure that compiled code called is reached in a
// synchronous nested run of the interpreter, which cannot wait, so the program
// stops only once the compiled code returns. So while a program is being
// debugged, every procedure compiled over an interpreted closure -- the
// libraries' prebuilt code, `compileEnvironment` -- runs as that closure again:
// the declining-to-optimize every toolchain offers beside its debug info,
// applied to the whole program. The closures are kept for that when the
// compiled code is installed.

/**
 * Each compiled procedure installed over an interpreted closure, mapped to the
 * closure.
 * @type {Map<Function, Function>}
 */
const compiledOver = new Map();

/**
 * The global environments of the programs being debugged, whose compiled
 * procedures run as their closures, each mapped to the library registry its
 * libraries were switched in.
 * @type {Map<Object, Map<string, Object>>}
 */
const interpretingIn = new Map();

/**
 * Replaces values in an environment and every environment it is inside.
 * @param {Object} env - The innermost environment.
 * @param {Map<*, *>} replacements - Each replaced value, mapped to its
 *   replacement.
 */
function substituteInChain(env, replacements) {
    for (let e = env; e; e = e.parent) {
        if (!(e.bindings instanceof Map)) continue;
        for (const [name, value] of e.bindings) {
            const replacement = replacements.get(value);
            if (replacement === undefined) continue;
            if (typeof e.rebind === 'function') e.rebind(name, replacement);
            else e.bindings.set(name, replacement);
        }
    }
}

/**
 * Records compiled procedures just installed over interpreted closures, so a
 * debugger can switch back to the closures.
 *
 * Installed straight into the global environment of a program being debugged
 * -- `compileEnvironment` on it -- they are switched back at once. A library
 * loaded while a program is being debugged is switched back when the program
 * next runs (`interpretCompiledOver`, which each asynchronous run asks for):
 * the library's values reach the program by import, after this.
 *
 * A tool's own procedures -- the compiler's, in its private libraries -- are
 * recorded too, and never switched: switching reaches only the libraries
 * loaded where the switch is made and the program's global environment.
 *
 * @param {Map<Function, Function>} replaced - Each interpreted closure, mapped
 *   to the compiled procedure installed over it.
 * @param {Object} env - The environment they were installed into.
 */
export function recordCompiledOver(replaced, env) {
    if (replaced.size === 0) return;
    const back = new Map();
    for (const [closure, compiled] of replaced) {
        compiledOver.set(compiled, closure);
        back.set(compiled, closure);
    }
    if (interpretingIn.has(env)) {
        substituteLibraryValues(back, interpretingIn.get(env));
        substituteInChain(env, back);
    }
}

/**
 * Whether a compiled procedure was installed over an interpreted closure it
 * can run as instead.
 * @param {Function} procedure - The procedure.
 * @returns {boolean}
 */
export function isCompiledOver(procedure) {
    return compiledOver.has(procedure);
}

/**
 * Switches every recorded compiled procedure to its interpreted closure, or
 * back, for one program: in its global environment, and in every library
 * loaded in the current registry -- which the registry's other programs share,
 * so they are compiled again only once none of those is being debugged.
 *
 * Switching to the closures again is harmless, and catches what was compiled
 * since. Bindings are replaced through their frames, so the cells compiled
 * code reads globals through follow, and compiled code still running calls the
 * closures from its next call on. A compiled procedure a program holds in a
 * data structure, or has captured in a closure, is not found and stays
 * compiled.
 *
 * @param {boolean} interpreted - Whether to run the closures.
 * @param {Object} globalEnv - The program's global environment.
 */
export function interpretCompiledOver(interpreted, globalEnv) {
    let registry;
    if (interpreted) {
        registry = libraryRegistry;
        interpretingIn.set(globalEnv, registry);
    } else {
        registry = interpretingIn.get(globalEnv);
        if (registry === undefined) return;
        interpretingIn.delete(globalEnv);
    }
    const replacements = new Map();
    for (const [compiled, closure] of compiledOver) {
        if (interpreted) replacements.set(compiled, closure);
        else replacements.set(closure, compiled);
    }
    const stillDebugged = [...interpretingIn.values()].includes(registry);
    if (interpreted || !stillDebugged) substituteLibraryValues(replacements, registry);
    substituteInChain(globalEnv, replacements);
}

/**
 * Gets all loaded library keys (for debugging).
 * @returns {string[]}
 */
export function getLoadedLibraries() {
    return Array.from(libraryRegistry.keys());
}

/**
 * Clears the library registry (for testing).
 */
export function clearLibraryRegistry() {
    libraryRegistry.clear();
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
    const key = libraryNameToKey(libraryName);

    // Convert object to Map if needed
    const exportsMap = exports instanceof Map
        ? exports
        : new Map(Object.entries(exports));

    libraryRegistry.set(key, { exports: exportsMap, env });
}

// =============================================================================
// Syntax Keywords and Special Forms
// =============================================================================

/**
 * Standard Scheme syntax keywords.
 * These are handled by the analyzer as special forms.
 * Used by library_loader to filter keywords from exports.
 */
export const SYNTAX_KEYWORDS = new Set([
    'define', 'set!', 'lambda', 'if', 'begin', 'quote',
    'quasiquote', 'unquote', 'unquote-splicing',
    'define-syntax', 'let-syntax', 'letrec-syntax',
    'syntax-rules', '...', 'else', '=>', 'import', 'export',
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
