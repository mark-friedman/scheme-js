/**
 * Library Registry Module
 * 
 * Manages the registry of loaded libraries and feature detection.
 * This module is pure data management - no loading or parsing.
 */

import { toArray } from './cons.js';
import { Symbol } from './symbol.js';

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
                throw new Error('cond-expand: (not) requires exactly one argument');
            }
            return !evaluateFeatureRequirement(arr[1]);

        case 'library':
            // Check if library is available (loaded or loadable)
            if (arr.length !== 2) {
                throw new Error('cond-expand: (library) requires a library name');
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
const libraryRegistry = new Map();

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
    'cond-expand', 'let', 'letrec', 'call/cc', 'call-with-current-continuation'
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
    'define-syntax', 'let-syntax', 'letrec-syntax',
    // Control flow
    'call/cc', 'call-with-current-continuation',
    // Module system
    'import', 'cond-expand'
]);
