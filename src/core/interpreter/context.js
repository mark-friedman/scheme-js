/**
 * Interpreter Context
 * 
 * Encapsulates all mutable state for a single interpreter instance.
 * This enables multiple isolated interpreters to coexist without
 * sharing global state, which is essential for:
 * - Parallel test execution
 * - Multi-tenant REPL environments
 * - Sandboxed evaluation
 */

import { MacroRegistry, globalMacroRegistry } from './macro_registry.js';

// =============================================================================
// Scope Binding Registry (moved from syntax_object.js)
// =============================================================================

/**
 * Maps (name, scopes) → binding information within a context.
 * When resolving an identifier:
 * 1. Find all bindings for that name
 * 2. Find the binding whose scopes are a maximal subset of the identifier's scopes
 * 3. Return that binding, or null if none found
 */
class ScopeBindingRegistry {
    constructor() {
        /** @type {Map<string, Map<string, { scopes: Set<number>, value: * }>>} */
        this.bindings = new Map();
    }

    /**
     * Registers a binding for a name with given scopes.
     * @param {string} name 
     * @param {Set<number>} scopes 
     * @param {*} value 
     */
    register(name, scopes, value) {
        if (!this.bindings.has(name)) {
            this.bindings.set(name, new Map());
        }
        const scopeKey = [...scopes].sort((a, b) => a - b).join(',');
        this.bindings.get(name).set(scopeKey, { scopes: new Set(scopes), value });
    }

    /**
     * Resolves a binding for a name given the identifier's scopes.
     * Uses maximal subset matching.
     * @param {string} name 
     * @param {Set<number>} identifierScopes 
     * @returns {* | null}
     */
    resolve(name, identifierScopes) {
        const nameBindings = this.bindings.get(name);
        if (!nameBindings) return null;

        let bestMatch = null;
        let bestSize = -1;

        for (const [, binding] of nameBindings) {
            // Check if binding.scopes is a subset of identifierScopes
            let isSubset = true;
            for (const scope of binding.scopes) {
                if (!identifierScopes.has(scope)) {
                    isSubset = false;
                    break;
                }
            }
            if (isSubset && binding.scopes.size > bestSize) {
                bestMatch = binding.value;
                bestSize = binding.scopes.size;
            }
        }
        return bestMatch;
    }

    /**
     * Clears all bindings.
     */
    clear() {
        this.bindings.clear();
    }
}

// =============================================================================
// Interpreter Context
// =============================================================================

/**
 * Holds all mutable state for a single interpreter instance.
 * 
 * When creating interpreters that should be isolated from each other,
 * each should have its own InterpreterContext instance.
 */
export class InterpreterContext {
    constructor() {
        // Counters
        /** Scope ID counter for unique scope marks */
        this.scopeCounter = 0;
        /** Unique ID counter for analyzed variables */
        this.uniqueIdCounter = 0;

        // Caches and Registries
        /** Interned SyntaxObjects: key → SyntaxObject */
        this.syntaxInternCache = new Map();
        /** Scope → Binding registry */
        this.scopeRegistry = new ScopeBindingRegistry();
        /** Library scope → Environment map */
        this.libraryScopeEnvMap = new Map();
        /** Macro transformer registry (global for this context) - isolated but sees global macros */
        this.macroRegistry = new MacroRegistry(globalMacroRegistry);
        /** Current macro registry stack for scoped expansion (transient during analysis) */
        this.currentMacroRegistry = this.macroRegistry;
        /** Loaded libraries: key → { exports, env } */
        this.libraryRegistry = new Map();
        /** Feature flags for cond-expand */
        this.features = new Set(['r7rs', 'scheme-js', 'ratios', 'exact-complex']);

        // Detect environment
        const isNode = typeof process !== 'undefined' &&
            process.versions != null &&
            process.versions.node != null;
        this.features.add(isNode ? 'node' : 'browser');

        /** File resolver for loading library files */
        this.fileResolver = null;

        /** Baseline macro names for reset */
        this.baselineMacroNames = null;

        /** Stack of currently active defining scopes (for library loading) */
        this.definingScopes = [];

        /**
         * The syntactic keywords each library binds -- the macros it defines
         * and the keywords it imports, under the names it gives them -- and
         * those a program's top level imports, by scope: a library's, or 0
         * for the top level. Each name maps to the keyword's own name and,
         * for a macro, its transformer. See `defineKeyword`.
         * @type {Map<number, Map<string, {keyword: string, transformer: (Function|null)}>>}
         */
        this.keywordBindings = new Map();
    }

    // =========================================================================
    // Scope Management
    // =========================================================================

    /**
     * Creates a fresh scope identifier.
     * @returns {number} A unique scope ID
     */
    freshScope() {
        return this.scopeCounter++;
    }

    /**
     * Resets the scope counter. (For testing)
     */
    resetScopeCounter() {
        this.scopeCounter = 0;
    }

    // =========================================================================
    // Unique ID Management
    // =========================================================================

    /**
     * Creates a fresh unique ID for analyzed variables.
     * @returns {number}
     */
    freshUniqueId() {
        return this.uniqueIdCounter++;
    }

    /**
     * Resets the unique ID counter. (For testing)
     */
    resetUniqueIdCounter() {
        this.uniqueIdCounter = 0;
    }

    // =========================================================================
    // Syntax Interning
    // =========================================================================

    /**
     * Generates a cache key for a syntax object.
     * @param {string} name 
     * @param {Set<number>} scopes 
     * @returns {string}
     */
    getSyntaxKey(name, scopes) {
        const sortedScopes = [...scopes].sort((a, b) => a - b).join(',');
        return `${name}|${sortedScopes}`;
    }

    /**
     * Clears the syntax intern cache. (For testing)
     */
    resetSyntaxCache() {
        this.syntaxInternCache.clear();
    }

    // =========================================================================
    // Macro Registry
    // =========================================================================

    /**
     * Takes a snapshot of the current macro registry state as the baseline.
     */
    snapshotMacroRegistry() {
        this.baselineMacroNames = new Set(this.macroRegistry.macros.keys());
    }

    /**
     * Resets the macro registry to the baseline state.
     */
    resetMacroRegistry() {
        if (this.baselineMacroNames === null) {
            this.macroRegistry.macros.clear();
        } else {
            for (const name of this.macroRegistry.macros.keys()) {
                if (!this.baselineMacroNames.has(name)) {
                    this.macroRegistry.macros.delete(name);
                }
            }
        }
    }

    // =========================================================================
    // Library Registry
    // =========================================================================

    /**
     * Converts a library name to a string key.
     * @param {Array} name - Library name as list or array
     * @returns {string}
     */
    libraryNameToKey(name) {
        if (Array.isArray(name)) {
            return name.join('.');
        }
        // Assume it's a Cons list - convert to array first
        const parts = [];
        let current = name;
        while (current !== null && current.car !== undefined) {
            const part = current.car;
            parts.push(typeof part === 'object' && part.name ? part.name : String(part));
            current = current.cdr;
        }
        return parts.join('.');
    }

    /**
     * Checks if a library is loaded.
     * @param {string} key 
     * @returns {boolean}
     */
    isLibraryLoaded(key) {
        return this.libraryRegistry.has(key);
    }

    /**
     * Gets a library's exports.
     * @param {string|Array} library 
     * @returns {Map|null}
     */
    getLibraryExports(library) {
        const key = Array.isArray(library) ? library.join('.') : library;
        const entry = this.libraryRegistry.get(key);
        return entry ? entry.exports : null;
    }

    /**
     * Registers a library.
     * @param {string} key 
     * @param {Map} exports 
     * @param {*} env 
     */
    registerLibrary(key, exports, env) {
        this.libraryRegistry.set(key, { exports, env });
    }

    /**
     * Clears the library registry. (For testing)
     */
    clearLibraryRegistry() {
        this.libraryRegistry.clear();
    }

    // =========================================================================
    // Feature Detection
    // =========================================================================

    /**
     * Checks if a feature is supported.
     * @param {string} featureName 
     * @returns {boolean}
     */
    hasFeature(featureName) {
        return this.features.has(featureName);
    }

    /**
     * Adds a feature.
     * @param {string} featureName 
     */
    addFeature(featureName) {
        this.features.add(featureName);
    }

    // =========================================================================
    // Full Reset (For Testing)
    // =========================================================================

    /**
     * Resets all state in this context to initial values.
     * Useful for test isolation.
     */
    reset() {
        this.scopeCounter = 0;
        this.uniqueIdCounter = 0;
        this.syntaxInternCache.clear();
        this.scopeRegistry.clear();
        this.libraryScopeEnvMap.clear();
        this.resetMacroRegistry();
        this.libraryRegistry.clear();
        this.definingScopes = [];
        this.keywordBindings.clear();
    }

    // =========================================================================
    // Defining Scope Stack (for library loading)
    // =========================================================================

    /**
     * Push a defining scope onto the stack.
     * Call this when entering a library or module context.
     * @param {number} scope - The scope ID
     */
    pushDefiningScope(scope) {
        this.definingScopes.push(scope);
    }

    /**
     * Pop the current defining scope from the stack.
     * Call this when exiting a library or module context.
     * @returns {number|undefined} The popped scope ID
     */
    popDefiningScope() {
        return this.definingScopes.pop();
    }

    /**
     * Get all currently active defining scopes.
     * @returns {number[]} Array of scope IDs
     */
    getDefiningScopes() {
        return [...this.definingScopes];
    }

    /**
     * Register a library scope with its environment.
     * @param {number} scope - The scope ID
     * @param {Environment} env - The library's environment
     */
    registerLibraryScope(scope, env) {
        this.libraryScopeEnvMap.set(scope, env);
    }

    /**
     * Look up the environment for a library scope.
     * @param {number} scope - The scope ID
     * @returns {Environment|undefined}
     */
    lookupLibraryEnv(scope) {
        return this.libraryScopeEnvMap.get(scope);
    }

    /**
     * Binds a syntactic keyword in a library, or at a program's top level.
     *
     * Macros are also defined by name, for the whole process, and a name is
     * looked up there when nothing binds it here. These bindings are what
     * keeps libraries apart: a library's macros expand into its own macros,
     * though another library defines one of the same name, and a keyword it
     * imported -- under its own name or another -- is the one it imported,
     * though a library loaded later defines a macro of that name. A macro's
     * transformer is kept as it was, so a library defining its own
     * `quasiquote` on the standard one, imported under another name, still
     * reaches the standard one through that name.
     *
     * @param {number} scope - A library's scope, or 0 for a program's top level.
     * @param {string} name - The name bound.
     * @param {string} keyword - The keyword's own name.
     * @param {Function|null} transformer - A macro's transformer, or null for
     *   a special form or auxiliary keyword.
     */
    defineKeyword(scope, name, keyword, transformer) {
        let bindings = this.keywordBindings.get(scope);
        if (bindings === undefined) {
            bindings = new Map();
            this.keywordBindings.set(scope, bindings);
        }
        bindings.set(name, { keyword, transformer });
    }

    /**
     * Unbinds a syntactic keyword, so that its name is looked up by name.
     * @param {number} scope - A library's scope, or 0 for a program's top level.
     * @param {string} name - The name.
     */
    forgetKeyword(scope, name) {
        this.keywordBindings.get(scope)?.delete(name);
    }

    /**
     * The syntactic keyword a name is bound to in a library, or at a
     * program's top level.
     * @param {number} scope - A library's scope, or 0 for a program's top level.
     * @param {string} name - A name.
     * @returns {{keyword: string, transformer: (Function|null)}|undefined} The
     *   keyword, or undefined if nothing there binds the name.
     */
    keywordBinding(scope, name) {
        return this.keywordBindings.get(scope)?.get(name);
    }
}

// =============================================================================
// Default Global Context (for backwards compatibility)
// =============================================================================

/**
 * The default global context, used when no explicit context is provided.
 * This maintains backwards compatibility with existing code.
 */
export const globalContext = new InterpreterContext();
globalContext.macroRegistry = globalMacroRegistry;
globalContext.currentMacroRegistry = globalMacroRegistry;

/**
 * Gets the current context (for transitional code).
 * @returns {InterpreterContext}
 */
export function getGlobalContext() {
    return globalContext;
}
