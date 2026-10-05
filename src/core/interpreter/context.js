/**
 * Interpreter Context
 * 
 * Encapsulates the expander's mutable state for a single interpreter
 * instance: scopes, interned syntax, the tables keyed by scope, and macros.
 * This enables multiple isolated interpreters to coexist without
 * sharing global state, which is essential for:
 * - Parallel test execution
 * - Multi-tenant REPL environments
 * - Sandboxed evaluation
 *
 * The registries of loaded libraries and the features `cond-expand` tests
 * are not here: they are Scheme's, in (scheme-js library-system), reached
 * from JavaScript through library_registry.js.
 */

import { MacroRegistry, globalMacroRegistry } from './macro_registry.js';

/**
 * The scope of a program's top level. No scope `freshScope` makes is this
 * one: were the first library a process loads given it, it would be taken
 * for the top level, and the top level for it.
 * @type {number}
 */
export const GLOBAL_SCOPE_ID = 0;

/**
 * The first scope `freshScope` makes.
 * @type {number}
 */
const FIRST_FRESH_SCOPE = GLOBAL_SCOPE_ID + 1;

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
 * Holds the expander's mutable state for a single interpreter instance.
 * 
 * When creating interpreters that should be isolated from each other,
 * each should have its own InterpreterContext instance.
 */
export class InterpreterContext {
    constructor() {
        // Counters
        /** Scope ID counter for unique scope marks */
        this.scopeCounter = FIRST_FRESH_SCOPE;
        /** Unique ID counter for analyzed variables */
        this.uniqueIdCounter = 0;

        // Caches and Registries
        /**
         * Interned SyntaxObjects: key → SyntaxObject. What a library registry
         * made for a while interned goes with it (`leavePrivateLibraries`).
         */
        this.syntaxInternCache = new Map();
        /** Scope → Binding registry */
        this.scopeRegistry = new ScopeBindingRegistry();
        /**
         * Each library's scope, mapped to its environment.
         *
         * A process that loads libraries into many registries must not keep
         * every library it has loaded, so a registry made for a while takes
         * the entries made while it was current with it when it goes
         * (`leavePrivateLibraries`). A library is needed by scope for as
         * long as something naming its scope can be expanded -- its macros,
         * and what they expand into -- and each such macro holds the library,
         * and puts its entry back when it expands where the entry has gone
         * (`compileSyntaxRules` in syntax_rules.js).
         *
         * Not held weakly: a `WeakRef` keeps what it refers to alive until the
         * job that made or read it ends, and a program run from start to end
         * in one job, as a benchmark runs many, would keep every library it
         * loaded.
         * @type {Map<number, Environment>}
         */
        this.libraryScopeEnvMap = new Map();
        /**
         * The library registries made for a while that are current, inner
         * ones last, each with where the logs below stood when it began.
         * @type {Array<{scopes: number, syntax: number, firstScope: number}>}
         */
        this.privateLibraries = [];
        /**
         * The scopes entered in `libraryScopeEnvMap`, and the keys entered in
         * `syntaxInternCache`, while a registry made for a while was current,
         * in the order they were entered.
         * @type {number[]}
         */
        this.libraryScopeLog = [];
        /** @type {string[]} */
        this.syntaxInternLog = [];
        /** The macros defined by name for the whole process, which this context's sees too. */
        this.macroRegistry = new MacroRegistry(globalMacroRegistry);

        /** Baseline macro names for reset */
        this.baselineMacroNames = null;

        /** Stack of currently active defining scopes (for library loading) */
        this.definingScopes = [];

        /**
         * The syntactic keywords a program's top level binds, by its scope,
         * `GLOBAL_SCOPE_ID`. Each name maps to the keyword's own name and,
         * for a macro, its transformer. See `defineKeyword`.
         * @type {Map<number, Map<string, {keyword: string, transformer: (Function|null)}>>}
         */
        this.keywordBindings = new Map();
        /**
         * The syntactic keywords each library binds -- the macros it defines
         * and the keywords it imports, under the names it gives them -- by
         * the library's environment, so that they go when it does: they hold
         * transformers, which hold their own libraries.
         * @type {WeakMap<Environment, Map<string, {keyword: string, transformer: (Function|null)}>>}
         */
        this.libraryKeywords = new WeakMap();
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
        this.scopeCounter = FIRST_FRESH_SCOPE;
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
     * Interns a syntax object under its key.
     * @param {string} key - From `getSyntaxKey`.
     * @param {SyntaxObject} obj - The syntax object.
     */
    internSyntaxObject(key, obj) {
        this.syntaxInternCache.set(key, obj);
        if (this.privateLibraries.length > 0) this.syntaxInternLog.push(key);
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
    // Full Reset (For Testing)
    // =========================================================================

    /**
     * Resets all state in this context to initial values.
     * Useful for test isolation.
     */
    reset() {
        this.scopeCounter = FIRST_FRESH_SCOPE;
        this.uniqueIdCounter = 0;
        this.syntaxInternCache.clear();
        this.syntaxInternLog = [];
        this.scopeRegistry.clear();
        this.libraryScopeEnvMap.clear();
        this.libraryScopeLog = [];
        this.resetMacroRegistry();
        this.definingScopes = [];
        this.keywordBindings.clear();
        this.libraryKeywords = new WeakMap();
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
        if (this.libraryScopeEnvMap.get(scope) === env) return;
        this.libraryScopeEnvMap.set(scope, env);
        if (this.privateLibraries.length > 0) this.libraryScopeLog.push(scope);
    }

    /**
     * Look up the environment for a library scope.
     * @param {number} scope - The scope ID
     * @returns {Environment|undefined} The library's environment, or
     *   undefined if the scope is no library's, or its entry has gone.
     */
    lookupLibraryEnv(scope) {
        return this.libraryScopeEnvMap.get(scope);
    }

    /**
     * Begins a library registry made for a while (`withPrivateLibraries` in
     * library_registry.js): what the tables keyed by scope gain from now on
     * is logged, for it to take with it when it ends.
     */
    enterPrivateLibraries() {
        this.privateLibraries.push({
            scopes: this.libraryScopeLog.length,
            syntax: this.syntaxInternLog.length,
            firstScope: this.scopeCounter
        });
    }

    /**
     * Ends the innermost library registry made for a while, taking with it
     * the library scopes entered since it began -- its libraries', and those
     * its programs' use of other registries' macros put back -- and the
     * syntax objects interned since that hold a scope made since. Nothing
     * outside can make such a syntax object again, except from one that came
     * out of the registry, which keeps its own identity; one interned since
     * from older scopes alone is left, for an enclosing registry to take.
     */
    leavePrivateLibraries() {
        const { scopes, syntax, firstScope } = this.privateLibraries.pop();
        for (const scope of this.libraryScopeLog.splice(scopes)) this.libraryScopeEnvMap.delete(scope);
        const outer = this.privateLibraries.length > 0;
        for (const key of this.syntaxInternLog.splice(syntax)) {
            const obj = this.syntaxInternCache.get(key);
            if (obj === undefined) continue;
            let madeSince = false;
            for (const scope of obj.scopes) {
                if (scope >= firstScope) { madeSince = true; break; }
            }
            if (madeSince) this.syntaxInternCache.delete(key);
            else if (outer) this.syntaxInternLog.push(key);
        }
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
     * A name defined as a variable where it had named a macro is bound with
     * no keyword, so that it is not looked up by name (`shadowMacro` in
     * syntax_object.js).
     *
     * @param {number} scope - A library's scope, or 0 for a program's top level.
     * @param {string} name - The name bound.
     * @param {string|null} keyword - The keyword's own name, or null for a
     *   variable.
     * @param {Function|null} transformer - A macro's transformer, or null for
     *   a special form, an auxiliary keyword or a variable.
     */
    defineKeyword(scope, name, keyword, transformer) {
        this.keywordsIn(scope, true).set(name, { keyword, transformer });
    }

    /**
     * Unbinds a syntactic keyword, so that its name is looked up by name.
     * @param {number} scope - A library's scope, or 0 for a program's top level.
     * @param {string} name - The name.
     */
    forgetKeyword(scope, name) {
        this.keywordsIn(scope, false)?.delete(name);
    }

    /**
     * The keywords bound in a library, kept with its environment, or at a
     * program's top level, kept by its scope.
     * @param {number} scope - A library's scope, or 0 for a program's top level.
     * @param {boolean} make - Whether to make the table if there is none.
     * @returns {Map<string, {keyword: string, transformer: (Function|null)}>|undefined}
     */
    keywordsIn(scope, make) {
        const env = scope === GLOBAL_SCOPE_ID ? undefined : this.lookupLibraryEnv(scope);
        // A scope that is no library's is kept by number, as the top level's is.
        const tables = env === undefined ? this.keywordBindings : this.libraryKeywords;
        const key = env === undefined ? scope : env;
        let bindings = tables.get(key);
        if (bindings === undefined && make) {
            bindings = new Map();
            tables.set(key, bindings);
        }
        return bindings;
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
        return this.keywordsIn(scope, false)?.get(name);
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

/**
 * Gets the current context (for transitional code).
 * @returns {InterpreterContext}
 */
export function getGlobalContext() {
    return globalContext;
}
