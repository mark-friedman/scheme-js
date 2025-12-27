/**
 * Syntax Objects for Hygienic Macros
 * 
 * Implements Dybvig-style hygiene using marks (scopes).
 * Each identifier carries a set of scope marks that determine how it resolves.
 * 
 * Key concepts:
 * - A "scope" represents a lexical context (macro definition, expansion, binding form)
 * - Each identifier tracks which scopes it's been through via "marks"
 * - Resolution finds the binding whose scopes are a subset of the identifier's scopes
 */

import { Symbol, intern } from './symbol.js';
import { Cons } from './cons.js';

// =============================================================================
// Scope Management
// =============================================================================

/** Counter for generating unique scope IDs */
let scopeCounter = 0;

/**
 * Creates a fresh scope identifier.
 * @returns {number} A unique scope ID
 */
export function freshScope() {
    return ++scopeCounter;
}

/**
 * Resets the scope counter. Used for testing.
 */
export function resetScopeCounter() {
    scopeCounter = 0;
}

/**
 * The global scope ID, used for top-level bindings.
 * @type {number}
 */
export const GLOBAL_SCOPE_ID = 0;

// =============================================================================
// Library Scope Environment Map
// =============================================================================

/**
 * Maps a library's defining scope ID to its runtime Environment.
 * Used by GlobalRef to resolve bindings within the correct library environment.
 * @type {Map<number, Environment>}
 */
export const libraryScopeEnvMap = new Map();

/**
 * Associates a library scope with its environment.
 * @param {number} scope 
 * @param {Environment} env 
 */
export function registerLibraryScope(scope, env) {
    libraryScopeEnvMap.set(scope, env);
}

/**
 * Retrieves the environment associated with a library scope.
 * @param {number} scope 
 * @returns {Environment|undefined}
 */
export function lookupLibraryEnv(scope) {
    return libraryScopeEnvMap.get(scope);
}

// =============================================================================
// SyntaxObject Class
// =============================================================================

/**
 * A syntax object wraps an identifier with scope information.
 * 
 * Two identifiers are considered the same binding if:
 * - They have the same name
 * - Their scope marks resolve to the same binding
 */
export class SyntaxObject {
    /**
     * @param {string} name - The identifier name
     * @param {Set<number>} scopes - Set of scope marks
     * @param {Object} context - Source location info (optional, for error messages)
     */
    constructor(name, scopes = new Set(), context = null) {
        this.name = name;
        this.scopes = scopes instanceof Set ? scopes : new Set(scopes);
        this.context = context;
    }

    /**
     * Create a copy with an additional scope mark.
     * @param {number} scope - The scope to add
     * @returns {SyntaxObject} New syntax object with the mark
     */
    addScope(scope) {
        const newScopes = new Set(this.scopes);
        newScopes.add(scope);
        return new SyntaxObject(this.name, newScopes, this.context);
    }

    /**
     * Create a copy with a scope mark removed (for crossing binding boundaries).
     * @param {number} scope - The scope to remove
     * @returns {SyntaxObject} New syntax object without the mark
     */
    removeScope(scope) {
        const newScopes = new Set(this.scopes);
        newScopes.delete(scope);
        return new SyntaxObject(this.name, newScopes, this.context);
    }

    /**
     * Create a copy that flips a scope mark (add if absent, remove if present).
     * This is used in anti-mark hygiene implementations.
     * @param {number} scope - The scope to flip
     * @returns {SyntaxObject} New syntax object with flipped mark
     */
    flipScope(scope) {
        const newScopes = new Set(this.scopes);
        if (newScopes.has(scope)) {
            newScopes.delete(scope);
        } else {
            newScopes.add(scope);
        }
        return new SyntaxObject(this.name, newScopes, this.context);
    }

    /**
     * bound-identifier=? : Two identifiers that would bind the same if
     * used in binding position at the same point.
     * @param {SyntaxObject} other 
     * @returns {boolean}
     */
    boundIdentifierEquals(other) {
        if (!(other instanceof SyntaxObject)) return false;
        if (this.name !== other.name) return false;
        if (this.scopes.size !== other.scopes.size) return false;
        for (const s of this.scopes) {
            if (!other.scopes.has(s)) return false;
        }
        return true;
    }

    /**
     * Convert to a plain Symbol for backward compatibility with existing code.
     * @returns {Symbol}
     */
    toSymbol() {
        return intern(this.name);
    }

    toString() {
        const scopeStr = this.scopes.size > 0
            ? `{${[...this.scopes].sort().join(',')}}`
            : '';
        return `#<syntax ${this.name}${scopeStr}>`;
    }
}

// =============================================================================
// Scope Binding Registry
// =============================================================================

/**
 * The ScopeBindingRegistry maps (name, scopes) → binding information.
 * 
 * When resolving an identifier:
 * 1. Find all bindings for that name
 * 2. Find the binding whose scopes are a maximal subset of the identifier's scopes
 * 3. Return that binding, or null if none found
 */
export class ScopeBindingRegistry {
    constructor() {
        /**
         * Map from name to list of {scopes: Set, binding: any}
         * @type {Map<string, Array<{scopes: Set<number>, binding: any}>>}
         */
        this.bindings = new Map();
    }

    /**
     * Register a binding for a name with specific scopes.
     * @param {string} name - The identifier name
     * @param {Set<number>} scopes - The scopes where this binding is visible
     * @param {any} binding - The binding information (value, type, etc.)
     */
    bind(name, scopes, binding) {
        if (!this.bindings.has(name)) {
            this.bindings.set(name, []);
        }
        this.bindings.get(name).push({ scopes: new Set(scopes), binding });
    }

    /**
     * Resolve an identifier to its binding.
     * 
     * Finds the binding with the largest scope set that is a subset of
     * the identifier's scopes. This implements the "most specific binding" rule.
     * 
     * @param {SyntaxObject} syntaxObj - The identifier to resolve
     * @returns {any|null} The binding, or null if not found in registry
     */
    resolve(syntaxObj) {
        const name = syntaxObj.name;
        const idScopes = syntaxObj.scopes;

        if (!this.bindings.has(name)) {
            return null;
        }

        let bestBinding = null;
        let bestSize = -1;

        for (const { scopes, binding } of this.bindings.get(name)) {
            // Check if binding's scopes are a subset of identifier's scopes
            let isSubset = true;
            for (const s of scopes) {
                if (!idScopes.has(s)) {
                    isSubset = false;
                    break;
                }
            }

            if (isSubset && scopes.size >= bestSize) {
                bestBinding = binding;
                bestSize = scopes.size;
            }
        }

        return bestBinding;
    }

    /**
     * Check if a name has any bindings registered.
     * @param {string} name 
     * @returns {boolean}
     */
    hasBindings(name) {
        return this.bindings.has(name) && this.bindings.get(name).length > 0;
    }

    /**
     * Clear all bindings. Used for testing.
     */
    clear() {
        this.bindings.clear();
    }
}

// =============================================================================
// Global Scope Registry
// =============================================================================

/**
 * The global scope binding registry, used for macro-introduced bindings
 * and referential transparency.
 */
export const globalScopeRegistry = new ScopeBindingRegistry();

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Wrap a Symbol as a SyntaxObject with the given scopes.
 * @param {Symbol|string} sym - The symbol or name
 * @param {Set<number>|Array<number>} scopes - The scopes
 * @returns {SyntaxObject}
 */
export function syntaxWrap(sym, scopes = new Set()) {
    const name = sym instanceof Symbol ? sym.name : sym;
    return new SyntaxObject(name, scopes instanceof Set ? scopes : new Set(scopes));
}

/**
 * Check if an object is a SyntaxObject.
 * @param {any} obj 
 * @returns {boolean}
 */
export function isSyntaxObject(obj) {
    return obj instanceof SyntaxObject;
}

/**
 * Get the name from a symbol or syntax object.
 * @param {Symbol|SyntaxObject} obj 
 * @returns {string}
 */
export function syntaxName(obj) {
    if (obj instanceof SyntaxObject) return obj.name;
    if (obj instanceof Symbol) return obj.name;
    throw new Error(`Expected symbol or syntax object, got ${obj}`);
}

// =============================================================================
// Current Defining Scopes (for library loading)
// =============================================================================

/**
 * Stack of currently active defining scopes.
 * When a library is being loaded, its scope is pushed here.
 * Define forms register bindings with all active scopes.
 */
let currentDefiningScopes = [];

/**
 * Push a defining scope onto the stack.
 * Call this when entering a library or module context.
 * @param {number} scope - The scope ID
 */
export function pushDefiningScope(scope) {
    currentDefiningScopes.push(scope);
}

/**
 * Pop the current defining scope from the stack.
 * Call this when exiting a library or module context.
 * @returns {number|undefined} The popped scope ID
 */
export function popDefiningScope() {
    return currentDefiningScopes.pop();
}

/**
 * Get all currently active defining scopes.
 * @returns {number[]} Array of scope IDs
 */
export function getCurrentDefiningScopes() {
    return [...currentDefiningScopes];
}

/**
 * Reference to a global variable (dynamic lookup).
 * Can optionally carry the defining scope ID to locate the correct library environment.
 */
export class GlobalRef {
    /**
     * @param {string} name 
     * @param {number|null} scope - The defining scope ID (if inside a library)
     */
    constructor(name, scope = null) {
        this.name = name;
        this.scope = scope;
    }
}

/**
 * Register a binding with all currently active scopes.
 * Call this when a define is evaluated during library loading.
 * 
 * @param {string} name - The binding name
 * @param {any} [value] - The bound value (unused if GlobalRef is preferred)
 */
export function registerBindingWithCurrentScopes(name, value) {
    // Determine scope set (default to GLOBAL_SCOPE_ID if empty)
    const scopes = currentDefiningScopes.length > 0
        ? new Set(currentDefiningScopes)
        : new Set([GLOBAL_SCOPE_ID]);

    // Determine the specific defining scope (for Environment resolution)
    const definingScope = currentDefiningScopes.length > 0
        ? currentDefiningScopes[currentDefiningScopes.length - 1]
        : null;

    // Always bind as a GlobalRef to ensure dynamic lookup in the environment
    globalScopeRegistry.bind(name, scopes, new GlobalRef(name, definingScope));
}

/**
 * Clear the defining scope stack. Used for testing.
 */
export function clearDefiningScopes() {
    currentDefiningScopes = [];
}

/**
 * Compares two identifiers for equality (same name and scopes).
 * Handles both Symbols and SyntaxObjects.
 * @param {Symbol|SyntaxObject} id1 
 * @param {Symbol|SyntaxObject} id2 
 * @returns {boolean}
 */
export function identifierEquals(id1, id2) {
    if (id1 instanceof SyntaxObject) {
        if (id2 instanceof SyntaxObject) {
            return id1.boundIdentifierEquals(id2);
        } else if (id2 instanceof Symbol) {
            // Compare syntax object with symbol (treat symbol as empty scope)
            return id1.scopes.size === 0 && id1.name === id2.name;
        }
    } else if (id1 instanceof Symbol) {
        if (id2 instanceof SyntaxObject) {
            return id2.scopes.size === 0 && id2.name === id1.name;
        } else if (id2 instanceof Symbol) {
            return id1.name === id2.name;
        }
    }
    return false;
}

/**
 * Unwrap a syntax object to get the underlying symbol/value.
 * If strictly a symbol is needed, use toSymbol().
 * @param {any} obj 
 * @returns {any}
 */
export function unwrapSyntax(obj) {
    if (obj instanceof SyntaxObject) {
        return intern(obj.name);
    }
    // Recursively unwrap Cons structures
    if (obj instanceof Cons) {
        const car = unwrapSyntax(obj.car);
        const cdr = unwrapSyntax(obj.cdr);
        return new Cons(car, cdr);
    }
    // Handle arrays (vectors)
    if (Array.isArray(obj)) {
        return obj.map(unwrapSyntax);
    }
    return obj;
}

/**
 * Get scope set from an object (empty if not syntax object).
 * @param {any} obj
 * @returns {Set<number>}
 */
export function syntaxScopes(obj) {
    if (obj instanceof SyntaxObject) {
        return obj.scopes;
    }
    return new Set();
}

