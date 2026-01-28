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
 * 
 * All mutable state is delegated to the globalContext from context.js for proper isolation.
 */

import { Symbol, intern } from './symbol.js';
import { Cons } from './cons.js';
import { SchemeTypeError } from './errors.js';
import { globalContext } from './context.js';

// =============================================================================
// Scope Management (delegated to globalContext)
// =============================================================================

/**
 * Creates a fresh scope identifier.
 * Delegates to globalContext for isolation.
 * @returns {number} A unique scope ID
 */
export function freshScope() {
    return globalContext.freshScope();
}

/**
 * Resets the scope counter. Used for testing.
 * @deprecated Use globalContext.reset() instead
 */
export function resetScopeCounter() {
    globalContext.resetScopeCounter();
}

// =============================================================================
// Syntax Object Interning (delegated to globalContext)
// =============================================================================

/**
 * Generates a cache key for a syntax object.
 * @param {string} name 
 * @param {Set<number>} scopes 
 * @returns {string}
 */
function getSyntaxKey(name, scopes) {
    if (scopes.size === 0) return name;
    // Sort scopes for canonical key
    const sortedScopes = [...scopes].sort((a, b) => a - b);
    return `${name}|${sortedScopes.join(',')}`;
}

/**
 * Returns a canonical (interned) SyntaxObject for the given name and scopes.
 * This ensures that identifiers with the same name and scopes are object-identical,
 * which is required for using them as keys in Map-based environments.
 * 
 * Delegates to globalContext's syntaxInternCache for isolation.
 * 
 * @param {string} name 
 * @param {Set<number>|Array<number>} scopes 
 * @param {Object} context - Source location info (optional, for error messages)
 * @returns {SyntaxObject}
 */
export function internSyntax(name, scopes, context = null) {
    const scopeSet = scopes instanceof Set ? scopes : new Set(scopes);
    const key = getSyntaxKey(name, scopeSet);

    if (globalContext.syntaxInternCache.has(key)) {
        return globalContext.syntaxInternCache.get(key);
    }

    // Note: We use the constructor directly here.
    // The constructor does NOT intern automatically to allow temporary objects if needed,
    // but typically internSyntax should be used.
    const obj = new SyntaxObject(name, scopeSet, context);
    globalContext.syntaxInternCache.set(key, obj);
    return obj;
}

/**
 * Clear the intern cache. Used for testing/reset.
 * @deprecated Use globalContext.reset() instead
 */
export function resetSyntaxCache() {
    globalContext.syntaxInternCache.clear();
}

/**
 * The global scope ID, used for top-level bindings.
 * @type {number}
 */
export const GLOBAL_SCOPE_ID = 0;

// =============================================================================
// Library Scope Environment Map (delegated to globalContext)
// =============================================================================

/**
 * Alias to globalContext.libraryScopeEnvMap for backwards compatibility.
 * @type {Map<number, Environment>}
 */
export const libraryScopeEnvMap = globalContext.libraryScopeEnvMap;

/**
 * Associates a library scope with its environment.
 * Delegates to globalContext for isolation.
 * @param {number} scope 
 * @param {Environment} env 
 */
export function registerLibraryScope(scope, env) {
    globalContext.registerLibraryScope(scope, env);
}

/**
 * Retrieves the environment associated with a library scope.
 * Delegates to globalContext for isolation.
 * @param {number} scope 
 * @returns {Environment|undefined}
 */
export function lookupLibraryEnv(scope) {
    return globalContext.lookupLibraryEnv(scope);
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
        return internSyntax(this.name, newScopes, this.context);
    }

    /**
     * Create a copy with a scope mark removed (for crossing binding boundaries).
     * @param {number} scope - The scope to remove
     * @returns {SyntaxObject} New syntax object without the mark
     */
    removeScope(scope) {
        const newScopes = new Set(this.scopes);
        newScopes.delete(scope);
        return internSyntax(this.name, newScopes, this.context);
    }

    /**
     * Create a copy that flips a scope mark (add if absent, remove if present).
     * This is used in anti-mark hygiene implementations.
     * When all scopes cancel out (empty set), returns a plain Symbol.
     * @param {number} scope - The scope to flip
     * @returns {SyntaxObject|Symbol} New syntax object with flipped mark, or Symbol if empty
     */
    flipScope(scope) {
        const newScopes = new Set(this.scopes);
        if (newScopes.has(scope)) {
            newScopes.delete(scope);
        } else {
            newScopes.add(scope);
        }
        // If all scopes cancelled, return a plain Symbol instead of empty-scoped SyntaxObject
        // This ensures proper equal? behavior after anti-mark + mark cancellation
        if (newScopes.size === 0) {
            return intern(this.name);
        }
        return internSyntax(this.name, newScopes, this.context);
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
        const name = syntaxObj instanceof SyntaxObject ? syntaxObj.name : syntaxObj.name;
        const idScopes = syntaxObj instanceof SyntaxObject ? syntaxObj.scopes : new Set();

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
    return internSyntax(name, scopes instanceof Set ? scopes : new Set(scopes));
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
    throw new SchemeTypeError('syntaxName', 1, 'symbol or syntax object', obj);
}

// =============================================================================
// Current Defining Scopes (delegated to globalContext)
// =============================================================================

/**
 * Push a defining scope onto the stack.
 * Call this when entering a library or module context.
 * Delegates to globalContext for isolation.
 * @param {number} scope - The scope ID
 */
export function pushDefiningScope(scope) {
    globalContext.pushDefiningScope(scope);
}

/**
 * Pop the current defining scope from the stack.
 * Call this when exiting a library or module context.
 * Delegates to globalContext for isolation.
 * @returns {number|undefined} The popped scope ID
 */
export function popDefiningScope() {
    return globalContext.popDefiningScope();
}

/**
 * Get all currently active defining scopes.
 * Delegates to globalContext for isolation.
 * @returns {number[]} Array of scope IDs
 */
export function getCurrentDefiningScopes() {
    return globalContext.getDefiningScopes();
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
 * Delegates to globalContext for isolation.
 * 
 * @param {string} name - The binding name
 * @param {any} [value] - The bound value (unused if GlobalRef is preferred)
 */
export function registerBindingWithCurrentScopes(name, value) {
    const definingScopes = globalContext.getDefiningScopes();
    // Determine scope set (default to GLOBAL_SCOPE_ID if empty)
    const scopes = definingScopes.length > 0
        ? new Set(definingScopes)
        : new Set([GLOBAL_SCOPE_ID]);

    // Determine the specific defining scope (for Environment resolution)
    const definingScope = definingScopes.length > 0
        ? definingScopes[definingScopes.length - 1]
        : null;

    // Always bind as a GlobalRef to ensure dynamic lookup in the environment
    globalScopeRegistry.bind(name, scopes, new GlobalRef(name, definingScope));
}

/**
 * Clear the defining scope stack. Used for testing.
 * @deprecated Use globalContext.reset() instead
 */
export function clearDefiningScopes() {
    globalContext.definingScopes = [];
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
        // recursively unwrap the content
        // If the content is a string, it's an identifier name -> Symbol
        if (typeof obj.name === 'string') {
            return intern(obj.name);
        }
        return unwrapSyntax(obj.name);
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
    // Base case: return as is (Symbol, Number, String, etc)
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

/**
 * Add a scope mark to all identifiers in an expression.
 * This is used by binding forms (let, lambda) to mark identifiers
 * in their body with the binding's scope for hygiene purposes.
 * 
 * @param {any} exp - The expression to process
 * @param {number} scope - The scope ID to add
 * @returns {any} Expression with scope marks added to all identifiers
 */
export function addScopeToExpression(exp, scope) {
    // Handle Symbol - wrap as SyntaxObject with scope
    if (exp instanceof Symbol) {
        return internSyntax(exp.name, new Set([scope]));
    }

    // Handle SyntaxObject - add scope to existing
    if (exp instanceof SyntaxObject) {
        return exp.addScope(scope);
    }

    // Handle Cons - recurse on car and cdr
    if (exp instanceof Cons) {
        const car = addScopeToExpression(exp.car, scope);
        const cdr = addScopeToExpression(exp.cdr, scope);
        return new Cons(car, cdr);
    }

    // Handle arrays (vectors)
    if (Array.isArray(exp)) {
        return exp.map(e => addScopeToExpression(e, scope));
    }

    // Primitives pass through unchanged
    return exp;
}

/**
 * Flip a scope mark on all identifiers in an expression.
 * Used for Dybvig anti-mark hygiene.
 * 
 * @param {any} exp - The expression to process
 * @param {number} scope - The scope ID to flip
 * @returns {any} Expression with scope marks flipped on all identifiers
 */
export function flipScopeInExpression(exp, scope) {
    // Handle Symbol - wrap as SyntaxObject with scope (flip on empty = add)
    if (exp instanceof Symbol) {
        return internSyntax(exp.name, new Set([scope]));
    }

    // Handle SyntaxObject - flip scope on existing
    if (exp instanceof SyntaxObject) {
        return exp.flipScope(scope);
    }

    // Handle Cons - recurse on car and cdr
    if (exp instanceof Cons) {
        const car = flipScopeInExpression(exp.car, scope);
        const cdr = flipScopeInExpression(exp.cdr, scope);
        return new Cons(car, cdr);
    }

    // Handle arrays (vectors)
    if (Array.isArray(exp)) {
        return exp.map(e => flipScopeInExpression(e, scope));
    }

    // Primitives pass through unchanged
    return exp;
}
