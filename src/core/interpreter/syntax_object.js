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
import { labelReferencesRead } from './reader/datum_labels.js';

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
    // Sort scopes for canonical key. An identifier carries one to three
    // scopes almost always -- an expansion's, and a library's -- and every
    // identifier a template introduces is interned, so those sizes are
    // ordered without making an array.
    switch (scopes.size) {
        case 0:
            return name;
        case 1:
            for (const a of scopes) return `${name}|${a}`;
        case 2: {
            const [a, b] = scopes;
            return a < b ? `${name}|${a},${b}` : `${name}|${b},${a}`;
        }
        default: {
            const sortedScopes = [...scopes].sort((a, b) => a - b);
            return `${name}|${sortedScopes.join(',')}`;
        }
    }
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

    const interned = globalContext.syntaxInternCache.get(key);
    if (interned !== undefined) return interned;

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

/**
 * The library scope an identifier carries, if it carries one: the scope of
 * the library whose macro introduced it into an expansion. `transcribe` in
 * `syntax_rules.js` adds it, to every identifier a library macro's template
 * introduces that was not written in a library already.
 * @param {Symbol|SyntaxObject} identifier - An identifier.
 * @returns {number|null} The scope, or null.
 */
export function libraryScopeOf(identifier) {
    if (!(identifier instanceof SyntaxObject)) return null;
    if (identifier.libraryScope === undefined) {
        identifier.libraryScope = null;
        for (const scope of identifier.scopes) {
            if (globalContext.lookupLibraryEnv(scope) !== undefined) {
                identifier.libraryScope = scope;
                break;
            }
        }
    }
    return identifier.libraryScope;
}

/**
 * The scope an identifier is used in: the library whose macro introduced it,
 * if one did; otherwise `scope`, if given; otherwise the library being
 * loaded, or the program's top level.
 * @param {Symbol|SyntaxObject} identifier - An identifier.
 * @param {number|null} scope - Where it is used, if not where the analyzer is.
 * @returns {number} A library's scope, or `GLOBAL_SCOPE_ID`.
 */
function scopeOfUse(identifier, scope) {
    const defining = globalContext.definingScopes;
    return libraryScopeOf(identifier)
        ?? scope
        ?? (defining.length > 0 ? defining[defining.length - 1] : GLOBAL_SCOPE_ID);
}

/**
 * The syntactic keyword an identifier names where it is used: the keyword
 * bound to its name there (`InterpreterContext.defineKeyword`), which may
 * have another name, as `(import (rename (scheme base) (if when-true)))`
 * binds `when-true` to `if`; otherwise its own name.
 *
 * A macro's pattern literal, which the macro's transformer compares rather
 * than analyzes, is given the scope where the macro was defined.
 *
 * @param {Symbol|SyntaxObject} identifier - An identifier.
 * @param {number|null} [scope=null] - Where it is used, if not where the
 *   analyzer is: a library's scope, or `GLOBAL_SCOPE_ID`.
 * @returns {string} The keyword's own name, or the identifier's.
 */
export function keywordName(identifier, scope = null) {
    return keywordBindingOf(identifier, scope)?.keyword ?? identifier.name;
}

/**
 * The syntactic keyword bound to an identifier's name where it is used, if
 * one is: its own name, and, for a macro, its transformer.
 * @param {Symbol|SyntaxObject} identifier - An identifier.
 * @param {number|null} [scope=null] - Where it is used, if not where the
 *   analyzer is.
 * @returns {{keyword: string, transformer: (Function|null)}|undefined}
 */
export function keywordBindingOf(identifier, scope = null) {
    if (globalContext.keywordBindings.size === 0) return undefined;
    return globalContext.keywordBinding(scopeOfUse(identifier, scope), identifier.name);
}

/**
 * What an identifier in operator position names: a macro, by its
 * transformer, or else a keyword, which may be a special form's.
 *
 * A macro defined locally -- by `let-syntax`, `letrec-syntax` or a body's
 * `define-syntax` -- comes first, by name; then the keyword bound to the
 * name where the identifier is used, which keeps libraries' macros apart
 * (`InterpreterContext.defineKeyword`); then, for a name bound nowhere, the
 * macro defined under it for the whole process, if there is one.
 *
 * @param {Symbol|SyntaxObject} operator - The identifier.
 * @param {InterpreterContext} ctx - The interpreter context.
 * @returns {{keyword: string, transformer: (Function|null)}} The keyword's
 *   own name, and a macro's transformer or null.
 */
export function operatorKeyword(operator, ctx) {
    const name = syntaxName(operator);
    for (let registry = ctx.currentMacroRegistry; registry !== null && registry !== ctx.macroRegistry; registry = registry.parent) {
        const local = registry.macros.get(name);
        if (local !== undefined) return { keyword: name, transformer: local };
    }
    const bound = keywordBindingOf(operator);
    if (bound !== undefined) return bound;
    return { keyword: name, transformer: ctx.currentMacroRegistry.lookup(name) ?? null };
}

/**
 * Binds a macro defined by `define-syntax` or `define-macro` where it is
 * defined: in the library being loaded, which keeps it from any other
 * library's macro of the same name. A program's top level defines by name,
 * for the whole process, so a definition there unbinds whatever the top
 * level had imported under the name.
 * @param {string} name - The macro's name.
 * @param {Function} transformer - Its transformer.
 */
export function bindDefinedMacro(name, transformer) {
    const defining = globalContext.definingScopes;
    if (defining.length > 0) {
        globalContext.defineKeyword(defining[defining.length - 1], name, name, transformer);
    } else {
        globalContext.forgetKeyword(GLOBAL_SCOPE_ID, name);
    }
}

/**
 * Where an identifier a library's macro introduced refers to its binding: the
 * library's environment, or null if looking its name up where it is used
 * finds that binding.
 *
 * Macros are referentially transparent (R7RS 4.3), so the identifier means the
 * library's binding of its name, exported or not. Looking the name up where
 * it is used finds that binding in three cases, and those are left as
 * references by name, which the compiler tier can compile:
 *  - the use is in the library itself, so its environment is the library's,
 *    and a plain name there denotes a global, locals being alpha-renamed;
 *  - the library has no binding of the name of its own, so it is one the
 *    library and its user both find in the environment they share, or one
 *    the library defines after this use within it;
 *  - the use site holds the same procedure under the name, as a program or
 *    library that imports it does. Every derived form in the standard
 *    libraries expands into calls of procedures, so this is what keeps their
 *    expansions compilable. A variable that is not a procedure is not shared
 *    this way: an import is a copy, and an assignment in the library would
 *    leave the copy behind.
 *
 * @param {SyntaxObject} identifier - A free identifier, bound by nothing
 *   local to its use.
 * @param {number} scope - Its library scope, from `libraryScopeOf`.
 * @param {boolean} [assigning=false] - Whether it is the target of a `set!`,
 *   which must reach the library's binding, never a copy of it.
 * @returns {Environment|null} The library's environment, or null.
 */
export function libraryBindingEnv(identifier, scope, assigning = false) {
    const libEnv = globalContext.lookupLibraryEnv(scope);
    const name = identifier.name;
    if (!libEnv.bindings.has(name)) return null;
    const defining = globalContext.definingScopes;
    const current = defining.length > 0 ? defining[defining.length - 1] : null;
    if (current === scope) return null;
    if (!assigning) {
        // A library is loaded into the environment that first imported it,
        // which is therefore the environment of a use outside any library.
        const useEnv = (current !== null ? globalContext.lookupLibraryEnv(current) : undefined) ?? libEnv.parent;
        const holder = useEnv ? useEnv.findEnv(name) : null;
        const value = libEnv.bindings.get(name);
        if (holder !== null && typeof value === 'function' && holder.bindings.get(name) === value) return null;
    }
    return libEnv;
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
        /**
         * The library scope among `scopes`, null if none, or undefined until
         * `libraryScopeOf` has looked. A scope is a library's from the moment
         * it is made, so the answer never changes.
         * @type {number|null|undefined}
         */
        this.libraryScope = undefined;
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
    // A syntax object's content is an identifier's name, which becomes the
    // symbol, or a datum, unwrapped in turn
    const unwrapOne = (x, walk) => {
        if (!(x instanceof SyntaxObject)) return x;
        return typeof x.name === 'string' ? intern(x.name) : walk(x.name);
    };
    if (obj instanceof SyntaxObject && typeof obj.name === 'string') return intern(obj.name);
    if (obj instanceof Cons || Array.isArray(obj) || obj instanceof SyntaxObject) {
        return copyDatum(obj, unwrapOne);
    }
    // Base case: return as is (Symbol, Number, String, etc)
    return obj;
}

/**
 * Copies a datum's pairs and vectors, applying `leaf` to everything else in
 * it.
 *
 * A literal may share structure and be circular (R7RS 2.4), through datum
 * labels, and a copy made as a tree duplicates what is shared and never
 * finishes a cycle. So the copy is made as a graph, each pair and vector
 * copied once, wherever that can arise: once the reader has read a label
 * reference, or when a tree copy grows past `TREE_COPY_LIMIT`, as one of code
 * built at run time with a cycle in it does. Everywhere else it is a tree,
 * which costs no table of copies.
 *
 * @param {*} datum - The datum.
 * @param {function(*, function(*): *): *} leaf - What to make of anything
 *   not a pair or vector; given the copier too, for content of its own.
 * @returns {*} The copy.
 */
function copyDatum(datum, leaf) {
    if (!labelReferencesRead()) {
        treeCopiesLeft = TREE_COPY_LIMIT;
        const walk = (x) => copyTree(x, leaf, walk);
        try {
            return walk(datum);
        } catch (e) {
            if (e !== TREE_TOO_LARGE) throw e;
        }
    }
    const copies = new Map();
    const walk = (x) => copyGraph(x, leaf, copies, walk);
    return walk(datum);
}

/**
 * How many pairs and vectors a copy of code makes as a tree before it starts
 * again as a graph: more than any program's literal, and few enough that a
 * cycle is found quickly.
 * @type {number}
 */
const TREE_COPY_LIMIT = 100000;

/** Thrown to abandon a tree copy that reached `TREE_COPY_LIMIT`. */
const TREE_TOO_LARGE = { reason: 'a tree copy reached its limit' };

/** How many more pairs and vectors the tree copy under way may make. */
let treeCopiesLeft = 0;

/**
 * `copyDatum`'s copy as a tree. A list's pairs are copied along its spine
 * iteratively, so a long list does not take a JavaScript call per element.
 * @param {*} x - The datum.
 * @param {function(*, function(*): *): *} leaf - As for `copyDatum`.
 * @param {function(*): *} walk - This, closed over `leaf` and itself.
 * @returns {*} The copy.
 * @throws {Object} `TREE_TOO_LARGE`, once the copy reaches the limit.
 */
function copyTree(x, leaf, walk) {
    if (x instanceof Cons) {
        if (--treeCopiesLeft < 0) throw TREE_TOO_LARGE;
        const head = new Cons(null, null);
        let from = x;
        let to = head;
        for (;;) {
            to.car = copyTree(from.car, leaf, walk);
            const next = from.cdr;
            if (!(next instanceof Cons)) {
                to.cdr = copyTree(next, leaf, walk);
                return head;
            }
            if (--treeCopiesLeft < 0) throw TREE_TOO_LARGE;
            to = to.cdr = new Cons(null, null);
            from = next;
        }
    }
    if (Array.isArray(x)) {
        if (--treeCopiesLeft < 0) throw TREE_TOO_LARGE;
        const copy = new Array(x.length);
        for (let i = 0; i < x.length; i++) copy[i] = copyTree(x[i], leaf, walk);
        return copy;
    }
    return leaf(x, walk);
}

/**
 * `copyDatum`'s copy as a graph: each pair and vector copied once.
 * @param {*} x - The datum.
 * @param {function(*, function(*): *): *} leaf - As for `copyDatum`.
 * @param {Map} copies - The copies made so far, by original.
 * @param {function(*): *} walk - This, closed over its arguments.
 * @returns {*} The copy.
 */
function copyGraph(x, leaf, copies, walk) {
    if (x instanceof Cons) {
        const done = copies.get(x);
        if (done !== undefined) return done;
        const head = new Cons(null, null);
        copies.set(x, head);
        let from = x;
        let to = head;
        for (;;) {
            to.car = walk(from.car);
            const next = from.cdr;
            if (!(next instanceof Cons)) {
                to.cdr = walk(next);
                return head;
            }
            const copied = copies.get(next);
            if (copied !== undefined) {
                to.cdr = copied;
                return head;
            }
            to = to.cdr = new Cons(null, null);
            copies.set(next, to);
            from = next;
        }
    }
    if (Array.isArray(x)) {
        const done = copies.get(x);
        if (done !== undefined) return done;
        const copy = new Array(x.length);
        copies.set(x, copy);
        for (let i = 0; i < x.length; i++) copy[i] = walk(x[i]);
        return copy;
    }
    return leaf(x, walk);
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

    // Pairs and vectors, as a graph, which a literal written with datum
    // labels may be
    if (exp instanceof Cons || Array.isArray(exp)) {
        return copyDatum(exp, (x) => {
            if (x instanceof Symbol) return internSyntax(x.name, new Set([scope]));
            if (x instanceof SyntaxObject) return x.flipScope(scope);
            return x;
        });
    }

    // Primitives pass through unchanged
    return exp;
}
