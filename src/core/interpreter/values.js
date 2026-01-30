/**
 * Scheme Runtime Value Types
 * 
 * These classes and factory functions represent first-class values in the Scheme runtime
 * that are not primitive JavaScript values. They are used for closures, continuations,
 * and multiple return values.
 * 
 * IMPORTANT: Closures and Continuations are now created as callable JavaScript functions
 * with marker properties. This allows them to be called directly from JavaScript code
 * anywhere they appear (in variables, arrays, objects, Maps, etc.).
 */

import { LiteralNode, TailAppNode } from './ast_nodes.js';
import { Cons } from './cons.js';
import { jsToScheme } from './js_interop.js';

// =============================================================================
// Marker Symbols for Type Identification
// =============================================================================

/**
 * Symbol used to mark callable functions as Scheme closures.
 * @type {symbol}
 */
export const SCHEME_CLOSURE = Symbol.for('scheme.closure');

/**
 * Symbol used to mark callable functions as Scheme continuations.
 * @type {symbol}
 */
export const SCHEME_CONTINUATION = Symbol.for('scheme.continuation');

/**
 * Symbol used to mark JavaScript functions as Scheme-aware primitives.
 * @type {symbol}
 */
export const SCHEME_PRIMITIVE = Symbol.for('scheme.primitive');

// =============================================================================
// Type Checking Functions
// =============================================================================

/**
 * Checks if a value is a Scheme closure (callable function with closure marker).
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isSchemeClosure(x) {
    return typeof x === 'function' && x[SCHEME_CLOSURE] === true;
}

/**
 * Checks if a value is a Scheme continuation (callable function with continuation marker).
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isSchemeContinuation(x) {
    return typeof x === 'function' && x[SCHEME_CONTINUATION] === true;
}

/**
 * Checks if a value is a Scheme-aware function (closure, continuation, or primitive).
 * Used by the interop layer to decide whether to auto-convert arguments.
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isSchemePrimitive(x) {
    return typeof x === 'function' && (
        x[SCHEME_PRIMITIVE] === true ||
        x[SCHEME_CLOSURE] === true ||
        x[SCHEME_CONTINUATION] === true
    );
}

// =============================================================================
// Factory Functions
// =============================================================================

/**
 * Creates a callable Scheme closure.
 * 
 * The returned function can be called directly from JavaScript and will
 * invoke the Scheme interpreter to execute the closure body.
 * 
 * @param {Array<string>} params - Parameter names.
 * @param {Executable} body - The body AST node.
 * @param {Environment} env - The captured lexical environment.
 * @param {string|null} restParam - Name of rest parameter, or null if none.
 * @param {Interpreter} interpreter - The interpreter instance.
 * @returns {Function} A callable function representing the Scheme closure.
 */
export function createClosure(params, body, env, restParam, interpreter) {
    // Create the callable wrapper
    const closure = function (...jsArgs) {
        // Build the invocation AST: apply this closure to the given args
        // Normalize args entering Scheme from JS
        const argLiterals = jsArgs.map(val => new LiteralNode(jsToScheme(val)));
        const ast = new TailAppNode(new LiteralNode(closure), argLiterals);

        // Run through the interpreter with a sentinel frame to capture result.
        // Unpacking will respect the default interop policy (deep conversion by default).
        return interpreter.runWithSentinel(ast, this);
    };

    // Attach marker and closure data
    closure[SCHEME_CLOSURE] = true;
    closure.params = params;
    closure.body = body;
    closure.env = env;
    closure.restParam = restParam;

    // Custom toString for pretty-printing
    closure.toString = () => '#<procedure>';

    return closure;
}

/**
 * Creates a callable Scheme continuation.
 * 
 * The returned function can be called directly from JavaScript and will
 * invoke the continuation, rewinding the Scheme stack appropriately.
 * 
 * @param {Array} fstack - The captured frame stack (will be copied).
 * @param {Interpreter} interpreter - The interpreter instance.
 * @returns {Function} A callable function representing the Scheme continuation.
 */
export function createContinuation(fstack, interpreter) {
    // Create the callable wrapper
    const continuation = function (...jsArgs) {
        // Handle multiple values: wrap 2+ args in Values
        let value;
        if (jsArgs.length === 0) {
            value = null;
        } else if (jsArgs.length === 1) {
            value = jsArgs[0];
        } else {
            value = new Values(jsArgs);
        }

        // Invoke the continuation through the interpreter
        return interpreter.invokeContinuation(continuation, value, this);
    };

    // Attach marker and continuation data
    continuation[SCHEME_CONTINUATION] = true;
    continuation.fstack = [...fstack];  // Store a copy

    // Custom toString for pretty-printing
    continuation.toString = () => '#<continuation>';

    return continuation;
}

// =============================================================================
// Legacy Classes (Kept for reference and internal data access)
// =============================================================================

/**
 * Base class for all procedures.
 * @deprecated Use isSchemeClosure/isSchemeContinuation for type checking
 */
export class Procedure { }

/**
 * Closure data class - kept for backward compatibility and documentation.
 * 
 * @deprecated Closures are now created via createClosure() and are callable functions.
 *             Use isSchemeClosure() to check if a value is a Scheme closure.
 */
export class Closure extends Procedure {
    /**
     * @param {Array<string>} params - Parameter names.
     * @param {Executable} body - The body AST node.
     * @param {Environment} env - The captured lexical environment.
     * @param {string|null} restParam - Name of rest parameter, or null if none.
     */
    constructor(params, body, env, restParam = null) {
        super();
        this.params = params;
        this.body = body;
        this.env = env;
        this.restParam = restParam;
    }

    toString() {
        return "#<procedure>";
    }
}

/**
 * Continuation data class - kept for backward compatibility and documentation.
 * 
 * @deprecated Continuations are now created via createContinuation() and are callable functions.
 *             Use isSchemeContinuation() to check if a value is a Scheme continuation.
 */
export class Continuation {
    /**
     * @param {Array} fstack - The captured frame stack (copied).
     */
    constructor(fstack) {
        this.fstack = [...fstack];
    }

    toString() {
        return "#<continuation>";
    }
}

// =============================================================================
// Other Value Types
// =============================================================================

/**
 * TailCall "Thunk" for trampoline control flow.
 * Returned by primitives to request the interpreter to perform a tail call.
 */
export class TailCall {
    /**
     * @param {*} func - The target (Closure, AST node, etc.)
     * @param {Array} args - Arguments for the call.
     */
    constructor(func, args) {
        this.func = func;
        this.args = args;
    }
}

/**
 * Error thrown to unwind the JavaScript stack when invoking a Continuation.
 * This allows jumping across JS boundaries (e.g. inside js-eval).
 */
export class ContinuationUnwind extends Error {
    /**
     * @param {Array} registers - The register state to restore.
     * @param {boolean} isReturn - True if this is a value return, false for tail call.
     */
    constructor(registers, isReturn = false) {
        super("Continuation Unwind");
        this.registers = registers;
        this.isReturn = isReturn;
    }
}

/**
 * Multiple Values wrapper.
 * Used by `values` and `call-with-values` to pass multiple return values.
 * 
 * In R7RS, (values 1 2 3) returns a "multiple values" object.
 * call-with-values unpacks it and applies to the consumer.
 */
export class Values {
    /**
     * @param {Array} values - The array of values being returned.
     */
    constructor(values) {
        this.values = values;
    }

    /**
     * Get the first value (used in single-value contexts and JS interop).
     * @returns {*} The first value, or undefined if empty.
     */
    first() {
        return this.values[0];
    }

    /**
     * Get all values as an array.
     * @returns {Array}
     */
    toArray() {
        return this.values;
    }

    toString() {
        return `#<values: ${this.values.length} values>`;
    }
}
