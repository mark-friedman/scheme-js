/**
 * Scheme Runtime Value Types
 * 
 * These classes represent first-class values in the Scheme runtime that
 * are not primitive JavaScript values. They are used for closures,
 * continuations, and multiple return values.
 */

/**
 * Closures capture the environment at definition time.
 * Created by Lambda nodes when evaluated.
 */
export class Closure {
    /**
     * @param {Array<string>} params - Parameter names.
     * @param {Executable} body - The body AST node.
     * @param {Environment} env - The captured lexical environment.
     * @param {string|null} restParam - Name of rest parameter, or null if none.
     */
    constructor(params, body, env, restParam = null) {
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
 * First-class continuations.
 * Captures the frame stack at the point of call/cc invocation.
 */
export class Continuation {
    /**
     * @param {Array} fstack - The captured frame stack (copied).
     */
    constructor(fstack) {
        // We must store a *copy* of the fstack.
        this.fstack = [...fstack];
    }

    toString() {
        return "#<continuation>";
    }
}

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
