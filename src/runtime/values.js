
export class Value {
    constructor() { }
}

/**
 * Closures capture the environment at definition time.
 */
export class Closure extends Value {
    constructor(params, body, env) {
        super();
        this.params = params; // Array of parameter names (strings)
        this.body = body;     // AST node (e.g. Sequence or single expression)
        this.env = env;       // Captured Environment
    }

    toString() {
        return "#<procedure>";
        // return `#<closure params=${this.params} body=${this.body} env=${this.env}>`;
    }
}

/**
 * First-class continuations.
 * Captures the frame stack.
 */
export class Continuation extends Value {
    constructor(fstack) {
        super();
        // We must store a *copy* of the fstack.
        this.fstack = [...fstack];
    }

    toString() {
        return "#<continuation>";
    }
}

/**
 * TailCall "Thunk" (Trampoline return value).
 * Returned by primitives to request specific control flow changes.
 */
export class TailCall {
    constructor(func, args) {
        this.func = func; // Closure
        this.args = args; // Array of arguments
    }
}

/**
 * Error thrown to unwind the JavaScript stack when invoking a Continuation.
 * This allows jumping across JS boundaries (e.g. inside js-eval).
 */
export class ContinuationUnwind extends Error {
    constructor(registers, isReturn = false) {
        super("Continuation Unwind");
        this.registers = registers;
        this.isReturn = isReturn;
    }
}
