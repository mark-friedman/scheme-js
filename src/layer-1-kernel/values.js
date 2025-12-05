

/**
 * Base class for all runtime values.
 * These do *not* have a `step` method.
 */
export class SchemeValue { }

// --- PART 2: Runtime Value Classes ---

/**
 * A runtime closure (lambda + captured environment).
 */
export class Closure extends SchemeValue {
    constructor(params, body, env) {
        super();
        this.params = params;
        this.body = body;
        this.env = env;
    }
    toString() { return `[Closure]`; }
}



/**
 * A reified first-class continuation.
 */
export class Continuation extends SchemeValue {
    constructor(fstack) {
        super();
        // We must store a *copy* of the fstack.
        this.fstack = [...fstack];
    }
    toString() { return `[Continuation]`; }
}

/**
 * A wrapper for a tail call returned by a primitive.
 * This signals the interpreter to replace the current control and environment.
 */
export class TailCall extends SchemeValue {
    constructor(ast, env) {
        super();
        this.ast = ast;
        this.env = env;
    }
    toString() { return `[TailCall]`; }
}
