import { Literal, TailApp } from '../syntax/ast.js';

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
 * A runtime native JS function wrapper.
 */
export class NativeJsFunction extends SchemeValue {
    constructor(jsFunc, interpreter) {
        super();
        this.jsFunc = jsFunc;
        this.interpreter = interpreter;
    }

    call(args) {
        const wrappedArgs = args.map(arg => {
            if (arg instanceof Closure) {
                // This is the JS -> Scheme bridge
                // Return a new JS function that, when called,
                // schedules a new interpreter 'run'.
                return (...callbackArgs) => {
                    const schemeArgs = callbackArgs.map(a => new Literal(a));
                    const ctl = new TailApp(new Literal(arg), schemeArgs);
                    // Run this as a new, independent computation
                    // This correctly starts a new stack.
                    return this.interpreter.run(ctl, this.interpreter.globalEnv);
                };
            }
            if (arg instanceof SchemeValue) {
                // We can't pass all scheme values to JS, but
                // we'll pass continuations for testing.
                return arg;
            }
            // assume primitive
            return arg;
        });
        return this.jsFunc(...wrappedArgs);
    }

    toString() { return `[NativeFunction]`; }
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
