import { Closure, NativeJsFunction, Continuation } from '../data/values.js';

// --- PART 1: Base Classes ---

/**
 * Base class for all executable objects (AST nodes and Frames).
 * All must have a `step` method.
 */
export class Executable {
    step(registers, interpreter) {
        throw new Error("step() must be implemented in subclasses");
    }
}

// --- PART 3: AST Node Classes (Atomic) ---

/**
 * A literal value (number, string, boolean).
 * This is an atomic expression.
 */
export class Literal extends Executable {
    constructor(value) {
        super();
        this.value = value;
    }

    step(registers, interpreter) {
        registers[0] = this.value; // Put value in 'ans'
        return false; // Halt (value return)
    }

    toString() { return `(Literal ${this.value})`; }
}

/**
 * A variable lookup.
 * This is an atomic expression.
 */
export class Variable extends Executable {
    constructor(name) {
        super();
        this.name = name;
    }

    step(registers, interpreter) {
        const env = registers[2];
        registers[0] = env.lookup(this.name); // Look up value, put in 'ans'
        return false; // Halt (value return)
    }

    toString() { return `(Variable ${this.name})`; }
}

/**
 * A lambda expression.
 * This is created by the analyzer, not the reader.
 */
export class Lambda extends Executable {
    constructor(params, body) {
        super();
        this.params = params; // array of param names
        this.body = body;
    }

    step(registers, interpreter) {
        // Create a closure, capturing the current environment
        registers[0] = new Closure(this.params, this.body, registers[2]);
        return false; // Halt (value return)
    }

    toString() { return `(Lambda (${this.params.join(' ')}) ...`; }
}

// --- PART 4: Continuation Frame Classes ---
// These are also "Executable"

/**
 * Frame for a 'let' binding.
 * Pauses execution, evaluates the binding, then restores and
 * evaluates the body in an extended environment.
 */
export class LetFrame extends Executable {
    constructor(varName, body, env) {
        super();
        this.varName = varName;
        this.body = body;
        this.env = env;
    }

    step(registers, interpreter) {
        // The binding has been evaluated, its value is in 'ans'.
        const bindingResult = registers[0];
        const newEnv = this.env.extend(this.varName, bindingResult);

        registers[1] = this.body; // Set 'ctl' to the body
        registers[2] = newEnv;    // Set 'env' to the new environment
        return true; // Continue (tail call into body)
    }
}

/**
 * Frame for a 'letrec' binding.
 */
export class LetRecFrame extends Executable {
    constructor(varName, body, env) {
        super();
        this.varName = varName;
        this.body = body;
        this.env = env;
    }

    step(registers, interpreter) {
        // The lambda has been evaluated, its value (Closure) is in 'ans'.
        const closure = registers[0];
        // The environment *we* were created with was the one
        // with the placeholder. We now update it.
        this.env.bindings.set(this.varName, closure);

        registers[1] = this.body; // Set 'ctl' to the body
        registers[2] = this.env;    // Set 'env' to the updated environment
        return true; // Continue (tail call into body)
    }
}

/**
 * Frame for an 'if' expression.
 * Pauses execution, evaluates the test, then restores and
 * evaluates one of the branches.
 */
export class IfFrame extends Executable {
    constructor(consequent, alternative, env) {
        super();
        this.consequent = consequent;
        this.alternative = alternative;
        this.env = env;
    }

    step(registers, interpreter) {
        // The test has been evaluated, its value is in 'ans'.
        const testResult = registers[0];

        if (testResult !== false) {
            registers[1] = this.consequent;
        } else {
            registers[1] = this.alternative;
        }
        registers[2] = this.env; // Restore original environment
        return true; // Continue (tail call into branch)
    }
}

/**
 * Frame for a 'set!' expression.
 */
export class SetFrame extends Executable {
    constructor(name, env) {
        super();
        this.name = name;
        this.env = env;
    }

    step(registers, interpreter) {
        // The valueExpr has been evaluated, its value is in 'ans'.
        const value = registers[0];
        this.env.set(this.name, value);
        registers[0] = value; // 'set!' returns the value
        return false; // Halt
    }
}

/**
 * Frame for a 'define' expression.
 */
export class DefineFrame extends Executable {
    constructor(name, env) {
        super();
        this.name = name;
        this.env = env;
    }

    step(registers, interpreter) {
        // The valueExpr has been evaluated, its value is in 'ans'.
        const value = registers[0];
        this.env.define(this.name, value);
        registers[0] = this.name; // 'define' returns the symbol name (or void)
        return false; // Halt
    }
}

/**
 * Frame for a function application.
 * Pauses execution, evaluates the function, then restores
 * and continues to evaluate arguments.
 */
export class AppFrame extends Executable {
    constructor(argExprs, argValues, env) {
        super();
        this.argExprs = argExprs;   // List of remaining arg *expressions*
        this.argValues = argValues; // List of *values* already computed
        this.env = env;
    }

    step(registers, interpreter) {
        // A value is in 'ans'. It's either the function, or an argument.
        const value = registers[0];
        const newArgValues = [...this.argValues, value];

        if (this.argExprs.length > 0) {
            // --- More arguments to evaluate ---
            const nextArgExpr = this.argExprs[0];
            const remainingArgExprs = this.argExprs.slice(1);

            // Push a *new* frame for the *rest* of the arguments
            registers[3].push(new AppFrame(
                remainingArgExprs,
                newArgValues,
                this.env
            ));

            registers[1] = nextArgExpr; // Set 'ctl' to evaluate the next arg
            registers[2] = this.env;    // Restore env for arg evaluation
            return true; // Continue
        } else {
            // --- All arguments evaluated, ready to apply ---
            // 'newArgValues[0]' is the function
            // 'newArgValues.slice(1)' are the arguments
            const func = newArgValues[0];
            const args = newArgValues.slice(1);

            if (func instanceof Closure) {
                registers[1] = func.body;
                registers[2] = func.env.extendMany(func.params, args);
                return true; // Tail call
            }

            if (func instanceof NativeJsFunction) {
                registers[0] = func.call(args);
                return false; // Halt (value return)
            }

            if (func instanceof Continuation) {
                // --- INVOKE CONTINUATION ---
                // Replace the fstack, set 'ans', and halt
                registers[3] = [...func.fstack]; // Restore stack
                registers[0] = args[0] || null;  // Set 'ans' to the value being passed
                return false; // Halt
            }

            throw new Error(`Not a function: ${func}`);
        }
    }
}

/**
 * Frame for a 'begin' expression.
 * Evaluates a sequence of expressions, returning the last.
 */
export class BeginFrame extends Executable {
    constructor(remainingExprs, env) {
        super();
        this.remainingExprs = remainingExprs;
        this.env = env;
    }

    step(registers, interpreter) {
        // A value is in 'ans' from the previous expression.

        if (this.remainingExprs.length === 0) {
            // This was the last expression. The result is already
            // in 'ans', so we just halt.
            return false; // Halt
        }

        // --- More expressions to evaluate ---
        const nextExpr = this.remainingExprs[0];
        const rest = this.remainingExprs.slice(1);

        // Push a new BeginFrame for the *rest* of the expressions
        registers[3].push(new BeginFrame(rest, this.env));

        registers[1] = nextExpr; // Set 'ctl' to evaluate the next expr
        registers[2] = this.env; // Restore env
        return true; // Continue
    }
}


// --- PART 5: AST Node Classes (Complex) ---

/**
 * A 'let' binding.
 */
export class Let extends Executable {
    constructor(varName, binding, body) {
        super();
        this.varName = varName;
        this.binding = binding;
        this.body = body;
    }

    step(registers, interpreter) {
        // 1. Push the "rest of the work" (the LetFrame)
        registers[3].push(new LetFrame(
            this.varName,
            this.body,
            registers[2] // Capture current env
        ));

        // 2. Set 'ctl' to the work we need done *now*
        registers[1] = this.binding;
        // 'env' is unchanged
        return true; // Continue to evaluate the binding
    }
}

/**
 * A 'letrec' binding for recursive functions.
 */
export class LetRec extends Executable {
    constructor(varName, lambdaExpr, body) {
        super();
        this.varName = varName;
        this.lambdaExpr = lambdaExpr; // Must be a Lambda
        this.body = body;
    }
    step(registers, interpreter) {
        // 1. Create a new environment with a placeholder
        const newEnv = registers[2].extend(this.varName, null);

        // 2. Push the "rest of the work"
        registers[3].push(new LetRecFrame(
            this.varName,
            this.body,
            newEnv // Pass the *new* env to the frame
        ));

        // 3. Set 'ctl' to evaluate the lambda
        registers[1] = this.lambdaExpr;
        // 4. Set 'env' to the *new* env for the lambda
        registers[2] = newEnv;
        return true; // Continue
    }
}

/**
 * An 'if' expression.
 */
export class If extends Executable {
    constructor(test, consequent, alternative) {
        super();
        this.test = test;
        this.consequent = consequent;
        this.alternative = alternative;
    }

    step(registers, interpreter) {
        // 1. Push the "rest of the work"
        registers[3].push(new IfFrame(
            this.consequent,
            this.alternative,
            registers[2] // Capture current env
        ));

        // 2. Set 'ctl' to the test expression
        registers[1] = this.test;
        // 'env' is unchanged
        return true; // Continue to evaluate the test
    }
}

/**
 * A variable definition or update (set!).
 */
export class Set extends Executable {
    constructor(name, valueExpr) {
        super();
        this.name = name;
        this.valueExpr = valueExpr;
    }

    step(registers, interpreter) {
        // 1. Push the "rest of the work"
        registers[3].push(new SetFrame(
            this.name,
            registers[2] // Capture current env
        ));

        // 2. Set 'ctl' to the value expression
        registers[1] = this.valueExpr;
        // 'env' is unchanged
        return true; // Continue
    }
}

/**
 * A variable definition (define).
 */
export class Define extends Executable {
    constructor(name, valueExpr) {
        super();
        this.name = name;
        this.valueExpr = valueExpr;
    }

    step(registers, interpreter) {
        // 1. Push the "rest of the work"
        registers[3].push(new DefineFrame(
            this.name,
            registers[2] // Capture current env
        ));

        // 2. Set 'ctl' to the value expression
        registers[1] = this.valueExpr;
        // 'env' is unchanged
        return true; // Continue
    }
}

/**
 * A tail-call application.
 */
export class TailApp extends Executable {
    constructor(funcExpr, argExprs) {
        super();
        this.funcExpr = funcExpr;
        this.argExprs = argExprs;
    }

    step(registers, interpreter) {
        // This is the *only* complex node that *doesn't*
        // push a frame, because it's in tail position.
        // Instead, it *sets up* the AppFrame.

        const allExprs = [this.funcExpr, ...this.argExprs];
        const firstExpr = allExprs[0];
        const remainingExprs = allExprs.slice(1);

        // 1. Push the AppFrame that will do all the work
        registers[3].push(new AppFrame(
            remainingExprs,
            [], // No values computed yet
            registers[2] // Capture current env
        ));

        // 2. Set 'ctl' to the first thing to evaluate
        registers[1] = firstExpr;
        // 'env' is unchanged
        return true; // Continue
    }
}

/**
 * call-with-current-continuation.
 */
export class CallCC extends Executable {
    constructor(lambdaExpr) {
        super();
        this.lambdaExpr = lambdaExpr; // This should be a (lambda (k) ...)
    }

    step(registers, interpreter) {
        // 1. Create the Continuation object by *copying* the fstack
        const continuation = new Continuation(registers[3]);

        // 2. Create the function call (lambda-expr continuation)
        // This is a tail call, so we just set 'ctl'
        registers[1] = new TailApp(
            this.lambdaExpr,
            [new Literal(continuation)]
        );
        // 'env' is unchanged
        return true; // Continue
    }
}

/**
 * A 'begin' expression.
 */
export class Begin extends Executable {
    constructor(expressions) {
        super();
        this.expressions = expressions;
    }

    step(registers, interpreter) {
        if (this.expressions.length === 0) {
            registers[0] = null; // (begin) returns null/undefined
            return false; // Halt
        }

        const firstExpr = this.expressions[0];
        const remainingExprs = this.expressions.slice(1);

        // 1. Push a BeginFrame for the *rest* of the expressions
        if (remainingExprs.length > 0) {
            registers[3].push(new BeginFrame(
                remainingExprs,
                registers[2] // Capture current env
            ));
        }

        // 2. Set 'ctl' to the first expression
        registers[1] = firstExpr;
        // 'env' is unchanged
        return true; // Continue
    }
}
