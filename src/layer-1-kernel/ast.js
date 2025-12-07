import { Closure, Continuation, TailCall, ContinuationUnwind } from './values.js';

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
 * Frame used to set up 'dynamic-wind' after 'before' runs.
 */
export class DynamicWindSetupFrame extends Executable {
    constructor(before, thunk, after, env) {
        super();
        this.before = before;
        this.thunk = thunk;
        this.after = after;
        this.env = env;
    }

    step(registers, interpreter) {
        // 'before' has completed. 'ans' is ignored/void.

        // 1. Push the WindFrame (active extent)
        registers[3].push(new WindFrame(
            this.before,
            this.after,
            this.env // Use captured environment
        ));

        // 2. Call the 'thunk'
        registers[1] = new TailApp(new Literal(this.thunk), []);
        // Maintain env
        registers[2] = this.env;
        return true;
    }
}

/**
 * AST Node to initialize 'dynamic-wind'.
 * Primitives cannot touch stack, so they return this node to do it.
 */
export class DynamicWindInit extends Executable {
    constructor(before, thunk, after) {
        super();
        this.before = before;
        this.thunk = thunk;
        this.after = after;
    }

    step(registers, interpreter) {
        // 1. Push the Setup frame (waits for 'before')
        registers[3].push(new DynamicWindSetupFrame(
            this.before,
            this.thunk,
            this.after,
            registers[2]
        ));

        // 2. Call 'before'
        registers[1] = new TailApp(new Literal(this.before), []);
        return true;
    }
}

/**
 * Frame for 'dynamic-wind' to restore the value after the 'after' thunk runs.
 */
export class RestoreValueFrame extends Executable {
    constructor(savedValue) {
        super();
        this.savedValue = savedValue;
    }

    step(registers, interpreter) {
        registers[0] = this.savedValue; // Restore saved value
        return false; // Halt (return value)
    }
}

/**
 * Frame for 'dynamic-wind' execution.
 * Represents an active dynamic extent.
 * If we return through this frame normally, we must run 'after'.
 */
export class WindFrame extends Executable {
    constructor(before, after, env) {
        super();
        this.before = before; // Thunk
        this.after = after;   // Thunk
        this.env = env;
    }

    step(registers, interpreter) {
        // We are exiting the extent normally. 'ans' holds the body's result.
        const result = registers[0];

        // 1. Push a frame to restore the result *after* the 'after' thunk runs
        registers[3].push(new RestoreValueFrame(result));

        // 2. Call the 'after' thunk
        registers[1] = new TailApp(new Literal(this.after), []);
        // Maintain current env for the call logic, though TailApp will Switch it for the Closure
        // registers[2] is fine.
        return true;
    }
}

/**
 * Special AST node to restore a continuation's stack and value.
 * Used as the final step in a dynamic-wind sequence.
 */
export class RestoreContinuation extends Executable {
    constructor(targetStack, value) {
        super();
        this.targetStack = targetStack;
        this.value = value;
    }

    step(registers, interpreter) {
        registers[3] = [...this.targetStack]; // Restore target stack
        registers[0] = this.value;            // Restore value
        return false; // Halt (trampoline will see false and check stack, which we just filled)
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
            // --- APPLICATION ---
            const func = newArgValues[0];
            const args = newArgValues.slice(1);

            // 1. CLOSURE APPLICATION
            if (func instanceof Closure) {
                registers[1] = func.body;
                registers[2] = func.env.extendMany(func.params, args);
                return true; // Tail call
            }

            if (typeof func === 'function') {
                // DIRECT JS INTEROP
                // We assume 'func' is a raw JS function (like Math.max or console.log)
                // We pass the raw values (numbers, strings, arrays) directly.
                // BUT if an argument is a Closure or Continuation, we must wrap it in a Bridge!
                const jsArgs = args.map(arg => {
                    // Check if the function explicitly requests raw Closures (e.g. apply)
                    if ((arg instanceof Closure || arg instanceof Continuation) && !func.skipBridge) {
                        return interpreter.createJsBridge(arg, registers[3]);
                    }
                    return arg;
                });

                const result = func(...jsArgs);
                if (result instanceof TailCall) {
                    const target = result.func;
                    // If target is a Closure, Continuation, or JS Function, we treat it as an Application (TailApp)
                    if (target instanceof Closure || target instanceof Continuation || typeof target === 'function') {
                        const args = result.args || [];
                        const argLiterals = args.map(a => new Literal(a));
                        registers[1] = new TailApp(new Literal(target), argLiterals);
                        return true;
                    }
                    // Otherwise, we assume it is an Executable AST node (e.g. DynamicWindInit)
                    // We jump to it directly. Args must be null/empty in this case.
                    registers[1] = target;
                    return true;
                }
                registers[0] = result;
                return false; // Halt (value return)
            }

            if (func instanceof Continuation) {
                // --- INVOKE CONTINUATION WITH DYNAMIC-WIND ---
                const currentStack = registers[3];
                const targetStack = func.fstack;
                const value = args[0] || null;

                // 1. Find common ancestor
                // Stacks are arrays of frames. We scan from the bottom (index 0) up.
                // The stacks are different instances, but frames *should* be shared by reference
                // if they are common.
                let i = 0;
                while (i < currentStack.length && i < targetStack.length && currentStack[i] === targetStack[i]) {
                    i++;
                }
                const ancestorIndex = i;

                // 2. Identify WindFrames to unwind (from top of current down to ancestor)
                const toUnwind = currentStack.slice(ancestorIndex).reverse().filter(f => f instanceof WindFrame);

                // 3. Identify WindFrames to rewind (from ancestor up to top of target)
                const toRewind = targetStack.slice(ancestorIndex).filter(f => f instanceof WindFrame);

                // 4. Construct sequence of operations
                const actions = [];

                for (const frame of toUnwind) {
                    actions.push(new TailApp(new Literal(frame.after), []));
                }
                for (const frame of toRewind) {
                    actions.push(new TailApp(new Literal(frame.before), []));
                }

                // CRITICAL: Unwind JS stack (Return Value Mode)
                if (actions.length === 0) {
                    registers[3] = [...targetStack];
                    registers[0] = value;

                    if (interpreter.depth > 1) {
                        throw new ContinuationUnwind(registers, true);
                    }
                    return false;
                }

                // Append the final restoration
                actions.push(new RestoreContinuation(targetStack, value));

                // 5. Execute via BeginFrame mechanism
                const firstAction = actions[0];
                const remainingActions = actions.slice(1);

                if (remainingActions.length > 0) {
                    registers[3].push(new BeginFrame(remainingActions, this.env));
                }

                registers[1] = firstAction;

                // CRITICAL: Unwind JS stack (Tail Call Mode)
                if (interpreter.depth > 1) {
                    throw new ContinuationUnwind(registers, false);
                }

                return true;
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
