/**
 * Continuation Frame Classes for the Scheme interpreter.
 * 
 * Frames represent "the rest of the computation" - they are pushed onto
 * the fstack when an AST node needs to evaluate subexpressions before
 * completing its work.
 */

import { Closure, Continuation, TailCall, ContinuationUnwind } from './values.js';
import { registerFrames, createBeginFrame, createWindFrame, createRestoreValueFrame, getWindFrameClass } from './frame_registry.js';
import { Literal, TailApp, RestoreContinuation } from './nodes.js';

// --- Base Class ---

/**
 * Base class for all executable objects (AST nodes and Frames).
 * All must have a `step` method.
 */
export class Executable {
    step(registers, interpreter) {
        throw new Error("step() must be implemented in subclasses");
    }
}

// --- Let/Letrec Frames ---

/**
 * Frame for a 'let' binding.
 * Waits for the binding value, then extends the environment and evaluates body.
 */
export class LetFrame extends Executable {
    /**
     * @param {string} varName - The variable name to bind.
     * @param {Executable} body - The body expression.
     * @param {Environment} env - The captured environment.
     */
    constructor(varName, body, env) {
        super();
        this.varName = varName;
        this.body = body;
        this.env = env;
    }

    step(registers, interpreter) {
        const bindingResult = registers[0];
        const newEnv = this.env.extend(this.varName, bindingResult);

        registers[1] = this.body;
        registers[2] = newEnv;
        return true;
    }
}

/**
 * Frame for a 'letrec' binding.
 * Waits for the lambda value, updates the placeholder, then evaluates body.
 */
export class LetRecFrame extends Executable {
    /**
     * @param {string} varName - The variable name to bind.
     * @param {Executable} body - The body expression.
     * @param {Environment} env - The captured environment (with placeholder).
     */
    constructor(varName, body, env) {
        super();
        this.varName = varName;
        this.body = body;
        this.env = env;
    }

    step(registers, interpreter) {
        const closure = registers[0];
        this.env.bindings.set(this.varName, closure);

        registers[1] = this.body;
        registers[2] = this.env;
        return true;
    }
}

// --- Control Flow Frames ---

/**
 * Frame for an 'if' expression.
 * Waits for the test result, then evaluates appropriate branch.
 */
export class IfFrame extends Executable {
    /**
     * @param {Executable} consequent - The 'then' branch.
     * @param {Executable} alternative - The 'else' branch.
     * @param {Environment} env - The captured environment.
     */
    constructor(consequent, alternative, env) {
        super();
        this.consequent = consequent;
        this.alternative = alternative;
        this.env = env;
    }

    step(registers, interpreter) {
        const testResult = registers[0];

        if (testResult !== false) {
            registers[1] = this.consequent;
        } else {
            registers[1] = this.alternative;
        }
        registers[2] = this.env;
        return true;
    }
}

/**
 * Frame for a 'set!' expression.
 * Waits for the value, then updates the binding.
 */
export class SetFrame extends Executable {
    /**
     * @param {string} name - The variable name to set.
     * @param {Environment} env - The captured environment.
     */
    constructor(name, env) {
        super();
        this.name = name;
        this.env = env;
    }

    step(registers, interpreter) {
        const value = registers[0];
        this.env.set(this.name, value);
        registers[0] = value;
        return false;
    }
}

/**
 * Frame for a 'define' expression.
 * Waits for the value, then creates a binding in the current scope.
 */
export class DefineFrame extends Executable {
    /**
     * @param {string} name - The variable name to define.
     * @param {Environment} env - The captured environment.
     */
    constructor(name, env) {
        super();
        this.name = name;
        this.env = env;
    }

    step(registers, interpreter) {
        const value = registers[0];
        this.env.define(this.name, value);
        registers[0] = this.name;
        return false;
    }
}

/**
 * Frame for a 'begin' expression.
 * Evaluates remaining expressions in sequence, returns the last.
 */
export class BeginFrame extends Executable {
    /**
     * @param {Array<Executable>} remainingExprs - Expressions left to evaluate.
     * @param {Environment} env - The captured environment.
     */
    constructor(remainingExprs, env) {
        super();
        this.remainingExprs = remainingExprs;
        this.env = env;
    }

    step(registers, interpreter) {
        if (this.remainingExprs.length === 0) {
            return false;
        }

        const nextExpr = this.remainingExprs[0];
        const rest = this.remainingExprs.slice(1);

        registers[3].push(new BeginFrame(rest, this.env));

        registers[1] = nextExpr;
        registers[2] = this.env;
        return true;
    }
}

// --- Application Frame ---

/**
 * Frame for a function application.
 * Accumulates evaluated arguments, then performs the application.
 */
export class AppFrame extends Executable {
    /**
     * @param {Array<Executable>} argExprs - Remaining argument expressions.
     * @param {Array<*>} argValues - Already-evaluated argument values.
     * @param {Environment} env - The captured environment.
     */
    constructor(argExprs, argValues, env) {
        super();
        this.argExprs = argExprs;
        this.argValues = argValues;
        this.env = env;
    }

    step(registers, interpreter) {
        const value = registers[0];
        const newArgValues = [...this.argValues, value];

        if (this.argExprs.length > 0) {
            const nextArgExpr = this.argExprs[0];
            const remainingArgExprs = this.argExprs.slice(1);

            registers[3].push(new AppFrame(
                remainingArgExprs,
                newArgValues,
                this.env
            ));

            registers[1] = nextArgExpr;
            registers[2] = this.env;
            return true;
        }

        // All arguments evaluated, ready to apply
        const func = newArgValues[0];
        const args = newArgValues.slice(1);

        // 1. CLOSURE APPLICATION
        if (func instanceof Closure) {
            registers[1] = func.body;
            registers[2] = func.env.extendMany(func.params, args);
            return true;
        }

        // 2. JS FUNCTION APPLICATION
        if (typeof func === 'function') {
            const jsArgs = args.map(arg => {
                if ((arg instanceof Closure || arg instanceof Continuation) && !func.skipBridge) {
                    return interpreter.createJsBridge(arg, registers[3]);
                }
                return arg;
            });

            const result = func(...jsArgs);

            if (result instanceof TailCall) {
                const target = result.func;
                if (target instanceof Closure || target instanceof Continuation || typeof target === 'function') {
                    const tailArgs = result.args || [];
                    const argLiterals = tailArgs.map(a => new Literal(a));
                    registers[1] = new TailApp(new Literal(target), argLiterals);
                    return true;
                }
                registers[1] = target;
                return true;
            }

            registers[0] = result;
            return false;
        }

        // 3. CONTINUATION INVOCATION
        if (func instanceof Continuation) {
            return this.invokeContinuation(func, args, registers, interpreter);
        }

        throw new Error(`Not a function: ${func}`);
    }

    /**
     * Invokes a captured continuation with proper dynamic-wind handling.
     * @private
     */
    invokeContinuation(func, args, registers, interpreter) {
        const currentStack = registers[3];
        const targetStack = func.fstack;

        // Handle multiple values: wrap 2+ args in Values, like `values` primitive
        let value;
        if (args.length === 0) {
            value = null;
        } else if (args.length === 1) {
            value = args[0];
        } else {
            value = new Values(args);
        }


        // Get WindFrame class for instanceof check
        const WindFrameClass = getWindFrameClass();

        // 1. Find common ancestor
        let i = 0;
        while (i < currentStack.length && i < targetStack.length && currentStack[i] === targetStack[i]) {
            i++;
        }
        const ancestorIndex = i;

        // 2. Identify WindFrames to unwind
        const toUnwind = currentStack.slice(ancestorIndex).reverse().filter(f => f instanceof WindFrameClass);

        // 3. Identify WindFrames to rewind
        const toRewind = targetStack.slice(ancestorIndex).filter(f => f instanceof WindFrameClass);

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
            // Filter out SentinelFrames from the target stack.
            // SentinelFrames are JS boundary markers that should not be executed
            // when restoring a continuation - they would prematurely terminate
            // the computation before parent context frames are executed.
            const filteredStack = targetStack.filter(f => f.constructor.name !== 'SentinelFrame');
            registers[3] = [...filteredStack];
            registers[0] = value;

            if (interpreter.depth > 1) {
                throw new ContinuationUnwind(registers, true);
            }
            return false;
        }

        // Append the final restoration (with SentinelFrames filtered out)
        const filteredStack = targetStack.filter(f => f.constructor.name !== 'SentinelFrame');
        actions.push(new RestoreContinuation(filteredStack, value));

        // Execute via BeginFrame mechanism
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
}

// --- Dynamic-Wind Frames ---

/**
 * Frame used to set up 'dynamic-wind' after 'before' thunk runs.
 */
export class DynamicWindSetupFrame extends Executable {
    /**
     * @param {Closure} before - The 'before' thunk.
     * @param {Closure} thunk - The main thunk.
     * @param {Closure} after - The 'after' thunk.
     * @param {Environment} env - The captured environment.
     */
    constructor(before, thunk, after, env) {
        super();
        this.before = before;
        this.thunk = thunk;
        this.after = after;
        this.env = env;
    }

    step(registers, interpreter) {
        // 'before' has completed. 'ans' is ignored.
        registers[3].push(new WindFrame(
            this.before,
            this.after,
            this.env
        ));

        registers[1] = new TailApp(new Literal(this.thunk), []);
        registers[2] = this.env;
        return true;
    }
}

/**
 * Frame for 'dynamic-wind' execution.
 * Represents an active dynamic extent.
 * When we return through this frame normally, we run 'after'.
 */
export class WindFrame extends Executable {
    /**
     * @param {Closure} before - The 'before' thunk.
     * @param {Closure} after - The 'after' thunk.
     * @param {Environment} env - The captured environment.
     */
    constructor(before, after, env) {
        super();
        this.before = before;
        this.after = after;
        this.env = env;
    }

    step(registers, interpreter) {
        const result = registers[0];

        registers[3].push(new RestoreValueFrame(result));

        registers[1] = new TailApp(new Literal(this.after), []);
        return true;
    }
}

/**
 * Frame for 'dynamic-wind' to restore a value after the 'after' thunk runs.
 */
export class RestoreValueFrame extends Executable {
    /**
     * @param {*} savedValue - The value to restore to 'ans'.
     */
    constructor(savedValue) {
        super();
        this.savedValue = savedValue;
    }

    step(registers, interpreter) {
        registers[0] = this.savedValue;
        return false;
    }
}

// --- Multiple Values Frames ---

import { Values } from './values.js';

/**
 * Frame for call-with-values.
 * Waits for producer result, unpacks Values if present, then applies consumer.
 */
export class CallWithValuesFrame extends Executable {
    /**
     * @param {Closure} consumer - The consumer procedure
     * @param {Environment} env - The captured environment
     */
    constructor(consumer, env) {
        super();
        this.consumer = consumer;
        this.env = env;
    }

    step(registers, interpreter) {
        const result = registers[0];

        // Unpack Values object, or treat single value as 1-element array
        let args;
        if (result instanceof Values) {
            args = result.toArray();
        } else {
            args = [result];
        }

        // Create argument Literals for TailApp
        const argLiterals = args.map(a => new Literal(a));

        // Invoke consumer with the values
        registers[1] = new TailApp(new Literal(this.consumer), argLiterals);
        registers[2] = this.env;
        return true;
    }
}

// --- Register all frames with the registry ---
registerFrames({
    LetFrame,
    LetRecFrame,
    IfFrame,
    SetFrame,
    DefineFrame,
    AppFrame,
    BeginFrame,
    DynamicWindSetupFrame,
    WindFrame,
    RestoreValueFrame,
    CallWithValuesFrame
});

