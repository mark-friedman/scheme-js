/**
 * Frame Classes for the Scheme interpreter.
 * 
 * This module contains all Frame classes. Frames represent "the rest of the
 * computation" and are pushed onto the frame stack (fstack). They are popped
 * and executed after AST nodes complete their immediate work.
 * 
 * All frames extend Executable and implement a step() method.
 */

import { Executable, ANS, CTL, ENV, FSTACK, THIS } from './stepables_base.js';
import { isSchemeClosure, isSchemeContinuation, TailCall, ContinuationUnwind, Values } from './values.js';
import { registerFrames, getWindFrameClass } from './frame_registry.js';
import { Cons } from './cons.js';
import { registerBindingWithCurrentScopes } from './syntax_object.js';
import { SchemeApplicationError } from './errors.js';

// Import AST nodes needed by frames (Literal, TailApp, RestoreContinuation)
// Note: This creates a dependency on ast_nodes, but it's a one-way dependency
import { LiteralNode, TailAppNode, RestoreContinuation, RaiseNode } from './ast_nodes.js';

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Filters out SentinelFrames from a stack.
 * SentinelFrames are JS boundary markers that should not be executed
 * when restoring a continuation.
 * @param {Array} stack - The stack to filter
 * @returns {Array} Stack with SentinelFrames removed
 */
function filterSentinelFrames(stack) {
    return stack.filter(f => f.constructor.name !== 'SentinelFrame');
}

// =============================================================================
// Frames - Let/Letrec
// =============================================================================

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
        const bindingResult = registers[ANS];
        const newEnv = this.env.extend(this.varName, bindingResult);

        registers[CTL] = this.body;
        registers[ENV] = newEnv;
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
        const closure = registers[ANS];
        this.env.bindings.set(this.varName, closure);

        registers[CTL] = this.body;
        registers[ENV] = this.env;
        return true;
    }
}

// =============================================================================
// Frames - Control Flow
// =============================================================================

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
        const testResult = registers[ANS];

        if (testResult !== false) {
            registers[CTL] = this.consequent;
        } else {
            registers[CTL] = this.alternative;
        }
        registers[ENV] = this.env;
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
        const value = registers[ANS];
        this.env.set(this.name, value);
        registers[ANS] = undefined;
        return false;
    }
}

/**
 * Frame for a 'define' expression.
 * Waits for the value, then creates a binding in the current scope.
 * Also registers the binding with any active defining scopes (for macro hygiene).
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
        const value = registers[ANS];
        this.env.define(this.name, value);

        // Register binding with current defining scopes for macro referential transparency
        registerBindingWithCurrentScopes(this.name, value);

        registers[ANS] = undefined;
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

        registers[FSTACK].push(new BeginFrame(rest, this.env));

        registers[CTL] = nextExpr;
        registers[ENV] = this.env;
        return true;
    }
}

// =============================================================================
// Frames - Application
// =============================================================================

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
        const value = registers[ANS];
        const newArgValues = [...this.argValues, value];

        if (this.argExprs.length > 0) {
            const nextArgExpr = this.argExprs[0];
            const remainingArgExprs = this.argExprs.slice(1);

            registers[FSTACK].push(new AppFrame(
                remainingArgExprs,
                newArgValues,
                this.env
            ));

            registers[CTL] = nextArgExpr;
            registers[ENV] = this.env;
            return true;
        }

        // All arguments evaluated, ready to apply
        const func = newArgValues[0];
        const args = newArgValues.slice(1);

        // 1. SCHEME CLOSURE APPLICATION
        // Check for callable Scheme closures first (they are typeof 'function')
        if (isSchemeClosure(func)) {
            registers[CTL] = func.body;

            // Handle rest parameter if present
            if (func.restParam) {
                // Required params get their args, rest param gets remaining as list
                const requiredCount = func.params.length;
                const requiredArgs = args.slice(0, requiredCount);
                const restArgs = args.slice(requiredCount);

                // Build a Scheme list from rest args
                let restList = null;
                for (let i = restArgs.length - 1; i >= 0; i--) {
                    restList = new Cons(restArgs[i], restList);
                }

                // Extend environment with required params + rest param
                const allParams = [...func.params, func.restParam];
                const allArgs = [...requiredArgs, restList];
                let newEnv = func.env.extendMany(allParams, allArgs);
                // Bind 'this' pseudo-variable if available (method call)
                if (registers[THIS] !== undefined) {
                    registers[ENV] = newEnv.extend('this', registers[THIS]);
                } else {
                    registers[ENV] = newEnv;
                }
            } else {
                let newEnv = func.env.extendMany(func.params, args);
                // Bind 'this' pseudo-variable if available (method call)
                if (registers[THIS] !== undefined) {
                    registers[ENV] = newEnv.extend('this', registers[THIS]);
                } else {
                    registers[ENV] = newEnv;
                }
            }
            return true;
        }

        // 2. SCHEME CONTINUATION INVOCATION
        // Check for callable Scheme continuations (they are also typeof 'function')
        if (isSchemeContinuation(func)) {
            return this.invokeContinuation(func, args, registers, interpreter);
        }

        // 3. JS FUNCTION APPLICATION
        // Regular JavaScript functions (including callable closures passed to JS)
        if (typeof func === 'function') {
            // CRITICAL: Push the current Scheme context before calling JS.
            // This allows callable closures/continuations invoked by JS to
            // properly track dynamic-wind frames for unwinding/rewinding.
            interpreter.pushJsContext(registers[FSTACK]);

            let result;
            try {
                result = func(...args);
            } finally {
                // Pop the context after JS returns (or throws)
                interpreter.popJsContext();
            }

            if (result instanceof TailCall) {
                const target = result.func;
                if (isSchemeClosure(target) || isSchemeContinuation(target) || typeof target === 'function') {
                    const tailArgs = result.args || [];
                    const argLiterals = tailArgs.map(a => new LiteralNode(a));
                    registers[CTL] = new TailAppNode(new LiteralNode(target), argLiterals);
                    return true;
                }
                registers[CTL] = target;
                return true;
            }

            registers[ANS] = result;
            return false;
        }

        throw new SchemeApplicationError(func);
    }

    /**
     * Invokes a captured continuation with proper dynamic-wind handling.
     * @private
     */
    invokeContinuation(func, args, registers, interpreter) {
        const currentStack = registers[FSTACK];
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
            actions.push(new TailAppNode(new LiteralNode(frame.after), []));
        }
        for (const frame of toRewind) {
            actions.push(new TailAppNode(new LiteralNode(frame.before), []));
        }

        // CRITICAL: Unwind JS stack (Return Value Mode)
        if (actions.length === 0) {
            const filteredStack = filterSentinelFrames(targetStack);
            registers[FSTACK] = [...filteredStack];
            registers[ANS] = value;

            if (interpreter.depth > 1) {
                throw new ContinuationUnwind(registers, true);
            }
            return false;
        }

        // Append the final restoration (with SentinelFrames filtered out)
        const filteredStack = filterSentinelFrames(targetStack);
        actions.push(new RestoreContinuation(filteredStack, value));

        // Execute via BeginFrame mechanism
        const firstAction = actions[0];
        const remainingActions = actions.slice(1);

        if (remainingActions.length > 0) {
            registers[FSTACK].push(new BeginFrame(remainingActions, this.env));
        }

        registers[CTL] = firstAction;

        // CRITICAL: Unwind JS stack (Tail Call Mode)
        if (interpreter.depth > 1) {
            throw new ContinuationUnwind(registers, false);
        }

        return true;
    }
}

// =============================================================================
// Frames - Dynamic Wind
// =============================================================================

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
        registers[FSTACK].push(new WindFrame(
            this.before,
            this.after,
            this.env
        ));

        registers[CTL] = new TailAppNode(new LiteralNode(this.thunk), []);
        registers[ENV] = this.env;
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
        const result = registers[ANS];

        registers[FSTACK].push(new RestoreValueFrame(result));

        registers[CTL] = new TailAppNode(new LiteralNode(this.after), []);
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
        registers[ANS] = this.savedValue;
        return false;
    }
}

// =============================================================================
// Frames - Multiple Values
// =============================================================================

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
        const result = registers[ANS];

        // Unpack Values object, or treat single value as 1-element array
        let args;
        if (result instanceof Values) {
            args = result.toArray();
        } else {
            args = [result];
        }

        // Create argument Literals for TailApp
        const argLiterals = args.map(a => new LiteralNode(a));

        // Invoke consumer with the values
        registers[CTL] = new TailAppNode(new LiteralNode(this.consumer), argLiterals);
        registers[ENV] = this.env;
        return true;
    }
}

// =============================================================================
// Frames - Exceptions
// =============================================================================

/**
 * Frame representing an active exception handler.
 * Installed by with-exception-handler, searched by raise.
 * When control returns normally through this frame, it just passes through.
 */
export class ExceptionHandlerFrame extends Executable {
    /**
     * @param {Closure|Function} handler - Exception handler procedure
     * @param {Environment} env - The captured environment
     */
    constructor(handler, env) {
        super();
        this.handler = handler;
        this.env = env;
    }

    step(registers, interpreter) {
        // Handler frame is popped normally - just pass through
        // The value in ANS is the result of the thunk
        return false;
    }
}

/**
 * Frame for resuming after a continuable exception.
 * If the handler returns, this frame restores the saved frames
 * and continues with the handler's return value.
 */
export class RaiseContinuableResumeFrame extends Executable {
    /**
     * @param {Array} savedFrames - Frames to restore on resumption
     * @param {Environment} env - The captured environment
     */
    constructor(savedFrames, env) {
        super();
        this.savedFrames = savedFrames;
        this.env = env;
    }

    step(registers, interpreter) {
        // Handler returned with a value - resume with that value
        // The saved frames expect the handler's return value in ANS
        // Push them back onto the stack
        for (const frame of this.savedFrames) {
            registers[FSTACK].push(frame);
        }

        // ANS already contains the handler's return value
        // Pop the next frame to continue execution
        return false;
    }
}

/**
 * Frame for handling return from a non-continuable exception.
 * If the handler returns, this frame re-raises the exception to outer handlers,
 * as per R7RS which states returning from a non-continuable exception handler
 * is undefined behavior (we implement it as re-raise).
 */
export class RaiseNonContinuableResumeFrame extends Executable {
    /**
     * @param {*} exception - The exception being raised
     * @param {Environment} env - The captured environment
     */
    constructor(exception, env) {
        super();
        this.exception = exception;
        this.env = env;
    }

    step(registers, interpreter) {
        // Handler returned from non-continuable exception
        // Re-raise the exception to continue propagating to outer handlers
        registers[CTL] = new RaiseNode(this.exception, false);
        return true;
    }
}

// =============================================================================
// Frame Registration
// =============================================================================

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
    CallWithValuesFrame,
    ExceptionHandlerFrame,
    RaiseContinuableResumeFrame,
    RaiseNonContinuableResumeFrame
});
