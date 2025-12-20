/**
 * Stepable Classes for the Scheme interpreter.
 * 
 * This module contains all "stepable" objects - entities that can be executed
 * by the trampoline. This includes:
 * - AST Nodes: Produced by the Analyzer, represent program structure
 * - Frames: Represent "the rest of the computation", pushed onto fstack
 * 
 * All stepables implement the step(registers, interpreter) method.
 */

import { Closure, Continuation, TailCall, ContinuationUnwind, Values } from './values.js';
import { registerFrames, createBeginFrame, getWindFrameClass } from './frame_registry.js';
import { Cons } from './cons.js';

// =============================================================================
// Register Constants
// =============================================================================

/** Answer register - holds result of last computation */
export const ANS = 0;
/** Control register - holds next AST node or Frame to execute */
export const CTL = 1;
/** Environment register - holds current lexical environment */
export const ENV = 2;
/** Frame stack register - holds continuation frames */
export const FSTACK = 3;

// =============================================================================
// Base Class
// =============================================================================

/**
 * Base class for all executable objects (AST nodes and Frames).
 * All must have a `step` method.
 */
export class Executable {
    /**
     * Executes a single step of this node's computation.
     * @param {Array} registers - The [ans, ctl, env, fstack] registers array.
     * @param {Interpreter} interpreter - The interpreter instance.
     * @returns {boolean} `true` to continue trampoline, `false` to halt.
     */
    step(registers, interpreter) {
        throw new Error("step() must be implemented in subclasses");
    }
}

// =============================================================================
// AST Nodes - Atomic
// =============================================================================

/**
 * A literal value (number, string, boolean).
 * This is an atomic expression that returns immediately.
 */
export class Literal extends Executable {
    /**
     * @param {*} value - The literal value.
     */
    constructor(value) {
        super();
        this.value = value;
    }

    step(registers, interpreter) {
        registers[ANS] = this.value;
        return false;
    }

    toString() { return `(Literal ${this.value})`; }
}

/**
 * A variable lookup.
 * This is an atomic expression that looks up a name in the environment.
 */
export class Variable extends Executable {
    /**
     * @param {string} name - The variable name to look up.
     */
    constructor(name) {
        super();
        this.name = name;
    }

    step(registers, interpreter) {
        const env = registers[ENV];
        registers[ANS] = env.lookup(this.name);
        return false;
    }

    toString() { return `(Variable ${this.name})`; }
}

/**
 * A lambda expression.
 * Creates a closure capturing the current environment.
 */
export class Lambda extends Executable {
    /**
     * @param {Array<string>} params - Array of parameter names.
     * @param {Executable} body - The body expression.
     * @param {string|null} restParam - Name of rest parameter, or null if none.
     */
    constructor(params, body, restParam = null) {
        super();
        this.params = params;
        this.body = body;
        this.restParam = restParam;
    }

    step(registers, interpreter) {
        registers[ANS] = new Closure(this.params, this.body, registers[ENV], this.restParam);
        return false;
    }

    toString() { return `(Lambda (${this.params.join(' ')}${this.restParam ? ' . ' + this.restParam : ''}) ...`; }
}

// =============================================================================
// AST Nodes - Complex (push frames)
// =============================================================================

import * as FrameRegistry from './frame_registry.js';

/**
 * A 'let' binding.
 * Evaluates the binding expression, then the body in an extended environment.
 */
export class Let extends Executable {
    /**
     * @param {string} varName - The variable name to bind.
     * @param {Executable} binding - The expression to evaluate for the binding.
     * @param {Executable} body - The body expression.
     */
    constructor(varName, binding, body) {
        super();
        this.varName = varName;
        this.binding = binding;
        this.body = body;
    }

    step(registers, interpreter) {
        registers[FSTACK].push(FrameRegistry.createLetFrame(
            this.varName,
            this.body,
            registers[ENV]
        ));
        registers[CTL] = this.binding;
        return true;
    }
}

/**
 * A 'letrec' binding for recursive functions.
 * Creates environment with placeholder, evaluates lambda, then patches.
 */
export class LetRec extends Executable {
    /**
     * @param {string} varName - The variable name to bind.
     * @param {Lambda} lambdaExpr - Must be a Lambda expression.
     * @param {Executable} body - The body expression.
     */
    constructor(varName, lambdaExpr, body) {
        super();
        this.varName = varName;
        this.lambdaExpr = lambdaExpr;
        this.body = body;
    }

    step(registers, interpreter) {
        const newEnv = registers[ENV].extend(this.varName, null);

        registers[FSTACK].push(FrameRegistry.createLetRecFrame(
            this.varName,
            this.body,
            newEnv
        ));

        registers[CTL] = this.lambdaExpr;
        registers[ENV] = newEnv;
        return true;
    }
}

/**
 * An 'if' expression.
 * Evaluates the test, then one of the branches.
 */
export class If extends Executable {
    /**
     * @param {Executable} test - The test expression.
     * @param {Executable} consequent - The 'then' branch.
     * @param {Executable} alternative - The 'else' branch.
     */
    constructor(test, consequent, alternative) {
        super();
        this.test = test;
        this.consequent = consequent;
        this.alternative = alternative;
    }

    step(registers, interpreter) {
        registers[FSTACK].push(FrameRegistry.createIfFrame(
            this.consequent,
            this.alternative,
            registers[ENV]
        ));
        registers[CTL] = this.test;
        return true;
    }
}

/**
 * A variable assignment (set!).
 * Evaluates the expression, then updates the binding.
 */
export class Set extends Executable {
    /**
     * @param {string} name - The variable name to set.
     * @param {Executable} valueExpr - The expression to evaluate.
     */
    constructor(name, valueExpr) {
        super();
        this.name = name;
        this.valueExpr = valueExpr;
    }

    step(registers, interpreter) {
        registers[FSTACK].push(FrameRegistry.createSetFrame(
            this.name,
            registers[ENV]
        ));
        registers[CTL] = this.valueExpr;
        return true;
    }
}

/**
 * A variable definition (define).
 * Evaluates the expression, then creates a binding in the current scope.
 */
export class Define extends Executable {
    /**
     * @param {string} name - The variable name to define.
     * @param {Executable} valueExpr - The expression to evaluate.
     */
    constructor(name, valueExpr) {
        super();
        this.name = name;
        this.valueExpr = valueExpr;
    }

    step(registers, interpreter) {
        registers[FSTACK].push(FrameRegistry.createDefineFrame(
            this.name,
            registers[ENV]
        ));
        registers[CTL] = this.valueExpr;
        return true;
    }
}

/**
 * A tail-call application.
 * This is the only complex node that doesn't push its own frame first,
 * because it's in tail position.
 */
export class TailApp extends Executable {
    /**
     * @param {Executable} funcExpr - The function expression.
     * @param {Array<Executable>} argExprs - The argument expressions.
     */
    constructor(funcExpr, argExprs) {
        super();
        this.funcExpr = funcExpr;
        this.argExprs = argExprs;
    }

    step(registers, interpreter) {
        const allExprs = [this.funcExpr, ...this.argExprs];
        const firstExpr = allExprs[0];
        const remainingExprs = allExprs.slice(1);

        registers[FSTACK].push(FrameRegistry.createAppFrame(
            remainingExprs,
            [],
            registers[ENV]
        ));

        registers[CTL] = firstExpr;
        return true;
    }
}

/**
 * call-with-current-continuation.
 * Captures the current continuation and passes it to the lambda.
 */
export class CallCC extends Executable {
    /**
     * @param {Executable} lambdaExpr - Should be a (lambda (k) ...) expression.
     */
    constructor(lambdaExpr) {
        super();
        this.lambdaExpr = lambdaExpr;
    }

    step(registers, interpreter) {
        const continuation = new Continuation(registers[FSTACK]);

        registers[CTL] = new TailApp(
            this.lambdaExpr,
            [new Literal(continuation)]
        );
        return true;
    }
}

/**
 * A 'begin' expression.
 * Evaluates a sequence of expressions, returning the last.
 */
export class Begin extends Executable {
    /**
     * @param {Array<Executable>} expressions - The expressions to evaluate.
     */
    constructor(expressions) {
        super();
        this.expressions = expressions;
    }

    step(registers, interpreter) {
        if (this.expressions.length === 0) {
            registers[ANS] = null;
            return false;
        }

        const firstExpr = this.expressions[0];
        const remainingExprs = this.expressions.slice(1);

        if (remainingExprs.length > 0) {
            registers[FSTACK].push(FrameRegistry.createBeginFrame(
                remainingExprs,
                registers[ENV]
            ));
        }

        registers[CTL] = firstExpr;
        return true;
    }
}

/**
 * AST Node for top-level import.
 * Populates the environment with exports from a loaded library.
 * Libraries must be loaded into the registry first.
 */
export class ImportNode extends Executable {
    /**
     * @param {Array<object>} importSpecs - Array of { libraryName, only, except, rename, prefix }
     * @param {Function} getLibraryExports - Function to look up loaded library exports
     * @param {Function} applyImports - Function to apply imports to an environment
     */
    constructor(importSpecs, getLibraryExports, applyImports) {
        super();
        this.importSpecs = importSpecs;
        this.getLibraryExports = getLibraryExports;
        this.applyImports = applyImports;
    }

    step(registers, interpreter) {
        const env = registers[ENV];
        for (const spec of this.importSpecs) {
            const exports = this.getLibraryExports(spec.libraryName);
            if (!exports) {
                const libNameStr = Array.isArray(spec.libraryName) ? spec.libraryName.join('.') : spec.libraryName;
                throw new Error(`Import error: Library ${libNameStr} not loaded. Use loadLibrary first.`);
            }
            this.applyImports(env, exports, spec);
        }
        registers[ANS] = true;
        return false;
    }
}

// =============================================================================
// AST Nodes - Dynamic Wind
// =============================================================================

/**
 * AST Node to initialize 'dynamic-wind'.
 * Primitives cannot touch the stack, so they return this node to do it.
 */
export class DynamicWindInit extends Executable {
    /**
     * @param {Closure} before - The 'before' thunk.
     * @param {Closure} thunk - The main thunk.
     * @param {Closure} after - The 'after' thunk.
     */
    constructor(before, thunk, after) {
        super();
        this.before = before;
        this.thunk = thunk;
        this.after = after;
    }

    step(registers, interpreter) {
        registers[FSTACK].push(FrameRegistry.createDynamicWindSetupFrame(
            this.before,
            this.thunk,
            this.after,
            registers[ENV]
        ));

        registers[CTL] = new TailApp(new Literal(this.before), []);
        return true;
    }
}

/**
 * Special AST node to restore a continuation's stack and value.
 * Used as the final step in a dynamic-wind sequence.
 */
export class RestoreContinuation extends Executable {
    /**
     * @param {Array} targetStack - The stack to restore.
     * @param {*} value - The value to restore.
     */
    constructor(targetStack, value) {
        super();
        this.targetStack = targetStack;
        this.value = value;
    }

    step(registers, interpreter) {
        registers[FSTACK] = [...this.targetStack];
        registers[ANS] = this.value;
        return false;
    }
}

// =============================================================================
// AST Nodes - Multiple Values
// =============================================================================

/**
 * AST node for call-with-values.
 * Calls producer with no arguments, then applies consumer to the result(s).
 */
export class CallWithValuesNode extends Executable {
    /**
     * @param {Closure} producer - Zero-argument procedure that produces values
     * @param {Closure} consumer - Procedure that consumes the values
     */
    constructor(producer, consumer) {
        super();
        this.producer = producer;
        this.consumer = consumer;
    }

    step(registers, interpreter) {
        registers[FSTACK].push(FrameRegistry.createCallWithValuesFrame(
            this.consumer,
            registers[ENV]
        ));

        registers[CTL] = new TailApp(new Literal(this.producer), []);
        return true;
    }

    toString() { return "(CallWithValues ...)"; }
}

// =============================================================================
// AST Nodes - Exceptions
// =============================================================================

/**
 * AST node to set up with-exception-handler.
 * Pushes ExceptionHandlerFrame, then executes thunk.
 */
export class WithExceptionHandlerInit extends Executable {
    /**
     * @param {Closure|Function} handler - Exception handler procedure
     * @param {Closure|Function} thunk - Zero-argument procedure to execute
     */
    constructor(handler, thunk) {
        super();
        this.handler = handler;
        this.thunk = thunk;
    }

    step(registers, interpreter) {
        // Push handler frame onto stack
        registers[FSTACK].push(new ExceptionHandlerFrame(
            this.handler,
            registers[ENV]
        ));

        // Execute thunk
        registers[CTL] = new TailApp(new Literal(this.thunk), []);
        return true;
    }
}

/**
 * AST node to raise an exception.
 * Searches the stack for ExceptionHandlerFrame, then invokes it.
 */
export class RaiseNode extends Executable {
    /**
     * @param {*} exception - The exception value to raise
     * @param {boolean} continuable - Whether this is a continuable exception
     */
    constructor(exception, continuable = false) {
        super();
        this.exception = exception;
        this.continuable = continuable;
    }

    step(registers, interpreter) {
        const fstack = registers[FSTACK];

        // Search for the nearest ExceptionHandlerFrame
        let handlerIndex = -1;
        for (let i = fstack.length - 1; i >= 0; i--) {
            if (fstack[i] instanceof ExceptionHandlerFrame) {
                handlerIndex = i;
                break;
            }
        }

        if (handlerIndex === -1) {
            // No handler found - propagate as JS error
            const exc = this.exception;
            if (exc instanceof Error) {
                throw exc;
            }
            throw new Error(`Unhandled exception: ${exc}`);
        }

        // Get the handler
        const handlerFrame = fstack[handlerIndex];
        const handler = handlerFrame.handler;

        // Find WindFrames between current position and handler that need unwinding
        const WindFrameClass = getWindFrameClass();
        const framesToUnwind = fstack.slice(handlerIndex + 1).reverse()
            .filter(f => f instanceof WindFrameClass);

        // Build the action sequence: unwind frames, then invoke handler
        const actions = [];

        // Add 'after' thunks for each WindFrame to unwind
        for (const frame of framesToUnwind) {
            actions.push(new TailApp(new Literal(frame.after), []));
        }

        // The final action is to invoke the handler
        // We create an InvokeHandlerNode to handle the actual invocation
        // after unwinding is complete
        actions.push(new InvokeExceptionHandler(
            handler,
            this.exception,
            handlerIndex,
            this.continuable,
            fstack.slice(handlerIndex + 1) // Save frames for continuable
        ));

        // Truncate stack to handler (keeping handler for now, InvokeExceptionHandler will remove it)
        fstack.length = handlerIndex + 1;

        // Execute via Begin mechanism
        if (actions.length === 1) {
            registers[CTL] = actions[0];
        } else {
            const firstAction = actions[0];
            const remainingActions = actions.slice(1);
            if (remainingActions.length > 0) {
                registers[FSTACK].push(FrameRegistry.createBeginFrame(remainingActions, registers[ENV]));
            }
            registers[CTL] = firstAction;
        }
        return true;
    }
}

/**
 * AST node to invoke exception handler after unwinding is complete.
 * This is the final step after all 'after' thunks have run.
 */
export class InvokeExceptionHandler extends Executable {
    constructor(handler, exception, handlerIndex, continuable, savedFrames) {
        super();
        this.handler = handler;
        this.exception = exception;
        this.handlerIndex = handlerIndex;
        this.continuable = continuable;
        this.savedFrames = savedFrames;
    }

    step(registers, interpreter) {
        const fstack = registers[FSTACK];

        // Remove the handler frame
        fstack.pop(); // Pop the ExceptionHandlerFrame

        // For continuable: push a resume frame
        if (this.continuable) {
            fstack.push(new RaiseContinuableResumeFrame(
                this.savedFrames,
                registers[ENV]
            ));
        }

        // Invoke handler with the exception
        registers[CTL] = new TailApp(new Literal(this.handler), [new Literal(this.exception)]);
        return true;
    }
}

// =============================================================================
// Frames - Helper Functions
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
        registers[ANS] = value;
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
        const value = registers[ANS];
        this.env.define(this.name, value);
        registers[ANS] = this.name;
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

        // 1. CLOSURE APPLICATION
        if (func instanceof Closure) {
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
                registers[ENV] = func.env.extendMany(allParams, allArgs);
            } else {
                registers[ENV] = func.env.extendMany(func.params, args);
            }
            return true;
        }

        // 2. JS FUNCTION APPLICATION
        if (typeof func === 'function') {
            const jsArgs = args.map(arg => {
                if ((arg instanceof Closure || arg instanceof Continuation) && !func.skipBridge) {
                    return interpreter.createJsBridge(arg, registers[FSTACK]);
                }
                return arg;
            });

            const result = func(...jsArgs);

            if (result instanceof TailCall) {
                const target = result.func;
                if (target instanceof Closure || target instanceof Continuation || typeof target === 'function') {
                    const tailArgs = result.args || [];
                    const argLiterals = tailArgs.map(a => new Literal(a));
                    registers[CTL] = new TailApp(new Literal(target), argLiterals);
                    return true;
                }
                registers[CTL] = target;
                return true;
            }

            registers[ANS] = result;
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
            actions.push(new TailApp(new Literal(frame.after), []));
        }
        for (const frame of toRewind) {
            actions.push(new TailApp(new Literal(frame.before), []));
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

        registers[CTL] = new TailApp(new Literal(this.thunk), []);
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

        registers[CTL] = new TailApp(new Literal(this.after), []);
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
        const argLiterals = args.map(a => new Literal(a));

        // Invoke consumer with the values
        registers[CTL] = new TailApp(new Literal(this.consumer), argLiterals);
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
    CallWithValuesFrame
});
