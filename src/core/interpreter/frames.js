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
import { isSchemeClosure, isSchemeContinuation, isSchemePrimitive, TailCall, ContinuationUnwind, Values, createContinuation, SCHEME_RAW_CALL } from './values.js';
import { registerFrames, getWindFrameClass } from './frame_registry.js';
import { schemeToJsDeep } from './js_interop.js';
import { Cons } from './cons.js';
import { globalContext } from './context.js';
import { GlobalRef, GLOBAL_SCOPE_ID, globalScopeRegistry } from './syntax_object.js';
import { SchemeApplicationError, SchemeError } from './errors.js';
import { UNWIND, completeCapture } from './unwind.js';

// Import AST nodes needed by frames (Literal, TailApp, RestoreContinuation)
// Note: This creates a dependency on ast_nodes, but it's a one-way dependency
import { LiteralNode, VariableNode, TailAppNode, RestoreContinuation, RaiseNode } from './ast_nodes.js';

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Register a binding with all currently active scopes.
 * Called when a define is evaluated during library loading.
 * Uses globalContext for scope tracking.
 * 
 * @param {string} name - The binding name
 * @param {any} [value] - The bound value (unused if GlobalRef is preferred)
 */
function registerBindingWithCurrentScopes(name, value) {
    const definingScopes = globalContext.getDefiningScopes();
    // Determine scope set (default to GLOBAL_SCOPE_ID if empty)
    const scopes = definingScopes.length > 0
        ? new Set(definingScopes)
        : new Set([GLOBAL_SCOPE_ID]);

    // Determine the specific defining scope (for Environment resolution)
    const definingScope = definingScopes.length > 0
        ? definingScopes[definingScopes.length - 1]
        : null;

    // Always bind as a GlobalRef to ensure dynamic lookup in the environment
    globalScopeRegistry.bind(name, scopes, new GlobalRef(name, definingScope));
}

/**
 * Filters out SentinelFrames from a stack.
 * SentinelFrames are JS boundary markers that should not be executed
 * when restoring a continuation.
 * @param {Array} stack - The stack to filter
 * @returns {Array} Stack with SentinelFrames removed
 */
function filterSentinelFrames(stack) {
    // Matched by a property rather than by `constructor.name`, so that a
    // sentinel carrying extra information -- a compiled-code boundary, say --
    // is still recognised as one.
    return stack.filter(f => f.isSentinel !== true);
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
 * Records entry into a procedure for the debugger's shadow call stack.
 *
 * Tail calls must replace the current shadow frame rather than push a new one.
 * Pushing would both misreport the stack (a tail-recursive loop would appear
 * as unbounded recursion) and, worse, defeat tail-call optimization outright:
 * each pushed `DebugExitFrame` stays on the real frame stack until the whole
 * chain returns, so a tail loop's memory would grow with its iteration count
 * whenever debugging was switched on.
 *
 * Tail position is detected from the frame stack rather than from the analyzer,
 * which does not compute it. A `DebugExitFrame` sitting on top of the stack at
 * the moment of application means nothing is pending in the calling procedure,
 * so this call's result flows straight to that procedure's exit -- which is
 * exactly what it means to be in tail position. In a non-tail call the pending
 * work has already pushed its own frame above the exit frame.
 *
 * @param {Object} interpreter - The interpreter, carrying the debug runtime.
 * @param {Array} fstack - The current frame stack.
 * @param {Object} frameInfo - `{name, env, source}` for the procedure entered.
 * @returns {void}
 */
function recordDebugFrameEntry(interpreter, fstack, frameInfo) {
    const isTailCall = fstack.length > 0 && fstack[fstack.length - 1] instanceof DebugExitFrame;

    if (isTailCall) {
        // Reuse the exit frame already on the stack; replacing keeps the shadow
        // stack the same depth and records the TCO count for display.
        interpreter.debugRuntime.replaceFrame(frameInfo);
    } else {
        interpreter.debugRuntime.enterFrame(frameInfo);
        fstack.push(new DebugExitFrame());
    }
}

/**
 * Special frame pushed by the debugger to track function exit.
 */
export class DebugExitFrame extends Executable {
    step(registers, interpreter) {
        if (interpreter.debugRuntime) {
            interpreter.debugRuntime.exitFrame();
        }
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
     * @param {Array<Executable>} exprs - The operator followed by the operand
     *   expressions. This array belongs to the AST node and is shared by every
     *   frame for this call site; it is never modified.
     * @param {number} index - Index within `exprs` of the expression whose
     *   value is being awaited.
     * @param {Array<*>} values - Values of `exprs[0..index-1]`, in order.
     * @param {Environment} env - The captured environment.
     */
    constructor(exprs, index, values, env) {
        super();
        this.exprs = exprs;
        this.index = index;
        this.values = values;
        this.env = env;
    }

    step(registers, interpreter) {
        // A fresh `values` array is built on every step rather than the frame
        // being advanced in place. That is deliberate: `call/cc` captures the
        // frame stack by copying the array, so frames are shared with every
        // continuation captured while this call was being evaluated. Mutating a
        // frame would let a captured continuation observe operands evaluated
        // after its capture. `dynamic-wind` compounds this: its unwind/rewind
        // logic finds the common ancestor of two stacks by frame identity, so
        // frames cannot be cloned at capture time either.
        return continueApplication(
            this.exprs,
            this.index + 1,
            [...this.values, registers[ANS]],
            this.env,
            registers,
            interpreter
        );
    }

}


// =============================================================================
// Application
// =============================================================================

/**
 * Invokes a captured continuation, running the `dynamic-wind` thunks that lie
 * between the current stack and the captured one.
 *
 * A module-level function rather than a method so that both `AppFrame` and the
 * inlined application path can reach it.
 *
 * @param {Function} func - The continuation being invoked.
 * @param {Array<*>} args - The values being passed to it.
 * @param {Environment} env - Environment for any wind thunks that must run.
 * @param {Array} registers - The interpreter register array.
 * @param {Object} interpreter - The interpreter instance.
 * @returns {boolean} Whether the trampoline should continue.
 */
function invokeContinuationFrom(func, args, env, registers, interpreter) {
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
        registers[FSTACK].push(new BeginFrame(remainingActions, env));
    }

    registers[CTL] = firstAction;

    // CRITICAL: Unwind JS stack (Tail Call Mode)
    if (interpreter.depth > 1) {
        throw new ContinuationUnwind(registers, false);
    }

    return true;
}

/**
 * Continues an application: evaluates any operands that remain, then applies.
 *
 * `values` holds the already-evaluated operator and operands, in order;
 * `exprs[index]` is the next expression needing evaluation. `values` must be an
 * array owned by the caller -- it is mutated here before being handed to a new
 * frame, which is safe only because no frame already on the stack, and no
 * captured continuation, holds a reference to it.
 *
 * @param {Array<Executable>} exprs - Operator followed by operand expressions.
 * @param {number} index - Index of the next expression to evaluate.
 * @param {Array<*>} values - Values evaluated so far.
 * @param {Environment} env - The environment of the call site.
 * @param {Array} registers - The interpreter register array.
 * @param {Object} interpreter - The interpreter instance.
 * @returns {boolean} Whether the trampoline should continue.
 */
export function continueApplication(exprs, index, values, env, registers, interpreter) {
    // Operands that cannot capture a continuation are evaluated here rather
    // than suspended into a frame and bounced through the trampoline. A
    // literal and a variable reference each produce a value with no
    // sub-computation, so there is nothing for a continuation to be
    // captured in the middle of, and evaluating them in place is
    // indistinguishable from evaluating them through the trampoline.
    //
    // This matters because the frame machinery, not the work it schedules,
    // dominates the profile: a call like `(< n 2)` previously cost three
    // frames and six dispatches to compute one comparison.
    //
    // Not done while debugging: the debugger's breakpoint check happens per
    // dispatch, so inlining an operand would make it unstoppable. Suspending
    // on every operand reproduces the previous behaviour exactly, and the
    // fidelity matters more than the speed on that path.
    if (!(interpreter.debugRuntime && interpreter.debugRuntime.enabled)) {
        while (index < exprs.length) {
            const expr = exprs[index];
            if (expr.constructor === LiteralNode) {
                values.push(expr.value);
            } else if (expr.constructor === VariableNode) {
                values.push(env.lookup(expr.name));
            } else {
                break;
            }
            index++;
        }
    }

    if (index < exprs.length) {
        registers[FSTACK].push(new AppFrame(exprs, index, values, env));
        registers[CTL] = exprs[index];
        registers[ENV] = env;
        return true;
    }

    // All arguments evaluated, ready to apply
    const func = values[0];

    // 1. SCHEME CLOSURE APPLICATION
    // Check for callable Scheme closures first (they are typeof 'function')
    if (isSchemeClosure(func)) {
        registers[CTL] = func.body;

        // Handle rest parameter if present
        if (func.restParam) {
            // The operands are only materialized as their own array on the
            // paths that need one. The fixed-parameter path -- the common
            // case by a wide margin -- reads them in place instead.
            const args = values.slice(1);
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
            const allOriginalParams = [...(func.originalParams || func.params), (func.originalRestParam || func.restParam)];
            const allArgs = [...requiredArgs, restList];
            let newEnv = func.env.extendMany(allParams, allArgs, allOriginalParams);
            // Bind 'this' pseudo-variable if available (method call)
            if (registers[THIS] !== undefined) {
                registers[ENV] = newEnv.extend('this', registers[THIS], 'this');
            } else {
                registers[ENV] = newEnv;
            }

            // Instrumentation: record frame entry (tail-call aware)
            if (interpreter.debugRuntime) {
                recordDebugFrameEntry(interpreter, registers[FSTACK], {
                    name: func.name || 'anonymous',
                    env: newEnv,
                    source: func.source
                });
            }
        } else {
            // Read operands straight out of `values` at offset 1, rather
            // than from the `args` copy, saving an array per call.
            let newEnv = func.env.extendManyFrom(func.params, values, 1, func.originalParams);
            // Bind 'this' pseudo-variable if available (method call)
            if (registers[THIS] !== undefined) {
                registers[ENV] = newEnv.extend('this', registers[THIS], 'this');
            } else {
                registers[ENV] = newEnv;
            }

            // Instrumentation: record frame entry (tail-call aware)
            if (interpreter.debugRuntime) {
                recordDebugFrameEntry(interpreter, registers[FSTACK], {
                    name: func.name || 'anonymous',
                    env: newEnv,
                    source: func.source
                });
            }
        }
        return true;
    }

    const args = values.slice(1);

    // 2. SCHEME CONTINUATION INVOCATION
    // Check for callable Scheme continuations (they are also typeof 'function')
    if (isSchemeContinuation(func)) {
        return invokeContinuationFrom(func, args, env, registers, interpreter);
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
            // If it's a foreign JS function (not a Scheme closure/primitive),
            // auto-convert arguments (e.g., BigInt -> Number)
            let appliedArgs = args;
            if (!isSchemePrimitive(func)) {
                // Respect the current js-auto-convert mode
                const mode = interpreter.jsAutoConvert ?? 'deep';
                if (mode === 'deep' || mode === true) {
                    appliedArgs = args.map(a => schemeToJsDeep(a));
                } else if (mode === 'shallow' || mode === false) {
                    // We use a light conversion for shallow mode
                    appliedArgs = args.map(a => (typeof a === 'bigint' ? Number(a) : a));
                }
            }

            result = func(...appliedArgs);
        } finally {
            // Pop the context after JS returns (or throws)
            interpreter.popJsContext();
        }

        // The callee was compiled, and something below it began capturing a
        // continuation. Every compiled frame between that capture and here has
        // recorded itself on the way out; splicing them in where the boundary
        // sat completes the stack, and the capture can finish.
        if (result === UNWIND) {
            return completeCapture(registers, interpreter, CAPTURE_HOOKS);
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
// Compiled Frames
// =============================================================================

/**
 * A suspended compiled procedure, as an interpreter frame.
 *
 * Once one of these is on the frame stack, a compiled procedure is part of a
 * continuation like anything else: the interpreter pops it and calls `step`,
 * and `step` resumes the procedure just after the call it was making.
 */
export class CompiledFrame extends Executable {
    /**
     * @param {Function} twin - The procedure's resumable form.
     * @param {number} pc - The block to continue at.
     * @param {Object} slots - Spilled local variables.
     */
    constructor(twin, pc, slots) {
        super();
        this.twin = twin;
        this.pc = pc;
        this.slots = slots;
    }

    /**
     * Resumes the procedure just after the call it was suspended at.
     * @param {Array} registers - The interpreter registers.
     * @param {Object} interpreter - The interpreter.
     * @returns {boolean} Whether to continue the trampoline.
     */
    step(registers, interpreter) {
        // Copied, never shared. A continuation may be invoked more than once,
        // and the second invocation must not see what the first assigned. This
        // is what makes a continuation multi-shot rather than one-shot.
        const frame = { ...this.slots, $r: registers[ANS] };

        let result = this.twin(this.pc, frame);
        while (result instanceof TailCall) {
            const raw = result.func[SCHEME_RAW_CALL];
            result = raw === undefined
                ? result.func(...result.args) : raw(...result.args);
        }

        // Reinstating this frame ran code that captured a continuation of its
        // own. The frame's remaining work went into that capture on the way
        // out, like any other suspension, and this frame has already been
        // popped -- so what is left on the stack below is exactly the rest of
        // the new continuation, and the capture can be finished here.
        if (result === UNWIND) {
            return completeCapture(registers, interpreter, CAPTURE_HOOKS);
        }

        registers[ANS] = result;
        return false;
    }
}

/**
 * What `unwind.js` needs from the interpreter in order to finish a capture.
 *
 * It owns the protocol but deliberately not the interpreter's vocabulary, so
 * the dependency points one way: the interpreter knows about compiled code only
 * through that module, and that module knows nothing about the compiler.
 */
const CAPTURE_HOOKS = {
    frameFor: (twin, pc, slots) => new CompiledFrame(twin, pc, slots),
    makeContinuation: (stack, interp) => createContinuation(stack, interp),
    // The receiver is an expression when `call/cc` was reached in interpreted
    // code, and a procedure value when compiled code called it directly, where
    // there is no expression left to evaluate.
    applyReceiver: (receiver, continuation) => new TailAppNode(
        receiver instanceof Executable ? receiver : new LiteralNode(receiver),
        [new LiteralNode(continuation)])
};

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
    RaiseNonContinuableResumeFrame,
    continueApplication
});
