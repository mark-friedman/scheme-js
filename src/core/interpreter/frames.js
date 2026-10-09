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
import { isSchemeClosure, isSchemeContinuation, isSchemePrimitive, TailCall, ContinuationUnwind, Values, createContinuation, createNativeContinuation, SCHEME_RAW_CALL, callWithSchemeValues, callSchemeProcedure } from './values.js';
import { registerFrames, getWindFrameClass } from './frame_registry.js';
import { schemeToJsDeep, jsToScheme } from './js_interop.js';
import { Cons } from './cons.js';
import { Environment } from './environment.js';
import { globalContext } from './context.js';
import { GlobalRef, GLOBAL_SCOPE_ID, globalScopeRegistry } from './syntax_object.js';
import { SchemeApplicationError, SchemeArityError, SchemeError } from './errors.js';
import { UNWIND, finishUnwind, nativeFrameStack, openCompiledSegment, suspendFlush, restoreFlush, noteResume } from './unwind.js';

// Import AST nodes needed by frames (Literal, TailApp, RestoreContinuation)
// Note: This creates a dependency on ast_nodes, but it's a one-way dependency
import { LiteralNode, VariableNode, LibraryVariableNode, TailAppNode, RestoreContinuation, RaiseNode } from './ast_nodes.js';

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
        this.env.rebind(this.varName, closure);

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
        // A procedure assigned to a top-level name, as `nboyer` assigns every
        // one of its own, is the compiler tier's as much as a defined one.
        if (interpreter.tier && isSchemeClosure(value)) {
            callSchemeProcedure(interpreter.tier.bound, [this.name, value, this.env.findEnv(this.name) ?? false]);
        }
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

        // The compiler tier decides when a top-level procedure is compiled.
        if (interpreter.tier && isSchemeClosure(value)) {
            callSchemeProcedure(interpreter.tier.bound, [this.name, value, this.env]);
        }

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
 * Tail position is detected from the frame stack rather than from the expander,
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
 * Frame for a 'begin' expression, or any other sequence: a procedure body of
 * several expressions, and the forms that expand to `begin` such as `when`,
 * `unless` and `cond` clauses. Evaluates the remaining expressions in turn;
 * the sequence's value is the last one's.
 *
 * The frame exists only while some expression after the current one is left
 * to evaluate. The last expression is in tail position (R7RS 3.5) -- the
 * sequence has nothing left to do with its value -- so it is evaluated with no
 * frame for the sequence beneath it. An exhausted frame left there would make
 * any loop whose body ends in a sequence gain a frame per iteration, and would
 * hide a procedure's `DebugExitFrame` from the debugger's tail-call detection.
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
        if (this.remainingExprs.length > 1) {
            registers[FSTACK].push(new BeginFrame(this.remainingExprs.slice(1), this.env));
        }

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
    // A continuation a driver made has its frame stack made when first needed.
    const targetStack = func.fstack ?? (func.fstack = nativeFrameStack(func));

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

    // A continuation captured in this run of the interpreter, while the run
    // is still going: the two stacks share everything up to and including the
    // run's sentinel. A run that JavaScript started -- a callback, or the
    // library system called from the expander -- has the Scheme frames beneath
    // its caller under that sentinel, so its continuations hold them too; but
    // a jump to one of its own continuations stays in it, and the run returns
    // to the JavaScript that started it. Only a continuation reaching past the
    // run unwinds to the outermost one, which is all that can reinstate frames
    // beneath JavaScript that cannot be resumed.
    let sentinel = currentStack.length - 1;
    while (sentinel >= 0 && currentStack[sentinel].isSentinel !== true) sentinel--;
    const withinRun = sentinel >= 0 && ancestorIndex > sentinel;

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

    if (withinRun) {
        if (actions.length === 0) {
            registers[FSTACK] = [...targetStack];
            registers[ANS] = value;
            return false;
        }
        actions.push(new RestoreContinuation([...targetStack], value));
        if (actions.length > 1) registers[FSTACK].push(new BeginFrame(actions.slice(1), env));
        registers[CTL] = actions[0];
        return true;
    }

    // CRITICAL: Unwind JS stack (Return Value Mode)
    if (actions.length === 0) {
        const filteredStack = filterSentinelFrames(targetStack);
        registers[FSTACK] = [...filteredStack];
        registers[ANS] = value;

        if (interpreter.depth > 1) {
            throw new ContinuationUnwind(registers, true, func, args, targetStack);
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
        throw new ContinuationUnwind(registers, false, func, args, targetStack);
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
            } else if (expr.constructor === LibraryVariableNode) {
                values.push(expr.env.lookup(expr.name));
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

    // A top-level procedure waiting to be compiled, whose calls have run out,
    // or any of whose calls is due while the tier compiles every procedure
    // (`set-tier-eager!` in src/compiler/tier.scm). Compiled, it no longer
    // answers as a closure, and this call runs compiled below, as any call to
    // a compiled procedure does.
    if (isSchemeClosure(func) && func.tierCountdown !== 0
        && (--func.tierCountdown === 0 || interpreter.tier?.eager === true) && interpreter.tier) {
        callSchemeProcedure(interpreter.tier.due, [func]);
    }

    // 1. SCHEME CLOSURE APPLICATION
    // Check for callable Scheme closures first (they are typeof 'function')
    if (isSchemeClosure(func)) {
        // Called with the wrong number of arguments, it is an error (R7RS
        // 4.1.4), signalled as compiled code signals it. JavaScript's calls
        // arrive fitted to the parameters (`createClosure`), so only
        // Scheme's are checked.
        const argc = values.length - 1;
        const required = func.params.length;
        if (func.restParam ? argc < required : argc !== required) {
            throw new SchemeArityError(func.name || 'anonymous', required, func.restParam ? Infinity : required, argc);
        }
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

            // Instrumentation: record frame entry (tail-call aware), while
            // debugging is on: a runtime attached and off, as the CLI's is
            // until asked, would otherwise cost every call.
            if (interpreter.debugRuntime?.enabled) {
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

            // Instrumentation: record frame entry (tail-call aware), while
            // debugging is on: a runtime attached and off, as the CLI's is
            // until asked, would otherwise cost every call.
            if (interpreter.debugRuntime?.enabled) {
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

        // Compiled code called from here may move its frames to the heap
        // stack when the JavaScript stack gets deep, since the unwind that
        // does it ends here, or passes through this run to one it ends in;
        // beneath any other function it would not. Given
        // back after a normal return only: after an exception, `run` gives back
        // what it found on entry, and until then every call out of this run
        // sets it again. A `finally` here would sit in every nested run on the
        // JavaScript stack, and cost recursion alternating between compiled
        // and interpreted code 8% of the depth it can reach.
        const compiled = func.$compiled === true;
        const flush = compiled ? openCompiledSegment(interpreter.unwindsOut) : suspendFlush();
        // A compiled procedure's plain call faces JavaScript, converting; the
        // interpreter holds Scheme values, so calls its raw entry.
        const callee = compiled ? func[SCHEME_RAW_CALL] : func;
        let result;
        try {
            // A JavaScript function of JavaScript's own, rather than one that
            // takes Scheme values, is given its arguments converted
            // throughout, and its result is converted back one level, as
            // `js-invoke` converts it (an integral number to an exact
            // integer) -- always, as compiled code converts them (`callForeign`
            // in values.js): nothing else may choose a conversion here, or a
            // compiled procedure's tail calls, which come here, and its other
            // calls, which do not, would give one function different values.
            const foreign = !isSchemePrimitive(callee);
            const appliedArgs = foreign ? args.map(a => schemeToJsDeep(a)) : args;

            result = callee(...appliedArgs);
            if (foreign) result = jsToScheme(result);
            restoreFlush(flush);
        } finally {
            // Pop the context after JS returns (or throws)
            interpreter.popJsContext();
        }

        // The callee was compiled, and something below it began capturing a
        // continuation. Every compiled frame between that capture and here has
        // recorded itself on the way out; splicing them in where the boundary
        // sat completes the stack, and the capture can finish.
        if (result === UNWIND) {
            return finishUnwind(registers, interpreter, CAPTURE_HOOKS);
        }

        if (result instanceof TailCall) return takeTailCall(result, registers);

        registers[ANS] = result;
        return false;
    }

    throw new SchemeApplicationError(func);
}

/**
 * Makes the tail call a procedure returned, as the run's next step.
 * @param {TailCall} result - The tail call.
 * @param {Array} registers - The interpreter registers.
 * @returns {boolean} True, to continue the trampoline.
 */
function takeTailCall(result, registers) {
    const target = result.func;
    if (isSchemeClosure(target) || isSchemeContinuation(target) || typeof target === 'function') {
        const tailArgs = result.args || [];
        const argLiterals = tailArgs.map(a => new LiteralNode(a));
        registers[CTL] = new TailAppNode(new LiteralNode(target), argLiterals);
        return true;
    }
    registers[CTL] = target;
    // `eval` hands back the expression with the environment it is to run in,
    // which is not the one around the call.
    if (result.args instanceof Environment) registers[ENV] = result.args;
    return true;
}

/**
 * The first step of a run started to finish what a compiled procedure that
 * JavaScript called directly could not finish on its own
 * (`Interpreter.callCompiledEntry`): an unwind it began, moving its frames to
 * the heap or capturing a continuation, which the run's stack takes as a run's
 * own call would have; a tail call it returned; or what it threw, thrown
 * again here, where the run's handlers -- its exception handlers, a
 * continuation's unwinding -- deal with it as they would have had the run made
 * the call. Each is what the run would have met as the call's own step, had
 * the call gone through it as before, so the run's stack is the same.
 */
export class CompiledEntryRemainder extends Executable {
    /**
     * @param {'unwind'|'tail'|'throw'} kind - What is left.
     * @param {*} value - The tail call, or what was thrown.
     */
    constructor(kind, value) {
        super();
        this.kind = kind;
        this.value = value;
    }

    step(registers, interpreter) {
        if (this.kind === 'unwind') return finishUnwind(registers, interpreter, CAPTURE_HOOKS);
        if (this.kind === 'tail') return takeTailCall(this.value, registers);
        throw this.value;
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

        // The procedure resumes directly above the interpreter's frames, so
        // what it calls may move its frames to the heap stack as well. Given
        // back as for a call the interpreter makes; see `continueApplication`.
        const flush = openCompiledSegment(interpreter.unwindsOut);
        // As when the interpreter calls compiled code: whatever the procedure
        // calls back into Scheme -- an interpreted procedure, a continuation --
        // starts from this stack. Without it, invoking a continuation from a
        // resumed frame started from whatever stack was recorded last, one
        // without the winds in force here, and ran their before-thunks again.
        interpreter.pushJsContext(registers[FSTACK]);
        let result;
        try {
            noteResume(this.twin, callSchemeProcedure);
            result = this.twin(this.pc, frame);
            while (result instanceof TailCall) {
                result = callWithSchemeValues(result.func, result.args);
            }
        } finally {
            interpreter.popJsContext();
        }
        restoreFlush(flush);

        // Reinstating this frame ran code that captured a continuation of its
        // own. The frame's remaining work went into that capture on the way
        // out, like any other suspension, and this frame has already been
        // popped -- so what is left on the stack below is exactly the rest of
        // the new continuation, and the capture can be finished here.
        if (result === UNWIND) {
            return finishUnwind(registers, interpreter, CAPTURE_HOOKS);
        }

        registers[ANS] = result;
        return false;
    }
}

/**
 * Compiled frames moved to the heap because the JavaScript stack got deep, as
 * one interpreter frame.
 *
 * Moving them one frame each made the interpreter's frame stack as deep as the
 * recursion, and every call from compiled code into an interpreted procedure
 * starts a nested run with a copy of that stack -- so `map`, compiled, given an
 * interpreted procedure and a list of 20,000 elements took a second, and ran
 * out of memory at 100,000. Held here, a move adds at most one frame: one that
 * finds the frames of an earlier move on top of the stack, still waiting,
 * links to them instead.
 *
 * Nothing here is ever changed, only replaced, because a continuation shares
 * the frames of the stack it was taken from and may be resumed more than once.
 */
export class MovedFrames extends Executable {
    /**
     * @param {Array<CompiledFrame>} frames - Outermost first.
     * @param {number} top - The index of the innermost frame not yet resumed.
     * @param {MovedFrames|null} below - The frames of an earlier move, beneath.
     */
    constructor(frames, top, below) {
        super();
        this.frames = frames;
        this.top = top;
        this.below = below;
    }

    /**
     * Resumes the innermost frame, leaving the rest to be resumed after it.
     *
     * The frame is handed back to the interpreter to run, rather than run from
     * here: a resumed frame can be a long-running outer loop, and run from here
     * `earley`'s ran 13% slower.
     *
     * @param {Array} registers - The interpreter registers.
     * @param {Object} interpreter - The interpreter.
     * @returns {boolean} True: the frame is the next thing to run.
     */
    step(registers, interpreter) {
        const rest = this.top > 0 ? new MovedFrames(this.frames, this.top - 1, this.below) : this.below;
        if (rest !== null) registers[FSTACK].push(rest);
        registers[CTL] = this.frames[this.top];
        return true;
    }
}

/**
 * Puts compiled frames moved to the heap onto the frame stack, as a
 * `MovedFrames` linked to an earlier one on top if there is one.
 * @param {Array} fstack - The frame stack.
 * @param {Array<CompiledFrame>} frames - The frames, outermost first.
 * @returns {void}
 */
function pushMovedFrames(fstack, frames) {
    if (frames.length === 0) return;
    const top = fstack[fstack.length - 1];
    if (top instanceof MovedFrames) {
        fstack[fstack.length - 1] = new MovedFrames(frames, frames.length - 1, top);
    } else {
        fstack.push(new MovedFrames(frames, frames.length - 1, null));
    }
}

/**
 * What `unwind.js` needs from the interpreter in order to finish a capture,
 * in a run (`completeCapture`) or in a driver of its own (`finishUnwind`).
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
        [new LiteralNode(continuation)]),
    applyCall: (procedure, args) => new TailAppNode(
        new LiteralNode(procedure), args.map((arg) => new LiteralNode(arg))),
    pushMoved: pushMovedFrames,
    call: callWithSchemeValues,
    callScheme: callSchemeProcedure,
    takeTailCall: (result, registers) => takeTailCall(result, registers),
    nativeContinuation: createNativeContinuation,
    isTailCall: (x) => x instanceof TailCall,
    // A run's stack is its parent's, then the sentinel it started on, then its
    // own frames; the parent's are there already, where the unwind ends.
    segmentOf: (fstack) => {
        let start = fstack.length;
        while (start > 0 && fstack[start - 1].isSentinel !== true) start--;
        return fstack.slice(start);
    }
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
    RaiseNonContinuableResumeFrame,
    continueApplication
});
