/**
 * AST Node Classes for the Scheme interpreter.
 * 
 * This module contains all AST (Abstract Syntax Tree) node classes.
 * AST nodes are produced by the Analyzer and represent program structure.
 * All nodes extend Executable and implement a step() method.
 */

import { Executable, ANS, CTL, ENV, FSTACK } from './stepables_base.js';
import {
    createClosure, createContinuation, isSchemeClosure, isSchemeContinuation, TailCall
} from './values.js';
import * as FrameRegistry from './frame_registry.js';
import { GlobalRef } from './syntax_object.js';
import { globalContext, GLOBAL_SCOPE_ID } from './context.js';
import { SchemeError } from './errors.js';
import { CaptureUnwind, beginCapture, CAPTURE_UNDER_PRIMITIVE } from './unwind.js';

// =============================================================================
// Helper Function
// =============================================================================

/**
 * Helper to ensure a value is an Executable AST node.
 * If it's already an Executable, returns it as-is.
 * Otherwise, wraps it in a Literal.
 * @param {*} obj 
 * @returns {Executable}
 */
export function ensureExecutable(obj) {
    if (obj instanceof Executable) return obj;
    return new LiteralNode(obj);
}

// =============================================================================
// AST Nodes - Atomic
// =============================================================================

/**
 * A literal value (number, string, boolean).
 * This is an atomic expression that returns immediately.
 */
export class LiteralNode extends Executable {
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
export class VariableNode extends Executable {
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
 * A scoped variable lookup.
 * This is used for macro free variables that carry scope marks.
 * It first checks the scope binding registry, then falls back to the environment.
 */
export class ScopedVariable extends Executable {
    /**
     * @param {string} name - The variable name to look up.
     * @param {Set<number>} scopes - The scope marks for this identifier.
     * @param {ScopeBindingRegistry} scopeRegistry - The registry to check first.
     */
    constructor(name, scopes, scopeRegistry) {
        super();
        this.name = name;
        this.scopes = scopes;
        this.scopeRegistry = scopeRegistry;
    }

    step(registers, interpreter) {
        // First, check scope registry for a marked binding
        if (this.scopeRegistry) {
            const resolved = this.scopeRegistry.resolve({ name: this.name, scopes: this.scopes });
            if (resolved !== null) {
                if (resolved instanceof GlobalRef) {
                    // Global/Dynamic lookup (for user defined globals or library internals)
                    // If the ref carries a scope, try to find the specific library environment
                    let envVal;
                    let found = false;

                    if (resolved.scope) {
                        const libEnv = globalContext.lookupLibraryEnv(resolved.scope);
                        if (libEnv) {
                            // Try to lookup in library env (findEnv avoids throwing if missing)
                            if (libEnv.findEnv(resolved.name)) {
                                envVal = libEnv.lookup(resolved.name);
                                found = true;
                            }
                        }
                    }

                    if (!found) {
                        // Fall back to current runtime environment
                        // This handles macro-introduced bindings that are local, not global
                        const env = registers[ENV];
                        envVal = env.lookup(resolved.name);
                    }
                    registers[ANS] = envVal;
                } else {
                    // Constant/Macro binding
                    registers[ANS] = resolved;
                }
                return false;
            }
        }

        // Fall back to regular environment lookup
        const env = registers[ENV];
        registers[ANS] = env.lookup(this.name);
        return false;
    }

    toString() { return `(ScopedVariable ${this.name} {${[...this.scopes].join(',')}})`; }
}

/**
 * A reference, introduced by a macro a library defined, to a binding in that
 * library's own environment.
 *
 * A macro's template means the bindings where the macro was written, so an
 * exported macro whose template calls a procedure the library keeps to itself
 * still calls it from a program that cannot name it. The expander makes one of
 * these when the use site's own environment would not find that same binding
 * by name; the name is looked up in the library's environment when it is
 * evaluated, since a library procedure may be redefined there after the use
 * was analyzed.
 *
 * Not a `VariableNode`, since compiled code reads a global from the
 * environment its procedure closes over, and this from the library's: the
 * compiler reads it through the library's environment's cell for it
 * (`library-global-key` in src/compiler/ir.scm).
 */
export class LibraryVariableNode extends Executable {
    /**
     * @param {string} name - The name, as the library binds it.
     * @param {Environment} env - The library's environment.
     */
    constructor(name, env) {
        super();
        this.name = name;
        this.env = env;
    }

    step(registers, interpreter) {
        registers[ANS] = this.env.lookup(this.name);
        return false;
    }

    toString() { return `(LibraryVariable ${this.name})`; }
}

/**
 * An assignment, introduced by a macro a library defined, to a binding in that
 * library's own environment: the `set!` counterpart of `LibraryVariableNode`.
 * Not a `SetNode`, for the same reason that is not a `VariableNode`.
 */
export class LibrarySetNode extends Executable {
    /**
     * @param {string} name - The name, as the library binds it.
     * @param {Environment} env - The library's environment.
     * @param {Executable} valueExpr - The expression to evaluate.
     */
    constructor(name, env, valueExpr) {
        super();
        this.name = name;
        this.env = env;
        this.valueExpr = valueExpr;
    }

    step(registers, interpreter) {
        registers[FSTACK].push(FrameRegistry.createSetFrame(this.name, this.env));
        registers[CTL] = this.valueExpr;
        return true;
    }

    toString() { return `(LibrarySet ${this.name})`; }
}

/**
 * A lambda expression.
 * Creates a closure capturing the current environment.
 */
export class LambdaNode extends Executable {
    /**
     * @param {Array<string>} params - Array of renamed parameter names.
     * @param {Executable} body - The body expression.
     * @param {string|null} restParam - Renamed rest parameter, or null if none.
     * @param {string} [name='anonymous'] - Optional name for debugging.
     * @param {Array<string>} [originalParams] - Original parameter names.
     * @param {string|null} [originalRestParam] - Original rest parameter name.
     */
    constructor(params, body, restParam = null, name = 'anonymous', originalParams = null, originalRestParam = null) {
        super();
        this.params = params;
        this.body = body;
        this.restParam = restParam;
        this.name = name;
        this.originalParams = originalParams || params;
        this.originalRestParam = originalRestParam || restParam;
    }

    step(registers, interpreter) {
        registers[ANS] = createClosure(
            this.params,
            this.body,
            registers[ENV],
            this.restParam,
            interpreter,
            this.name,
            this.source,
            this.originalParams,
            this.originalRestParam
        );
        return false;
    }

    toString() { return `(Lambda (${this.params.join(' ')}${this.restParam ? ' . ' + this.restParam : ''}) ...`; }
}

// =============================================================================
// AST Nodes - Complex (push frames)
// =============================================================================

/**
 * A 'let' binding.
 * Evaluates the binding expression, then the body in an extended environment.
 */
export class LetNode extends Executable {
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
export class LetRecNode extends Executable {
    /**
     * A `letrec` whose initializers are all lambda expressions.
     *
     * This is the shape every real `letrec` has -- a named `let`, a set of
     * internal definitions, a pair of mutually recursive procedures -- and it
     * is worth a node of its own for two reasons.
     *
     * **Semantics.** R7RS requires all initializers to be evaluated before any
     * variable is assigned. Evaluating a lambda expression has no side effects
     * and cannot observe another binding's value, so for this shape the
     * requirement is satisfied trivially and `letrec` and `letrec*` agree. The
     * general case, where an initializer is an arbitrary expression, still goes
     * through the desugaring in `analyzeLetRec`.
     *
     * **Compilability.** The previous expansion routed every lambda through
     * `(list init ...)` and `(car temp)`, so the compiler could not see that a
     * loop variable held the lambda two forms up, and declined every named
     * `let`. Here the binding is explicit, and both tiers can read it.
     *
     * @param {Array<string>} names - Renamed variables, bound simultaneously.
     * @param {Array<LambdaNode>} lambdaExprs - One lambda per name, in order.
     * @param {Executable} body - The body expression.
     * @param {Array<string>} [originalNames] - Names before alpha-renaming.
     */
    constructor(names, lambdaExprs, body, originalNames = null) {
        super();
        this.names = names;
        this.lambdaExprs = lambdaExprs;
        this.body = body;
        this.originalNames = originalNames || names;
    }

    step(registers, interpreter) {
        // Every binding is created before any closure is built, and every
        // closure captures that same environment -- which is what makes the
        // group mutually recursive. No frame is pushed because building a
        // closure cannot trampoline.
        const newEnv = registers[ENV].extendMany(
            this.names, new Array(this.names.length).fill(undefined), this.originalNames);

        for (let i = 0; i < this.names.length; i++) {
            const le = this.lambdaExprs[i];
            newEnv.bindings.set(this.names[i], createClosure(
                le.params, le.body, newEnv, le.restParam, interpreter,
                le.name, le.source, le.originalParams, le.originalRestParam));
        }

        registers[ENV] = newEnv;
        registers[CTL] = this.body;
        return true;
    }
}

/**
 * An 'if' expression.
 * Evaluates the test, then one of the branches.
 */
export class IfNode extends Executable {
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
        const env = registers[ENV];

        // A test that is a literal or a variable reference is evaluated here
        // and branched on immediately, instead of pushing a frame and bouncing
        // through the trampoline to read a name. Same reasoning as the operand
        // inlining in `continueApplication`: neither can capture a continuation,
        // so there is no suspension point to preserve.
        //
        // Skipped while debugging so the test expression still reaches the
        // dispatcher, where breakpoints are checked.
        if (!(interpreter.debugRuntime && interpreter.debugRuntime.enabled)) {
            const test = this.test;
            let testResult;
            if (test.constructor === LiteralNode) {
                testResult = test.value;
            } else if (test.constructor === VariableNode) {
                testResult = env.lookup(test.name);
            } else if (test.constructor === LibraryVariableNode) {
                testResult = test.env.lookup(test.name);
            } else {
                registers[FSTACK].push(FrameRegistry.createIfFrame(
                    this.consequent, this.alternative, env));
                registers[CTL] = test;
                return true;
            }
            registers[CTL] = testResult !== false ? this.consequent : this.alternative;
            return true;
        }

        registers[FSTACK].push(FrameRegistry.createIfFrame(
            this.consequent,
            this.alternative,
            env
        ));
        registers[CTL] = this.test;
        return true;
    }
}

/**
 * A variable assignment (set!).
 * Evaluates the expression, then updates the binding.
 */
export class SetNode extends Executable {
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
export class DefineNode extends Executable {
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
export class TailAppNode extends Executable {
    /**
     * @param {Executable} funcExpr - The function expression.
     * @param {Array<Executable>} argExprs - The argument expressions.
     */
    constructor(funcExpr, argExprs) {
        super();
        this.funcExpr = funcExpr;
        this.argExprs = argExprs;
        /**
         * Operator and operands as one array, built once at analysis time.
         * This was previously rebuilt -- spread and then sliced -- on every
         * single evaluation of the call site, re-deriving at run time something
         * already known statically.
         * @type {Array<Executable>}
         */
        this.exprs = [funcExpr, ...argExprs];
    }

    step(registers, interpreter) {
        const exprs = this.exprs;
        const env = registers[ENV];
        const operator = exprs[0];

        // When the operator is itself a literal or a variable reference -- which
        // it is for essentially every call in ordinary code -- it can be
        // evaluated here and the application continued directly, instead of
        // suspending into a frame and bouncing through the trampoline just to
        // read a name. `continueApplication` then inlines the operands that are
        // likewise incapable of capturing a continuation, so a call such as
        // `(< n 2)` completes within this single dispatch.
        //
        // Skipped while debugging, so that every subexpression still passes
        // through the dispatcher where breakpoints are checked.
        if (!(interpreter.debugRuntime && interpreter.debugRuntime.enabled)) {
            if (operator.constructor === LiteralNode) {
                return FrameRegistry.continueApplication(
                    exprs, 1, [operator.value], env, registers, interpreter);
            }
            if (operator.constructor === VariableNode) {
                return FrameRegistry.continueApplication(
                    exprs, 1, [env.lookup(operator.name)], env, registers, interpreter);
            }
            if (operator.constructor === LibraryVariableNode) {
                return FrameRegistry.continueApplication(
                    exprs, 1, [operator.env.lookup(operator.name)], env, registers, interpreter);
            }
        }

        registers[FSTACK].push(FrameRegistry.createAppFrame(exprs, 0, [], env));
        registers[CTL] = operator;
        return true;
    }
}

/**
 * call-with-current-continuation.
 * Captures the current continuation and passes it to the lambda.
 */
export class CallCCNode extends Executable {
    /**
     * @param {Executable} lambdaExpr - Should be a (lambda (k) ...) expression.
     */
    constructor(lambdaExpr) {
        super();
        this.lambdaExpr = lambdaExpr;
    }

    step(registers, interpreter) {
        // A continuation is the interpreter's frame stack. Compiled procedures
        // do not appear in it -- they run in JavaScript stack frames -- so if
        // compiled code called this run, a continuation built from this stack
        // alone would silently omit everything it had left to do. Those frames
        // are brought in by unwinding: this abandons the run, each compiled
        // frame records itself on the way out, each run of the interpreter the
        // unwind passes through adds its own frames, and the first run that
        // cannot pass it on finishes the capture (`completeCapture`). A run is
        // told apart by the sentinel it started on, which is the nearest one.
        const fstack = registers[FSTACK];
        let start = fstack.length;
        while (start > 0 && fstack[start - 1].isSentinel !== true) start--;
        // Called from an inline expansion of a redefined primitive, which has
        // no point to resume from.
        if (start > 0 && fstack[start - 1].refusesCapture === true) {
            throw new SchemeError(CAPTURE_UNDER_PRIMITIVE);
        }
        if (start > 0 && fstack[start - 1].compiledBoundary === true) {
            beginCapture({
                lambdaExpr: this.lambdaExpr,
                env: registers[ENV],
                segment: fstack.slice(start)
            });
            throw new CaptureUnwind();
        }

        const continuation = createContinuation(registers[FSTACK], interpreter);

        registers[CTL] = new TailAppNode(
            this.lambdaExpr,
            [new LiteralNode(continuation)]
        );
        return true;
    }
}

/**
 * A 'begin' expression.
 * Evaluates a sequence of expressions, returning the last.
 */
export class BeginNode extends Executable {
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
 * AST Node for import.
 * Imports its import sets into the environment it runs in, loading their
 * libraries synchronously.
 */
export class ImportNode extends Executable {
    /**
     * @param {Array} importSpecs - The import sets, as the form writes them.
     * @param {Function} importLibraries - Imports import sets into an
     *   environment (`importLibraries` in library_loader.js).
     * @param {Function} analyze - Analyze function for loading libraries
     */
    constructor(importSpecs, importLibraries, analyze) {
        super();
        this.importSpecs = importSpecs;
        this.importLibraries = importLibraries;
        this.analyze = analyze;
    }

    step(registers, interpreter) {
        this.importLibraries(this.importSpecs, this.analyze, interpreter, registers[ENV]);
        registers[ANS] = true;
        return false;
    }
}

/**
 * AST Node for define-library.
 * Registers a new library definition at runtime.
 */
export class DefineLibraryNode extends Executable {
    /**
     * @param {Cons} form - The define-library form.
     * @param {Function} defineLibrary - Defines and registers a library from
     *   its form (`defineLibrary` in library_loader.js).
     * @param {Function} analyze - Analyze function
     */
    constructor(form, defineLibrary, analyze) {
        super();
        this.form = form;
        this.defineLibrary = defineLibrary;
        this.analyze = analyze;
    }

    step(registers, interpreter) {
        this.defineLibrary(this.form, this.analyze, interpreter, registers[ENV]);
        registers[ANS] = true; // Returns true/unspecified
        return false;
    }
}

/**
 * A macro definition, restored from a library's prebuilt table: binds the
 * macro, pending, where `define-syntax` or `define-macro` would have bound
 * it -- in the library being loaded and, by name, for the process -- as the
 * definition and the scope of the library. The expander makes its transformer
 * from the definition the first time the macro is used (`realize!` in
 * expander.scm): a library the expander is written with is restored before
 * there is an expander to make one.
 */
export class DefineSyntaxNode extends Executable {
    /**
     * @param {string} name - The macro's name.
     * @param {*} definition - Its `define-syntax` or `define-macro` form.
     */
    constructor(name, definition) {
        super();
        this.name = name;
        this.definition = definition;
    }

    step(registers, interpreter) {
        const defining = globalContext.definingScopes;
        const scope = defining.length > 0 ? defining[defining.length - 1] : null;
        const pending = { pendingMacro: this.definition, scope: scope ?? GLOBAL_SCOPE_ID };
        globalContext.macroRegistry.define(this.name, pending);
        if (scope !== null) globalContext.defineKeyword(scope, this.name, this.name, pending);
        else globalContext.forgetKeyword(GLOBAL_SCOPE_ID, this.name);
        registers[ANS] = null;
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

        registers[CTL] = new TailAppNode(ensureExecutable(this.before), []);
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

        registers[CTL] = new TailAppNode(ensureExecutable(this.producer), []);
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
        // Push handler frame onto stack (using factory to avoid circular dep)
        registers[FSTACK].push(FrameRegistry.createExceptionHandlerFrame(
            this.handler,
            registers[ENV]
        ));

        // Execute thunk
        registers[CTL] = new TailAppNode(ensureExecutable(this.thunk), []);
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

        // Check if we should pause on this exception (debug mode)
        if (interpreter.debugRuntime?.shouldBreakOnException(this.exception, fstack)) {
            interpreter.debugRuntime.pauseOnException(this, registers);
            // After pause, execution will continue from here when resumed
            // The exception handling will proceed normally
        }

        // Search for the nearest ExceptionHandlerFrame
        const ExceptionHandlerFrame = FrameRegistry.getExceptionHandlerFrameClass();
        let handlerIndex = -1;
        for (let i = fstack.length - 1; i >= 0; i--) {
            if (fstack[i] instanceof ExceptionHandlerFrame) {
                handlerIndex = i;
                break;
            }
        }

        if (handlerIndex === -1) {
            // No handler found - propagate as JS error
            throw unhandled(this.exception);
        }

        // Get the handler
        const handlerFrame = fstack[handlerIndex];
        const handler = handlerFrame.handler;

        // Find WindFrames between current position and handler that need unwinding
        const WindFrameClass = FrameRegistry.getWindFrameClass();
        const framesToUnwind = fstack.slice(handlerIndex + 1).reverse()
            .filter(f => f instanceof WindFrameClass);

        // Build the action sequence: unwind frames, then invoke handler
        const actions = [];

        // Add 'after' thunks for each WindFrame to unwind
        for (const frame of framesToUnwind) {
            actions.push(new TailAppNode(ensureExecutable(frame.after), []));
        }

        // The final action is to invoke the handler
        // We create an InvokeExceptionHandler to handle the actual invocation
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
 * What a raise nobody handles throws: the raised value itself when it is an
 * error, and otherwise an error describing it.
 * @param {*} exception - The raised value.
 * @returns {Error} The value to throw.
 */
function unhandled(exception) {
    if (exception instanceof Error) return exception;
    return new SchemeError(`Unhandled exception: ${exception}`, [exception]);
}

// =============================================================================
// Raising from compiled code
// =============================================================================

/**
 * Raises compiled code has thrown for an interpreter run to perform, each
 * mapped to the value raised. Weak, since a raise nobody performs -- one that
 * reached a JavaScript caller with no run beneath it -- is never taken out.
 * @type {WeakMap<Error, *>}
 */
const compiledRaises = new WeakMap();

/**
 * Builds a pending raise: what `raise`, `raise-continuable` and `error` return
 * rather than raising themselves.
 *
 * The interpreter performs a pending raise by running its node, which looks for
 * a handler on the interpreter's frame stack. Compiled code cannot run a node:
 * it continues any pending call by calling the call's function with its
 * arguments, through the function's raw entry if it has one. So `RaiseNode`
 * has a raw entry, `raiseFromCompiledCode`, and because a raw entry is called
 * without a receiver, the arguments carry what it needs. The interpreter
 * ignores them.
 *
 * @param {*} exception - The value raised.
 * @param {boolean} continuable - Whether a handler may return to the raise.
 * @returns {TailCall} The pending raise.
 */
export function pendingRaise(exception, continuable) {
    return new TailCall(new RaiseNode(exception, continuable), [exception, continuable]);
}

/**
 * Performs a raise that reached compiled code, by throwing it to the nearest
 * interpreter run for that run to perform.
 *
 * This is the raise the interpreter would have performed. A compiled procedure
 * never establishes a handler or a `dynamic-wind` -- one that names them is not
 * compiled -- so the handlers and winds in force where compiled code raises are
 * exactly those on the frame stack of the run beneath it, and that run performs
 * the raise with the same `RaiseNode`, the debugger's pause on an uncaught
 * exception included. The compiled frames the throw leaves are abandoned, which
 * is what a raise that cannot return does to them anyway.
 *
 * A continuable raise can return: a handler's value becomes the value of
 * `raise-continuable`, in the frame that raised. That frame is compiled and the
 * throw would have left it, so the raise is refused, loudly, rather than having
 * the value arrive somewhere else. Compiled code only reaches one by being
 * handed `raise-continuable` as a value, since a procedure that names it is not
 * compiled.
 *
 * What is thrown is what an unhandled raise throws, so a JavaScript caller with
 * no run beneath it -- host code calling a compiled procedure directly --
 * receives what it would have from an interpreted one.
 *
 * @param {*} exception - The value raised.
 * @param {boolean} continuable - Whether a handler may return to the raise.
 * @returns {never}
 * @throws {Error} Always.
 */
function raiseFromCompiledCode(exception, continuable) {
    if (continuable) {
        throw new SchemeError(
            'raise-continuable: called from compiled code, which a handler cannot return to; '
            + 'this is not yet supported. Run this program with its code interpreted '
            + '(--no-compile at the command line, setUserCodeCompilation(false) in a page).',
            [exception]);
    }
    const thrown = unhandled(exception);
    compiledRaises.set(thrown, exception);
    throw thrown;
}

// `SCHEME_RAW_CALL` from values.js, which is `Symbol.for('scheme.rawCall')`.
// Named by its key here because values.js imports this module, so on the way
// in its binding is not yet initialised when this line runs.
RaiseNode.prototype[Symbol.for('scheme.rawCall')] = raiseFromCompiledCode;

/**
 * Takes a raise compiled code threw for an interpreter run to perform.
 * @param {*} thrown - What the run caught.
 * @returns {RaiseNode|null} The raise to run in its place, or null if `thrown`
 *   is anything else.
 */
export function takeCompiledRaise(thrown) {
    if (!compiledRaises.has(thrown)) return null;
    const exception = compiledRaises.get(thrown);
    compiledRaises.delete(thrown);
    return new RaiseNode(exception, false);
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

        // For continuable: push a resume frame that allows handler return value
        if (this.continuable) {
            fstack.push(FrameRegistry.createRaiseContinuableResumeFrame(
                this.savedFrames,
                registers[ENV]
            ));
        } else {
            // For non-continuable: if handler returns, R7RS requires raising a secondary exception.
            fstack.push(new NonContinuableFrame());
        }

        // Invoke handler with the exception
        registers[CTL] = new TailAppNode(ensureExecutable(this.handler), [new LiteralNode(this.exception)]);
        return true;
    }
}

/**
 * Frame that traps return from a non-continuable exception handler.
 * Raises a secondary exception.
 */
class NonContinuableFrame {
    step(registers, interpreter) {
        // If we reached here, the handler returned, which is forbidden for 'raise'
        throw new SchemeError("non-continuable exception: handler returned");
    }
}
