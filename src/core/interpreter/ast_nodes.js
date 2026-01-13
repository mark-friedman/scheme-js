/**
 * AST Node Classes for the Scheme interpreter.
 * 
 * This module contains all AST (Abstract Syntax Tree) node classes.
 * AST nodes are produced by the Analyzer and represent program structure.
 * All nodes extend Executable and implement a step() method.
 */

import { Executable, ANS, CTL, ENV, FSTACK } from './stepables_base.js';
import { createClosure, createContinuation, isSchemeClosure, isSchemeContinuation } from './values.js';
import * as FrameRegistry from './frame_registry.js';
import { GlobalRef, lookupLibraryEnv } from './syntax_object.js';
import { SchemeError } from './errors.js';

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
                        const libEnv = lookupLibraryEnv(resolved.scope);
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
 * A lambda expression.
 * Creates a closure capturing the current environment.
 */
export class LambdaNode extends Executable {
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
        registers[ANS] = createClosure(this.params, this.body, registers[ENV], this.restParam, interpreter);
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
export class CallCCNode extends Executable {
    /**
     * @param {Executable} lambdaExpr - Should be a (lambda (k) ...) expression.
     */
    constructor(lambdaExpr) {
        super();
        this.lambdaExpr = lambdaExpr;
    }

    step(registers, interpreter) {
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
 * AST Node for top-level import.
 * Populates the environment with exports from a loaded library.
 * Libraries must be loaded into the registry first.
 */
export class ImportNode extends Executable {
    /**
     * @param {Array<object>} importSpecs - Array of { libraryName, only, except, rename, prefix }
     * @param {Function} loadLibrary - Function to load a library recursively (sync or async depending on usage)
     * @param {Function} applyImports - Function to apply imports to an environment
     * @param {Function} analyze - Analyze function for loading libraries
     */
    constructor(importSpecs, loadLibrary, applyImports, analyze) {
        super();
        this.importSpecs = importSpecs;
        this.loadLibrary = loadLibrary;
        this.applyImports = applyImports;
        this.analyze = analyze;
    }

    step(registers, interpreter) {
        const env = registers[ENV];
        for (const spec of this.importSpecs) {
            // Load the library (synchronously if loadLibrary is sync)
            const exports = this.loadLibrary(spec.libraryName, this.analyze, interpreter, env);
            this.applyImports(env, exports, spec);
        }
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
     * @param {Object} libDef - Parsed library definition
     * @param {Function} evaluateLibraryDefinition - Function to evaluate and register library
     * @param {Function} analyze - Analyze function
     */
    constructor(libDef, evaluateLibraryDefinition, analyze) {
        super();
        this.libDef = libDef;
        this.evaluateLibraryDefinition = evaluateLibraryDefinition;
        this.analyze = analyze;
    }

    step(registers, interpreter) {
        const env = registers[ENV];
        // Evaluate the library (synchronously if evaluateLibraryDefinition is sync)
        this.evaluateLibraryDefinition(this.libDef, this.analyze, interpreter, env);
        registers[ANS] = true; // Returns true/unspecified
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
            const exc = this.exception;
            if (exc instanceof Error) {
                throw exc;
            }
            throw new SchemeError(`Unhandled exception: ${exc}`, [exc]);
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
