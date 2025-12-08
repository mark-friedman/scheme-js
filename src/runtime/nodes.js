/**
 * AST Node Classes for the Scheme interpreter.
 * 
 * These represent the Abstract Syntax Tree nodes produced by the Analyzer.
 * Each node implements the `step()` method for the trampoline executor.
 */

import { Closure, Continuation, TailCall, ContinuationUnwind } from './values.js';

// --- Base Class ---

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

// --- Atomic AST Nodes ---

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
        registers[0] = this.value; // Put value in 'ans'
        return false; // Halt (value return)
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
        const env = registers[2];
        registers[0] = env.lookup(this.name); // Look up value, put in 'ans'
        return false; // Halt (value return)
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
     */
    constructor(params, body) {
        super();
        this.params = params; // Array of parameter names (strings)
        this.body = body;
    }

    step(registers, interpreter) {
        // Create a closure, capturing the current environment
        registers[0] = new Closure(this.params, this.body, registers[2]);
        return false; // Halt (value return)
    }

    toString() { return `(Lambda (${this.params.join(' ')}) ...`; }
}

// --- Complex AST Nodes ---
// These nodes push frames onto the stack and continue execution.
// Frame imports are handled via the shared registry to avoid circular dependencies.

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
        // Push the LetFrame via registry to avoid circular import
        registers[3].push(FrameRegistry.createLetFrame(
            this.varName,
            this.body,
            registers[2]
        ));

        registers[1] = this.binding;
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
        const newEnv = registers[2].extend(this.varName, null);

        registers[3].push(FrameRegistry.createLetRecFrame(
            this.varName,
            this.body,
            newEnv
        ));

        registers[1] = this.lambdaExpr;
        registers[2] = newEnv;
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
        registers[3].push(FrameRegistry.createIfFrame(
            this.consequent,
            this.alternative,
            registers[2]
        ));

        registers[1] = this.test;
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
        registers[3].push(FrameRegistry.createSetFrame(
            this.name,
            registers[2]
        ));

        registers[1] = this.valueExpr;
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
        registers[3].push(FrameRegistry.createDefineFrame(
            this.name,
            registers[2]
        ));

        registers[1] = this.valueExpr;
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

        registers[3].push(FrameRegistry.createAppFrame(
            remainingExprs,
            [],
            registers[2]
        ));

        registers[1] = firstExpr;
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
        const continuation = new Continuation(registers[3]);

        registers[1] = new TailApp(
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
            registers[0] = null;
            return false;
        }

        const firstExpr = this.expressions[0];
        const remainingExprs = this.expressions.slice(1);

        if (remainingExprs.length > 0) {
            registers[3].push(FrameRegistry.createBeginFrame(
                remainingExprs,
                registers[2]
            ));
        }

        registers[1] = firstExpr;
        return true;
    }
}

// --- Dynamic-Wind AST Nodes ---

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
        registers[3].push(FrameRegistry.createDynamicWindSetupFrame(
            this.before,
            this.thunk,
            this.after,
            registers[2]
        ));

        registers[1] = new TailApp(new Literal(this.before), []);
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
        registers[3] = [...this.targetStack];
        registers[0] = this.value;
        return false;
    }
}

// --- Multiple Values AST Nodes ---

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
        // Push a frame to handle the consumer call after producer returns
        registers[3].push(FrameRegistry.createCallWithValuesFrame(
            this.consumer,
            registers[2]
        ));

        // Call producer with no arguments
        registers[1] = new TailApp(new Literal(this.producer), []);
        return true;
    }

    toString() { return "(CallWithValues ...)"; }
}

