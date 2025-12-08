import { TailCall } from '../values.js';
import { TailApp, Literal, DynamicWindInit } from '../ast.js';
import { analyze } from '../analyzer.js';
import { Cons, toArray } from '../cons.js';

export function getControlPrimitives(interpreter) {
    const applyPrimitive = (proc, ...args) => {
        if (args.length === 0) {
            throw new Error("apply: expects at least 1 argument");
        }

        // The last argument must be a list
        const lastArg = args.pop();
        let finalArgs = args;

        if (lastArg instanceof Cons) {
            finalArgs = finalArgs.concat(toArray(lastArg));
        } else if (lastArg === null) {
            // Empty list, do nothing
        } else {
            throw new Error(`apply: last argument must be a list, got ${lastArg}`);
        }

        // Wrap args in Literals for the AST
        const argLiterals = finalArgs.map(val => new Literal(val));

        // Return a TailCall to transfer control
        return new TailCall(
            new TailApp(new Literal(proc), argLiterals),
            null // Use current environment
        );
    };

    // CRITICAL: Tell AppFrame NOT to wrap the first argument (proc) in a bridge.
    // We want the raw Closure/Continuation so we can put it back into the AST.
    applyPrimitive.skipBridge = true;

    const controlPrimitives = {
        'apply': applyPrimitive,

        'eval': (expr, env) => {
            // 1. Analyze the expression to get an AST
            const ast = analyze(expr);

            // 2. Return a TailCall to execute the AST in the given environment
            return new TailCall(ast, env);
        },

        'interaction-environment': () => {
            if (!interpreter.globalEnv) {
                throw new Error("interaction-environment: global environment not set");
            }
            return interpreter.globalEnv;
        },

        'dynamic-wind': (before, thunk, after) => {
            // Return a TailCall to DynamicWindInit, which has access to the stack
            return new TailCall(
                new DynamicWindInit(before, thunk, after),
                null
            );
        }
    };

    // Allow raw Closures for dynamic-wind components
    controlPrimitives['dynamic-wind'].skipBridge = true;

    return controlPrimitives;
}
