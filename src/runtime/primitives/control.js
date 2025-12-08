import { TailCall, Values } from '../values.js';
import { TailApp, Literal, DynamicWindInit, CallWithValuesNode } from '../ast.js';
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

    /**
     * call-with-values: (call-with-values producer consumer)
     * 
     * Calls producer with no arguments, then passes its return value(s)
     * to consumer. If producer returns a Values object, unpack it.
     */
    const callWithValuesPrimitive = (producer, consumer) => {
        // Return a TailCall to execute producer, then apply consumer
        return new TailCall(
            new CallWithValuesNode(producer, consumer),
            null
        );
    };
    callWithValuesPrimitive.skipBridge = true;

    const controlPrimitives = {
        'apply': applyPrimitive,

        /**
         * values: Return multiple values.
         * (values) -> Returns undefined (single value)
         * (values x) -> Returns x (single value)
         * (values x y ...) -> Returns Values wrapper
         */
        'values': (...args) => {
            if (args.length === 0) {
                // No values - return undefined
                return undefined;
            } else if (args.length === 1) {
                // Single value - return as-is
                return args[0];
            } else {
                // Multiple values - wrap in Values
                return new Values(args);
            }
        },

        'call-with-values': callWithValuesPrimitive,

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

