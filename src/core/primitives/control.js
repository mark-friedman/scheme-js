/**
 * Control Primitives for Scheme.
 * 
 * Provides control flow operations including apply, eval, dynamic-wind, and values.
 */

import { TailCall, Values, Closure, Continuation } from '../interpreter/values.js';
import { TailAppNode, LiteralNode, DynamicWindInit, CallWithValuesNode, CallCCNode } from '../interpreter/ast.js';
import { analyze } from '../interpreter/analyzer.js';
import { Cons, toArray } from '../interpreter/cons.js';
import { assertProcedure, assertArity, assertList } from '../interpreter/type_check.js';
import { SchemeTypeError } from '../interpreter/errors.js';

/**
 * Returns control primitives.
 * @param {Interpreter} interpreter - The interpreter instance.
 * @returns {Object} Map of primitive names to functions.
 */
export function getControlPrimitives(interpreter) {
    /**
     * apply: Apply a procedure to a list of arguments.
     * (apply proc arg1 ... args)
     */
    const applyPrimitive = (proc, ...args) => {
        if (args.length === 0) {
            throw new SchemeTypeError('apply', 2, 'list', undefined);
        }

        // The last argument must be a list
        const lastArg = args.pop();
        let finalArgs = args;

        if (lastArg instanceof Cons) {
            finalArgs = finalArgs.concat(toArray(lastArg));
        } else if (lastArg === null) {
            // Empty list, do nothing
        } else {
            throw new SchemeTypeError('apply', args.length + 2, 'list', lastArg);
        }

        // Wrap args in Literals for the AST
        const argLiterals = finalArgs.map(val => new LiteralNode(val));

        // Return a TailCall to transfer control
        return new TailCall(
            new TailAppNode(new LiteralNode(proc), argLiterals),
            null // Use current environment
        );
    };

    // CRITICAL: Tell AppFrame NOT to wrap the first argument (proc) in a bridge.
    applyPrimitive.skipBridge = true;

    /**
     * call-with-values: (call-with-values producer consumer)
     */
    const callWithValuesPrimitive = (producer, consumer) => {
        assertProcedure('call-with-values', 1, producer);
        assertProcedure('call-with-values', 2, consumer);
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
         */
        'values': (...args) => {
            if (args.length === 0) {
                return undefined;
            } else if (args.length === 1) {
                return args[0];
            } else {
                return new Values(args);
            }
        },

        'call-with-values': callWithValuesPrimitive,

        /**
         * eval: Evaluate an expression in an environment.
         */
        'eval': (expr, env) => {
            const ast = analyze(expr);
            return new TailCall(ast, env);
        },

        /**
         * interaction-environment: Returns the global environment.
         */
        'interaction-environment': () => {
            if (!interpreter.globalEnv) {
                throw new Error("interaction-environment: global environment not set");
            }
            return interpreter.globalEnv;
        },

        /**
         * null-environment: Returns a specifier for the environment that is 
         * empty except for bindings for standard syntactic keywords.
         * For R5RS compatibility.
         */
        'null-environment': (version) => {
            // R7RS only requires this for version 5 (R5RS)
            if (version !== 5) {
                throw new Error(`null-environment: unsupported version ${version}`);
            }
            // Return the global environment for now - proper implementation
            // would filter to only syntax keywords
            if (!interpreter.globalEnv) {
                throw new Error("null-environment: global environment not set");
            }
            return interpreter.globalEnv;
        },

        /**
         * dynamic-wind: Install before/after thunks.
         */
        'dynamic-wind': (before, thunk, after) => {
            assertProcedure('dynamic-wind', 1, before);
            assertProcedure('dynamic-wind', 2, thunk);
            assertProcedure('dynamic-wind', 3, after);
            return new TailCall(
                new DynamicWindInit(before, thunk, after),
                null
            );
        },

        /**
         * call-with-current-continuation: Capture the current continuation.
         */
        'call-with-current-continuation': (proc) => {
            assertProcedure('call-with-current-continuation', 1, proc);
            return new TailCall(new CallCCNode(new LiteralNode(proc)), null);
        },

        'call/cc': (proc) => {
            assertProcedure('call/cc', 1, proc);
            return new TailCall(new CallCCNode(new LiteralNode(proc)), null);
        },

        /**
         * procedure?: Type predicate for procedures.
         */
        'procedure?': (obj) => {
            return typeof obj === 'function' ||
                obj instanceof Closure ||
                obj instanceof Continuation;
        }
    };

    // Allow raw Closures for dynamic-wind components
    controlPrimitives['dynamic-wind'].skipBridge = true;
    controlPrimitives['values'].skipBridge = true;
    controlPrimitives['procedure?'].skipBridge = true;
    controlPrimitives['eval'].skipBridge = true;
    controlPrimitives['call/cc'].skipBridge = true;
    controlPrimitives['call-with-current-continuation'].skipBridge = true;
    controlPrimitives['interaction-environment'].skipBridge = true;
    controlPrimitives['null-environment'].skipBridge = true;

    return controlPrimitives;
}
