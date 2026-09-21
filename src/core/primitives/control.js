/**
 * Control Primitives for Scheme.
 * 
 * Provides control flow operations including apply, eval, dynamic-wind, and values.
 */

import { TailCall, Values, isSchemeClosure, isSchemeContinuation, Closure, Continuation } from '../interpreter/values.js';
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

        // A `TailCall` naming the procedure and its arguments, rather than one
        // carrying an expression for the interpreter to evaluate. Both shapes
        // are accepted by `continueApplication`, but only this one can be
        // continued by *compiled* code, whose trampoline calls the procedure
        // directly and has no evaluator to hand an AST node to.
        //
        // That is the whole reason `apply` used to be off limits to the
        // compiler, and it was the single largest cause of declined procedures
        // in the benchmark corpus -- reached mostly through `map` and
        // `for-each`, which use it for their variadic case.
        return new TailCall(proc, finalArgs);
    };

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
         * %values->list: The values a producer returned, as a list.
         *
         * Exists for compiled code, which cannot use `call-with-values`: that
         * primitive hands the interpreter an expression to evaluate, and
         * compiled code has no evaluator. Given this, the compiler expresses
         * `(call-with-values p c)` as `(apply c (%values->list (p)))`, which is
         * built entirely from calls it already makes -- so the call to the
         * producer is an ordinary call site, with the resume point a captured
         * continuation needs.
         *
         * A result that is not a `Values` counts as exactly one value,
         * including the unspecified value, which is what `CallWithValuesFrame`
         * does and therefore what the two tiers have to agree on.
         */
        '%values->list': (result) => {
            const items = result instanceof Values ? result.toArray() : [result];
            let list = null;
            for (let i = items.length - 1; i >= 0; i--) list = new Cons(items[i], list);
            return list;
        },

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
            // Convert BigInt to Number for comparison
            const v = typeof version === 'bigint' ? Number(version) : version;
            if (v !== 5) {
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
         * Returns #t for Scheme closures, continuations, and JS functions.
         */
        'procedure?': (obj) => {
            // All callable functions are procedures
            // Scheme closures/continuations are now functions with markers
            return typeof obj === 'function' ||
                obj instanceof Closure ||
                obj instanceof Continuation;
        }
    };

    return controlPrimitives;
}
