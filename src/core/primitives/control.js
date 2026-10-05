/**
 * Control Primitives for Scheme.
 * 
 * Provides control flow operations including apply, eval, dynamic-wind, and values.
 */

import { TailCall, Values, NO_VALUES, isSchemeClosure, isSchemeContinuation, Closure, Continuation } from '../interpreter/values.js';
import { TailAppNode, LiteralNode, DynamicWindInit, CallWithValuesNode, CallCCNode } from '../interpreter/ast.js';
import { analyze } from '../interpreter/expand.js';
import { assertProcedure, assertArity, assertList } from '../interpreter/type_check.js';
import { SchemeTypeError } from '../interpreter/errors.js';
import { applyProcedure, valuesToList } from './apply.js';
import { globalContext } from '../interpreter/context.js';
import { importEnvironment } from '../interpreter/library_loader.js';

/**
 * Returns control primitives.
 * @param {Interpreter} interpreter - The interpreter instance.
 * @returns {Object} Map of primitive names to functions.
 */
export function getControlPrimitives(interpreter) {

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
        'apply': applyProcedure,

        /**
         * values: Return multiple values.
         */
        'values': (...args) => {
            if (args.length === 0) {
                // No values, which a consumer receives as no arguments: not
                // the unspecified value, which is one.
                return NO_VALUES;
            } else if (args.length === 1) {
                return args[0];
            } else {
                return new Values(args);
            }
        },

        'call-with-values': callWithValuesPrimitive,

        '%values->list': valuesToList,

        /**
         * eval: Evaluate an expression in an environment. One that has a
         * scope of its own -- what `environment` makes -- has the expression
         * analyzed under it, so that the keywords imported into it are found.
         */
        'eval': (expr, env) => {
            const scope = env?.libraryScope;
            if (scope === undefined) return new TailCall(analyze(expr), env);
            globalContext.pushDefiningScope(scope);
            try {
                return new TailCall(analyze(expr), env);
            } finally {
                globalContext.popDefiningScope();
            }
        },

        /**
         * The environment `environment` returns (R7RS 6.12): a new one, with
         * the import sets imported into it by the library system, as an
         * `import` form's are. It holds what they import and nothing else.
         */
        '%import-environment': (sets) => importEnvironment(sets, analyze, interpreter, interpreter.globalEnv),

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
