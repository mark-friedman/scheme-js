/**
 * @fileoverview `apply`, `procedure?` and `values`, and `%values->list`, which
 * compiled code makes `call-with-values` of.
 *
 * Primitives like the others in control.js, kept in a module of their own so
 * that generated code can reach them through the runtime (src/compiler/
 * runtime.js) without the runtime importing all the control primitives, and
 * everything those import. Compiled code rewrites `(call-with-values p c)` as
 * a call to these two, which must be the primitives whatever the environment
 * it runs in binds under their names: a library sees only what it imports,
 * and need not import `apply` to use `call-with-values`.
 */

import { TailCall, Values, NO_VALUES, SCHEME_PRIMITIVE, Closure, Continuation } from '../interpreter/values.js';
import { Cons, toArray } from '../interpreter/cons.js';
import { SchemeTypeError } from '../interpreter/errors.js';

/**
 * apply: Apply a procedure to a list of arguments.
 * (apply proc arg1 ... args)
 * @param {Function} proc - The procedure.
 * @param {...*} args - Its arguments, the last a list of the rest.
 * @returns {TailCall} The call.
 */
export function applyProcedure(proc, ...args) {
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
}
applyProcedure[SCHEME_PRIMITIVE] = true;

/**
 * %values->list: The values a producer returned, as a list.
 *
 * Exists for compiled code, which cannot use `call-with-values`: that
 * primitive hands the interpreter an expression to evaluate, and compiled code
 * has no evaluator. Given this, the compiler expresses `(call-with-values p
 * c)` as `(apply c (%values->list (p)))`, which is built entirely from calls
 * it already makes -- so the call to the producer is an ordinary call site,
 * with the resume point a captured continuation needs.
 *
 * A result that is not a `Values` counts as exactly one value, including the
 * unspecified value, which is what `CallWithValuesFrame` does and therefore
 * what the two tiers have to agree on.
 * @param {*} result - What the producer returned.
 * @returns {Cons|null} The values, as a list.
 */
export function valuesToList(result) {
    const items = result instanceof Values ? result.toArray() : [result];
    let list = null;
    for (let i = items.length - 1; i >= 0; i--) list = new Cons(items[i], list);
    return list;
}
valuesToList[SCHEME_PRIMITIVE] = true;

/**
 * The primitives about procedures and their values that need nothing of the
 * interpreter, so that compiled code has them without it: `apply`;
 * `procedure?`, true of Scheme closures, continuations and JavaScript
 * functions alike; `values`; and `%values->list`.
 * @type {Object<string, Function>}
 */
export const procedurePrimitives = {
    'apply': applyProcedure,
    'procedure?': (obj) => typeof obj === 'function' || obj instanceof Closure || obj instanceof Continuation,

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

    '%values->list': valuesToList
};
