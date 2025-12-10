/**
 * List Primitives for Scheme.
 * 
 * Provides basic pair and list operations for the Scheme runtime.
 */

import { Cons, cons, list } from '../interpreter/cons.js';

/**
 * List primitives exported to Scheme.
 */
export const listPrimitives = {
    'cons': cons,

    'car': (p) => {
        if (!(p instanceof Cons)) throw new Error(`car: expected pair, got ${p}`);
        return p.car;
    },

    'cdr': (p) => {
        if (!(p instanceof Cons)) throw new Error(`cdr: expected pair, got ${p}`);
        return p.cdr;
    },

    'list': list,

    'pair?': (p) => p instanceof Cons,

    'null?': (p) => p === null,

    'set-car!': (p, val) => {
        if (!(p instanceof Cons)) throw new Error(`set-car!: expected pair, got ${p}`);
        p.car = val;
        return null; // R7RS: unspecified
    },

    'set-cdr!': (p, val) => {
        if (!(p instanceof Cons)) throw new Error(`set-cdr!: expected pair, got ${p}`);
        p.cdr = val;
        return null; // R7RS: unspecified
    },

    'append': (...args) => {
        if (args.length === 0) return null;
        if (args.length === 1) return args[0];

        // Append all but last, then attach last
        let result = args[args.length - 1];

        for (let i = args.length - 2; i >= 0; i--) {
            const list = args[i];
            result = appendTwo(list, result);
        }
        return result;
    }
};

/**
 * Appends two lists (helper for n-ary append).
 * @param {Cons|null} list1 - First list (must be proper).
 * @param {*} list2 - Second list (can be improper tail).
 * @returns {Cons|*} The concatenated list.
 * @throws {Error} If list1 is not a proper list.
 */
function appendTwo(list1, list2) {
    if (list1 === null) return list2;
    if (!(list1 instanceof Cons)) {
        throw new Error(`append: expected proper list, got ${list1}`);
    }
    return new Cons(list1.car, appendTwo(list1.cdr, list2));
}
