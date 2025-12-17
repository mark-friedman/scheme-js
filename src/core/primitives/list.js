/**
 * List Primitives for Scheme.
 * 
 * Provides basic pair and list operations for the Scheme runtime.
 */

import { Cons, cons, list, cadr, cddr, caddr, cdddr, cadddr } from '../interpreter/cons.js';
import { assertPair, assertList, assertArity, isList } from '../interpreter/type_check.js';
import { SchemeTypeError } from '../interpreter/errors.js';

/**
 * List primitives exported to Scheme.
 */
export const listPrimitives = {
    /**
     * Constructs a pair.
     * @param {*} a - Car element.
     * @param {*} d - Cdr element.
     * @returns {Cons} New pair.
     */
    'cons': cons,

    /**
     * cadr - (car (cdr x))
     */
    'cadr': cadr,

    /**
     * cddr - (cdr (cdr x))
     */
    'cddr': cddr,

    /**
     * caddr - (car (cdr (cdr x)))
     */
    'caddr': caddr,

    /**
     * cdddr - (cdr (cdr (cdr x)))
     */
    'cdddr': cdddr,

    /**
     * cadddr - (car (cdr (cdr (cdr x))))
     */
    'cadddr': cadddr,

    /**
     * Returns the car of a pair.
     * @param {Cons} p - A pair.
     * @returns {*} The car element.
     */
    'car': (p) => {
        assertPair('car', 1, p);
        return p.car;
    },

    /**
     * Returns the cdr of a pair.
     * @param {Cons} p - A pair.
     * @returns {*} The cdr element.
     */
    'cdr': (p) => {
        assertPair('cdr', 1, p);
        return p.cdr;
    },

    /**
     * Constructs a list from arguments.
     */
    'list': list,

    /**
     * Pair type predicate.
     * @param {*} p - Value to check.
     * @returns {boolean} True if p is a pair.
     */
    'pair?': (p) => p instanceof Cons,

    /**
     * Null check.
     * @param {*} p - Value to check.
     * @returns {boolean} True if p is null (empty list).
     */
    'null?': (p) => p === null,

    /**
     * List type predicate.
     * @param {*} p - Value to check.
     * @returns {boolean} True if p is a proper list.
     */
    'list?': (p) => isList(p),

    /**
     * Mutates the car of a pair.
     * @param {Cons} p - A pair.
     * @param {*} val - New value for car.
     * @returns {null} Unspecified.
     */
    'set-car!': (p, val) => {
        assertPair('set-car!', 1, p);
        p.car = val;
        return null; // R7RS: unspecified
    },

    /**
     * Mutates the cdr of a pair.
     * @param {Cons} p - A pair.
     * @param {*} val - New value for cdr.
     * @returns {null} Unspecified.
     */
    'set-cdr!': (p, val) => {
        assertPair('set-cdr!', 1, p);
        p.cdr = val;
        return null; // R7RS: unspecified
    },

    /**
     * Appends lists together.
     * @param {...*} args - Lists to append (last can be any value).
     * @returns {*} The concatenated list.
     */
    'append': (...args) => {
        if (args.length === 0) return null;
        if (args.length === 1) return args[0];

        // Append all but last, then attach last
        let result = args[args.length - 1];

        for (let i = args.length - 2; i >= 0; i--) {
            const lst = args[i];
            result = appendTwo(lst, result, i + 1);
        }
        return result;
    }
};

/**
 * Appends two lists (helper for n-ary append).
 * @param {Cons|null} list1 - First list (must be proper).
 * @param {*} list2 - Second list (can be improper tail).
 * @param {number} argPos - Argument position for error reporting.
 * @returns {Cons|*} The concatenated list.
 * @throws {SchemeTypeError} If list1 is not a proper list.
 */
function appendTwo(list1, list2, argPos) {
    if (list1 === null) return list2;
    if (!(list1 instanceof Cons)) {
        throw new SchemeTypeError('append', argPos, 'list', list1);
    }
    return new Cons(list1.car, appendTwo(list1.cdr, list2, argPos));
}
