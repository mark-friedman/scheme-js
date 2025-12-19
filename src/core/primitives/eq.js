/**
 * Equality Primitives for Scheme.
 * 
 * Provides eq?, eqv?, and boolean operations.
 */

import { assertBoolean, assertArity } from '../interpreter/type_check.js';

/**
 * Equality primitives exported to Scheme.
 */
export const eqPrimitives = {
    /**
     * Identity comparison.
     * @param {*} a - First value.
     * @param {*} b - Second value.
     * @returns {boolean} True if a and b are the same object.
     */
    'eq?': (a, b) => a === b,

    /**
     * Equivalence comparison (handles NaN, -0).
     * @param {*} a - First value.
     * @param {*} b - Second value.
     * @returns {boolean} True if a and b are equivalent.
     */
    'eqv?': (a, b) => Object.is(a, b),

    /**
     * Boolean negation.
     * @param {*} obj - Value to negate.
     * @returns {boolean} True only if obj is #f.
     */
    'not': (obj) => obj === false,

    /**
     * Boolean type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is #t or #f.
     */
    'boolean?': (obj) => typeof obj === 'boolean',

    /**
     * Boolean equivalence. Returns true if all boolean arguments are equal.
     * @param {...boolean} args - Booleans to compare.
     * @returns {boolean} True if all arguments are the same boolean value.
     */
    'boolean=?': (...args) => {
        assertArity('boolean=?', args, 2, Infinity);
        args.forEach((arg, i) => assertBoolean('boolean=?', i + 1, arg));
        return args.every(b => b === args[0]);
    },

    /**
     * Symbol type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a symbol.
     */
    'symbol?': (obj) => obj !== null && typeof obj === 'object' && obj.constructor && obj.constructor.name === 'Symbol'
};
