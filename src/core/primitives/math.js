/**
 * Math Primitives for Scheme.
 * 
 * Provides arithmetic and comparison operations for the Scheme runtime.
 */

import { assertNumber, assertArity } from '../interpreter/type_check.js';

/**
 * Math primitives exported to Scheme.
 */
export const mathPrimitives = {
    /**
     * Addition. Returns the sum of all arguments.
     * @param {...number} args - Numbers to add.
     * @returns {number} Sum of all arguments.
     */
    '+': (...args) => {
        args.forEach((arg, i) => assertNumber('+', i + 1, arg));
        return args.reduce((a, b) => a + b, 0);
    },

    /**
     * Subtraction. With one argument, returns negation.
     * With multiple, subtracts rest from first.
     * @param {number} first - First number.
     * @param {...number} rest - Numbers to subtract.
     * @returns {number} Difference.
     */
    '-': (first, ...rest) => {
        assertArity('-', [first, ...rest], 1, Infinity);
        assertNumber('-', 1, first);
        rest.forEach((arg, i) => assertNumber('-', i + 2, arg));
        if (rest.length === 0) return -first;
        return rest.reduce((a, b) => a - b, first);
    },

    /**
     * Multiplication. Returns the product of all arguments.
     * @param {...number} args - Numbers to multiply.
     * @returns {number} Product of all arguments.
     */
    '*': (...args) => {
        args.forEach((arg, i) => assertNumber('*', i + 1, arg));
        return args.reduce((a, b) => a * b, 1);
    },

    /**
     * Division. With one argument, returns reciprocal.
     * With multiple, divides first by rest.
     * @param {number} first - First number.
     * @param {...number} rest - Divisors.
     * @returns {number} Quotient.
     */
    '/': (first, ...rest) => {
        assertArity('/', [first, ...rest], 1, Infinity);
        assertNumber('/', 1, first);
        rest.forEach((arg, i) => assertNumber('/', i + 2, arg));
        if (rest.length === 0) return 1 / first;
        return rest.reduce((a, b) => a / b, first);
    },

    /**
     * Numeric equality.
     * @param {number} a - First number.
     * @param {number} b - Second number.
     * @returns {boolean} True if equal.
     */
    '=': (a, b) => {
        assertArity('=', [a, b], 2, 2);
        assertNumber('=', 1, a);
        assertNumber('=', 2, b);
        return a === b;
    },

    /**
     * Less than comparison.
     * @param {number} a - First number.
     * @param {number} b - Second number.
     * @returns {boolean} True if a < b.
     */
    '<': (a, b) => {
        assertArity('<', [a, b], 2, 2);
        assertNumber('<', 1, a);
        assertNumber('<', 2, b);
        return a < b;
    },

    /**
     * Greater than comparison.
     * @param {number} a - First number.
     * @param {number} b - Second number.
     * @returns {boolean} True if a > b.
     */
    '>': (a, b) => {
        assertArity('>', [a, b], 2, 2);
        assertNumber('>', 1, a);
        assertNumber('>', 2, b);
        return a > b;
    },

    /**
     * Modulo operation.
     * @param {number} a - Dividend.
     * @param {number} b - Divisor.
     * @returns {number} Remainder.
     */
    'modulo': (a, b) => {
        assertArity('modulo', [a, b], 2, 2);
        assertNumber('modulo', 1, a);
        assertNumber('modulo', 2, b);
        return a % b;
    },

    /**
     * Number type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a number.
     */
    'number?': (obj) => typeof obj === 'number',
};
