/**
 * Equality Primitives for Scheme.
 * 
 * Provides eq?, eqv?, and boolean operations.
 */

import { assertBoolean, assertArity } from '../interpreter/type_check.js';
import { Complex } from './complex.js';
import { Rational } from './rational.js';
import { Char } from './char_class.js';

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
     * Equivalence comparison (handles NaN, -0, Complex, Rational, BigInt).
     * R7RS: eqv? distinguishes exact from inexact numbers.
     * @param {*} a - First value.
     * @param {*} b - Second value.
     * @returns {boolean} True if a and b are equivalent.
     */
    'eqv?': (a, b) => {
        if (Object.is(a, b)) return true;

        // Exact (BigInt) is not eqv? to inexact (Number) even if values match
        // e.g., (eqv? 5 5.0) => #f
        if (typeof a === 'bigint' && typeof b === 'number') return false;
        if (typeof a === 'number' && typeof b === 'bigint') return false;

        // BigInt comparison
        if (typeof a === 'bigint' && typeof b === 'bigint') {
            return a === b;
        }

        if (a instanceof Complex && b instanceof Complex) {
            return eqPrimitives['eqv?'](a.real, b.real) && eqPrimitives['eqv?'](a.imag, b.imag);
        }

        if (a instanceof Rational) {
            if (b instanceof Rational) {
                return a.numerator === b.numerator &&
                    a.denominator === b.denominator &&
                    a.exact === b.exact;  // exactness matters for eqv?
            }
            // Rational with denominator 1 vs BigInt
            if (typeof b === 'bigint' && a.denominator === 1n && a.exact) {
                return a.numerator === b;
            }
        }

        if (b instanceof Rational) {
            // BigInt vs Rational with denominator 1
            if (typeof a === 'bigint' && b.denominator === 1n && b.exact) {
                return b.numerator === a;
            }
        }

        if (a instanceof Char && b instanceof Char) {
            return a.codePoint === b.codePoint;
        }

        // Symbols are interned, so Object.is already covers them.

        return false;
    },

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
    'symbol?': (obj) => obj !== null && typeof obj === 'object' && obj.constructor && obj.constructor.name === 'Symbol',

    /**
     * Symbol equality. Returns true if all arguments are the same symbol.
     * @param {...Symbol} args - Symbols to compare.
     * @returns {boolean} True if all arguments are eq?.
     */
    'symbol=?': (...args) => {
        assertArity('symbol=?', args, 2, Infinity);
        if (args.length === 0) return true;
        const first = args[0];
        if (!(first !== null && typeof first === 'object' && first.constructor && first.constructor.name === 'Symbol')) {
            throw new Error('symbol=?: expected symbol');
        }
        return args.every(sym => {
            if (!(sym !== null && typeof sym === 'object' && sym.constructor && sym.constructor.name === 'Symbol')) {
                throw new Error('symbol=?: expected symbol');
            }
            return sym === first;
        });
    }
};

// Mark primitives that should receive raw Scheme objects (no JS bridge wrapping)
eqPrimitives['eq?'].skipBridge = true;
eqPrimitives['eqv?'].skipBridge = true;
