/**
 * Equality Primitives for Scheme.
 * 
 * Provides eq?, eqv?, and boolean operations.
 */

import { assertBoolean, assertArity } from '../interpreter/type_check.js';
import { Complex } from './complex.js';
import { Rational } from './rational.js';
import { Flonum } from '../interpreter/number_representation.js';
import { Char } from './char_class.js';
import { Symbol } from '../interpreter/symbol.js';

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
        // Each exact integer and each unboxed inexact real has one
        // representation (number_representation.js), so Object.is compares
        // them, NaN with NaN; an exact integer is never eqv? to an inexact
        // one, which is boxed: (eqv? 5 5.0) => #f.
        if (Object.is(a, b)) return true;

        // Inexact integers, boxed, by value, -0.0 apart from 0.0.
        if (a instanceof Flonum && b instanceof Flonum) return Object.is(a.value, b.value);

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
    'symbol?': (obj) => obj instanceof Symbol,

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
