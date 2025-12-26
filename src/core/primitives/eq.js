/**
 * Equality Primitives for Scheme.
 * 
 * Provides eq?, eqv?, and boolean operations.
 */

import { assertBoolean, assertArity } from '../interpreter/type_check.js';
import { Complex } from './complex.js';
import { Rational } from './rational.js';

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
     * Equivalence comparison (handles NaN, -0, Complex, Rational).
     * @param {*} a - First value.
     * @param {*} b - Second value.
     * @returns {boolean} True if a and b are equivalent.
     */
    'eqv?': (a, b) => {
        if (Object.is(a, b)) return true;

        if (a instanceof Complex && b instanceof Complex) {
            return a.real === b.real && a.imag === b.imag;
        }

        if (a instanceof Rational && b instanceof Rational) {
            return a.numerator === b.numerator && a.denominator === b.denominator;
        }

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
