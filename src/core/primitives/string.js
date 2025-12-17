/**
 * String Primitives for Scheme.
 * 
 * Provides string operations for the Scheme runtime.
 */

import { intern, Symbol } from '../interpreter/symbol.js';
import { assertString, assertSymbol, assertNumber, assertArity } from '../interpreter/type_check.js';

/**
 * String primitives exported to Scheme.
 */
export const stringPrimitives = {
    /**
     * String type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a string.
     */
    'string?': (obj) => typeof obj === 'string',

    /**
     * Concatenates strings.
     * @param {...string} args - Strings to append.
     * @returns {string} Concatenated string.
     */
    'string-append': (...args) => {
        args.forEach((arg, i) => assertString('string-append', i + 1, arg));
        return args.join('');
    },

    /**
     * Converts number to string.
     * @param {number} num - Number to convert.
     * @returns {string} String representation.
     */
    'number->string': (num) => {
        assertNumber('number->string', 1, num);
        return String(num);
    },

    /**
     * Converts string to symbol.
     * @param {string} str - String to convert.
     * @returns {Symbol} Interned symbol.
     */
    'string->symbol': (str) => {
        assertString('string->symbol', 1, str);
        return intern(str);
    },

    /**
     * Converts symbol to string.
     * @param {Symbol} sym - Symbol to convert.
     * @returns {string} Symbol's name.
     */
    'symbol->string': (sym) => {
        assertSymbol('symbol->string', 1, sym);
        return sym.name;
    }
};
