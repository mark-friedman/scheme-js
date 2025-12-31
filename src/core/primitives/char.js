/**
 * Character Primitives for Scheme.
 * 
 * Provides character operations per R7RS §6.6.
 * Characters are represented as single-character JavaScript strings.
 */

import {
    assertChar,
    assertInteger,
    assertArity,
    isChar
} from '../interpreter/type_check.js';
import { Char } from './char_class.js';
import { SchemeRangeError } from '../interpreter/errors.js';

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Asserts all arguments are characters.
 * @param {string} procName - Procedure name for error messages
 * @param {Array} args - Arguments to check
 */
function assertAllChars(procName, args) {
    args.forEach((arg, i) => assertChar(procName, i + 1, arg));
}

/**
 * Variadic character comparison helper.
 * @param {string} procName - Procedure name
 * @param {Function} compare - Comparison function (a, b) => boolean
 * @param {Array} args - Character arguments
 * @returns {boolean} True if comparison holds for all adjacent pairs
 */
function compareChars(procName, compare, args) {
    assertArity(procName, args, 2, Infinity);
    assertAllChars(procName, args);
    for (let i = 0; i < args.length - 1; i++) {
        if (!compare(args[i].toString(), args[i + 1].toString())) return false;
    }
    return true;
}

/**
 * Variadic case-insensitive character comparison helper.
 * @param {string} procName - Procedure name
 * @param {Function} compare - Comparison function (a, b) => boolean
 * @param {Array} args - Character arguments
 * @returns {boolean} True if comparison holds for all adjacent pairs
 */
function compareCiChars(procName, compare, args) {
    assertArity(procName, args, 2, Infinity);
    assertAllChars(procName, args);
    for (let i = 0; i < args.length - 1; i++) {
        const a = args[i].toString().toLowerCase();
        const b = args[i + 1].toString().toLowerCase();
        if (!compare(a, b)) return false;
    }
    return true;
}

// =============================================================================
// Character Primitives
// =============================================================================

/**
 * Character primitives exported to Scheme.
 */
export const charPrimitives = {
    // -------------------------------------------------------------------------
    // Type Predicate
    // -------------------------------------------------------------------------

    /**
     * Character type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a character.
     */
    'char?': (obj) => isChar(obj),

    // -------------------------------------------------------------------------
    // Case-Sensitive Comparison
    // -------------------------------------------------------------------------

    /**
     * Returns #t if all characters are equal.
     * @param {...string} chars - Characters to compare.
     * @returns {boolean}
     */
    'char=?': (...args) => compareChars('char=?', (a, b) => a === b, args),

    /**
     * Returns #t if characters are monotonically increasing.
     * @param {...string} chars - Characters to compare.
     * @returns {boolean}
     */
    'char<?': (...args) => compareChars('char<?', (a, b) => a < b, args),

    /**
     * Returns #t if characters are monotonically decreasing.
     * @param {...string} chars - Characters to compare.
     * @returns {boolean}
     */
    'char>?': (...args) => compareChars('char>?', (a, b) => a > b, args),

    /**
     * Returns #t if characters are monotonically non-decreasing.
     * @param {...string} chars - Characters to compare.
     * @returns {boolean}
     */
    'char<=?': (...args) => compareChars('char<=?', (a, b) => a <= b, args),

    /**
     * Returns #t if characters are monotonically non-increasing.
     * @param {...string} chars - Characters to compare.
     * @returns {boolean}
     */
    'char>=?': (...args) => compareChars('char>=?', (a, b) => a >= b, args),

    // -------------------------------------------------------------------------
    // Case-Insensitive Comparison
    // -------------------------------------------------------------------------

    /**
     * Case-insensitive char=?.
     * @param {...string} chars - Characters to compare.
     * @returns {boolean}
     */
    'char-ci=?': (...args) => compareCiChars('char-ci=?', (a, b) => a === b, args),

    /**
     * Case-insensitive char<?.
     * @param {...string} chars - Characters to compare.
     * @returns {boolean}
     */
    'char-ci<?': (...args) => compareCiChars('char-ci<?', (a, b) => a < b, args),

    /**
     * Case-insensitive char>?.
     * @param {...string} chars - Characters to compare.
     * @returns {boolean}
     */
    'char-ci>?': (...args) => compareCiChars('char-ci>?', (a, b) => a > b, args),

    /**
     * Case-insensitive char<=?.
     * @param {...string} chars - Characters to compare.
     * @returns {boolean}
     */
    'char-ci<=?': (...args) => compareCiChars('char-ci<=?', (a, b) => a <= b, args),

    /**
     * Case-insensitive char>=?.
     * @param {...string} chars - Characters to compare.
     * @returns {boolean}
     */
    'char-ci>=?': (...args) => compareCiChars('char-ci>=?', (a, b) => a >= b, args),

    // -------------------------------------------------------------------------
    // Character Class Predicates
    // -------------------------------------------------------------------------

    /**
     * Returns #t if char is alphabetic.
     * @param {string} char - Character to test.
     * @returns {boolean}
     */
    'char-alphabetic?': (char) => {
        assertChar('char-alphabetic?', 1, char);
        return /^[a-zA-Z]$/.test(char.toString());
    },

    /**
     * Returns #t if char is numeric (0-9).
     * @param {string} char - Character to test.
     * @returns {boolean}
     */
    'char-numeric?': (char) => {
        assertChar('char-numeric?', 1, char);
        return /^[0-9]$/.test(char.toString());
    },

    /**
     * Returns #t if char is whitespace.
     * @param {string} char - Character to test.
     * @returns {boolean}
     */
    'char-whitespace?': (char) => {
        assertChar('char-whitespace?', 1, char);
        return /^\s$/.test(char.toString());
    },

    /**
     * Returns #t if char is uppercase.
     * @param {string} char - Character to test.
     * @returns {boolean}
     */
    'char-upper-case?': (char) => {
        assertChar('char-upper-case?', 1, char);
        const s = char.toString();
        return s === s.toUpperCase() && s !== s.toLowerCase();
    },

    /**
     * Returns #t if char is lowercase.
     * @param {string} char - Character to test.
     * @returns {boolean}
     */
    'char-lower-case?': (char) => {
        assertChar('char-lower-case?', 1, char);
        const s = char.toString();
        return s === s.toLowerCase() && s !== s.toUpperCase();
    },

    // -------------------------------------------------------------------------
    // Conversion
    // -------------------------------------------------------------------------

    /**
     * Returns the Unicode code point of a character.
     * @param {string} char - Character.
     * @returns {number} Code point.
     */
    'char->integer': (char) => {
        assertChar('char->integer', 1, char);
        return BigInt(char.valueOf());
    },

    /**
     * Returns the character for a Unicode code point.
     * @param {number} n - Code point.
     * @returns {string} Character.
     */
    'integer->char': (n) => {
        assertInteger('integer->char', 1, n);
        const code = Number(n);
        if (code < 0 || code > 0x10FFFF) {
            throw new SchemeRangeError('integer->char', 'code point', 0, 0x10FFFF, n);
        }
        return new Char(code);
    },

    // -------------------------------------------------------------------------
    // Case Conversion
    // -------------------------------------------------------------------------

    /**
     * Returns the uppercase version of a character.
     * @param {string} char - Character.
     * @returns {string} Uppercase character.
     */
    'char-upcase': (char) => {
        assertChar('char-upcase', 1, char);
        return new Char(char.toString().toUpperCase().codePointAt(0));
    },

    /**
     * Returns the lowercase version of a character.
     * @param {string} char - Character.
     * @returns {string} Lowercase character.
     */
    'char-downcase': (char) => {
        assertChar('char-downcase', 1, char);
        return new Char(char.toString().toLowerCase().codePointAt(0));
    },

    /**
     * Returns the case-folded version of a character.
     * For simple cases, this is the same as downcase.
     * @param {string} char - Character.
     * @returns {string} Folded character.
     */
    'char-foldcase': (char) => {
        assertChar('char-foldcase', 1, char);
        return new Char(char.toString().toLowerCase().codePointAt(0));
    },

    // -------------------------------------------------------------------------
    // Digit Value
    // -------------------------------------------------------------------------

    /**
     * Returns the digit value (0-9) if char is a digit, else #f.
     * R7RS: For radix 10, only characters 0-9 have digit values.
     * @param {string} char - Character.
     * @returns {number|boolean} Digit value or #f.
     */
    'digit-value': (char) => {
        assertChar('digit-value', 1, char);
        const code = char.valueOf();
        if (code >= 48 && code <= 57) { // '0' to '9'
            return BigInt(code - 48);
        }
        return false;
    }
};
