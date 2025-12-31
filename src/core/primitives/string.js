/**
 * String Primitives for Scheme.
 * 
 * Provides string operations per R7RS §6.7.
 * Strings are JavaScript strings (immutable).
 */

import { intern, Symbol } from '../interpreter/symbol.js';
import { list, toArray } from '../interpreter/cons.js';
import { Values } from '../interpreter/values.js';
import { parseNumber } from '../interpreter/reader.js';
import { Char } from './char_class.js';
import {
    assertType,
    assertString,
    assertSymbol,
    assertNumber,
    assertInteger,
    assertChar,
    assertArity,
    isChar
} from '../interpreter/type_check.js';
import { SchemeRangeError, SchemeError } from '../interpreter/errors.js';
import { Complex } from './complex.js';
import { Rational } from './rational.js';

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Asserts all arguments are strings.
 * @param {string} procName - Procedure name for error messages
 * @param {Array} args - Arguments to check
 */
function assertAllStrings(procName, args) {
    args.forEach((arg, i) => assertString(procName, i + 1, arg));
}

/**
 * Asserts all arguments are characters.
 * @param {string} procName - Procedure name for error messages
 * @param {Array} args - Arguments to check
 */
function assertAllChars(procName, args) {
    args.forEach((arg, i) => assertChar(procName, i + 1, arg));
}

/**
 * Variadic string comparison helper.
 * @param {string} procName - Procedure name
 * @param {Function} compare - Comparison function (a, b) => boolean
 * @param {Array} args - String arguments
 * @returns {boolean} True if comparison holds for all adjacent pairs
 */
function compareStrings(procName, compare, args) {
    assertArity(procName, args, 2, Infinity);
    assertAllStrings(procName, args);
    for (let i = 0; i < args.length - 1; i++) {
        if (!compare(args[i], args[i + 1])) return false;
    }
    return true;
}

/**
 * Variadic case-insensitive string comparison helper.
 * @param {string} procName - Procedure name
 * @param {Function} compare - Comparison function (a, b) => boolean
 * @param {Array} args - String arguments
 * @returns {boolean} True if comparison holds for all adjacent pairs
 */
function compareCiStrings(procName, compare, args) {
    assertArity(procName, args, 2, Infinity);
    assertAllStrings(procName, args);
    for (let i = 0; i < args.length - 1; i++) {
        const a = args[i].toLowerCase();
        const b = args[i + 1].toLowerCase();
        if (!compare(a, b)) return false;
    }
    return true;
}

/**
 * Validates and returns start/end range for string operations.
 * @param {string} procName - Procedure name
 * @param {string} str - The string
 * @param {number|undefined} start - Start index (default 0)
 * @param {number|undefined} end - End index (default str.length)
 * @returns {[number, number]} Validated [start, end]
 */
function validateRange(procName, str, start, end) {
    const s = start === undefined ? 0 : Number(start);
    const e = end === undefined ? str.length : Number(end);

    if (!Number.isInteger(s) || s < 0 || s > str.length) {
        throw new SchemeRangeError(procName, 'start', 0, str.length, s);
    }
    if (!Number.isInteger(e) || e < s || e > str.length) {
        throw new SchemeRangeError(procName, 'end', s, str.length, e);
    }
    return [s, e];
}

// =============================================================================
// String Primitives
// =============================================================================

/**
 * String primitives exported to Scheme.
 */
export const stringPrimitives = {
    // -------------------------------------------------------------------------
    // Type Predicate
    // -------------------------------------------------------------------------

    /**
     * String type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a string.
     */
    'string?': (obj) => typeof obj === 'string',

    // -------------------------------------------------------------------------
    // Constructors
    // -------------------------------------------------------------------------

    /**
     * Creates a string of given length, optionally filled with a character.
     * @param {number} k - Length of string
     * @param {string} [char] - Fill character (default unspecified)
     * @returns {string} New string
     */
    'make-string': (k, char) => {
        assertInteger('make-string', 1, k);
        const len = Number(k);
        if (len < 0) {
            throw new SchemeRangeError('make-string', 'length', 0, Infinity, k);
        }
        if (char !== undefined) {
            assertChar('make-string', 2, char);
            return char.toString().repeat(len);
        }
        // R7RS: unspecified fill, we use space
        return ' '.repeat(len);
    },

    /**
     * Creates a string from character arguments.
     * @param {...string} chars - Characters
     * @returns {string} New string
     */
    'string': (...chars) => {
        assertAllChars('string', chars);
        return chars.map(c => c.toString()).join('');
    },

    // -------------------------------------------------------------------------
    // Accessors
    // -------------------------------------------------------------------------

    /**
     * Returns the length of a string.
     * @param {string} str - String
     * @returns {bigint} Length (exact integer)
     */
    'string-length': (str) => {
        assertString('string-length', 1, str);
        return BigInt(str.length);
    },

    /**
     * Returns the character at position k.
     * @param {string} str - String
     * @param {number} k - Index
     * @returns {string} Character at index
     */
    'string-ref': (str, k) => {
        assertString('string-ref', 1, str);
        assertInteger('string-ref', 2, k);
        const idx = Number(k);
        if (idx < 0 || idx >= str.length) {
            throw new SchemeRangeError('string-ref', 'index', 0, str.length - 1, k);
        }
        return new Char(str.codePointAt(idx));
    },

    /**
     * Mutation is not supported for JS string interoperability.
     * @throws {SchemeError} Always throws
     */
    'string-set!': (str, k, char) => {
        throw new SchemeError(
            'string-set!: strings are immutable in this implementation for JavaScript interoperability',
            [],
            'string-set!'
        );
    },

    // -------------------------------------------------------------------------
    // Comparison (Case-Sensitive)
    // -------------------------------------------------------------------------

    /**
     * Returns #t if all strings are equal.
     * @param {...string} strs - Strings to compare
     * @returns {boolean}
     */
    'string=?': (...args) => compareStrings('string=?', (a, b) => a === b, args),

    /**
     * Returns #t if strings are monotonically increasing.
     * @param {...string} strs - Strings to compare
     * @returns {boolean}
     */
    'string<?': (...args) => compareStrings('string<?', (a, b) => a < b, args),

    /**
     * Returns #t if strings are monotonically decreasing.
     * @param {...string} strs - Strings to compare
     * @returns {boolean}
     */
    'string>?': (...args) => compareStrings('string>?', (a, b) => a > b, args),

    /**
     * Returns #t if strings are monotonically non-decreasing.
     * @param {...string} strs - Strings to compare
     * @returns {boolean}
     */
    'string<=?': (...args) => compareStrings('string<=?', (a, b) => a <= b, args),

    /**
     * Returns #t if strings are monotonically non-increasing.
     * @param {...string} strs - Strings to compare
     * @returns {boolean}
     */
    'string>=?': (...args) => compareStrings('string>=?', (a, b) => a >= b, args),

    // -------------------------------------------------------------------------
    // Comparison (Case-Insensitive)
    // -------------------------------------------------------------------------

    /**
     * Case-insensitive string=?.
     * @param {...string} strs - Strings to compare
     * @returns {boolean}
     */
    'string-ci=?': (...args) => compareCiStrings('string-ci=?', (a, b) => a === b, args),

    /**
     * Case-insensitive string<?.
     * @param {...string} strs - Strings to compare
     * @returns {boolean}
     */
    'string-ci<?': (...args) => compareCiStrings('string-ci<?', (a, b) => a < b, args),

    /**
     * Case-insensitive string>?.
     * @param {...string} strs - Strings to compare
     * @returns {boolean}
     */
    'string-ci>?': (...args) => compareCiStrings('string-ci>?', (a, b) => a > b, args),

    /**
     * Case-insensitive string<=?.
     * @param {...string} strs - Strings to compare
     * @returns {boolean}
     */
    'string-ci<=?': (...args) => compareCiStrings('string-ci<=?', (a, b) => a <= b, args),

    /**
     * Case-insensitive string>=?.
     * @param {...string} strs - Strings to compare
     * @returns {boolean}
     */
    'string-ci>=?': (...args) => compareCiStrings('string-ci>=?', (a, b) => a >= b, args),

    // -------------------------------------------------------------------------
    // Substring and Copy
    // -------------------------------------------------------------------------

    /**
     * Extracts a substring.
     * @param {string} str - Source string
     * @param {number} start - Start index
     * @param {number} end - End index
     * @returns {string} Substring
     */
    'substring': (str, start, end) => {
        assertString('substring', 1, str);
        assertInteger('substring', 2, start);
        assertInteger('substring', 3, end);
        const [s, e] = validateRange('substring', str, start, end);
        return str.slice(s, e);
    },

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
     * Copies a string, optionally a portion of it.
     * @param {string} str - Source string
     * @param {number} [start] - Start index (default 0)
     * @param {number} [end] - End index (default length)
     * @returns {string} Copied string
     */
    'string-copy': (str, start, end) => {
        assertString('string-copy', 1, str);
        const [s, e] = validateRange('string-copy', str, start, end);
        return str.slice(s, e);
    },

    /**
     * Mutation is not supported for JS string interoperability.
     * @throws {SchemeError} Always throws
     */
    'string-fill!': (str, char, start, end) => {
        throw new SchemeError(
            'string-fill!: strings are immutable in this implementation for JavaScript interoperability',
            [],
            'string-fill!'
        );
    },

    // -------------------------------------------------------------------------
    // Conversion
    // -------------------------------------------------------------------------

    /**
     * Converts string to list of characters.
     * @param {string} str - String to convert
     * @param {number} [start] - Start index
     * @param {number} [end] - End index
     * @returns {Cons|null} List of characters
     */
    'string->list': (str, start, end) => {
        assertString('string->list', 1, str);
        const [s, e] = validateRange('string->list', str, start, end);
        const substr = str.slice(s, e);
        const chars = [];
        for (const char of substr) {
            chars.push(new Char(char.codePointAt(0)));
        }
        return list(...chars);
    },

    /**
     * Converts list of characters to string.
     * @param {Cons|null} lst - List of characters
     * @returns {string} String
     */
    'list->string': (lst) => {
        const chars = toArray(lst);
        chars.forEach((c, i) => {
            if (!isChar(c)) {
                throw new SchemeError(
                    `list->string: element ${i + 1} is not a character`,
                    [c],
                    'list->string'
                );
            }
        });
        return chars.map(c => c.toString()).join('');
    },

    /**
     * Converts number to string.
     * @param {number} num - Number to convert.
     * @param {number} [radix] - Radix (2, 8, 10, 16)
     * @returns {string} String representation.
     */
    'number->string': (num, radix) => {
        assertNumber('number->string', 1, num);

        const r = radix === undefined ? 10 : Number(radix);
        if (radix !== undefined) {
            assertInteger('number->string', 2, radix);
            if (![2, 8, 10, 16].includes(r)) {
                throw new SchemeRangeError('number->string', 'radix', 2, 16, radix);
            }
        }

        // Complex numbers have their own toString
        if (num instanceof Complex) {
            return num.toString(r);
        }

        // Rational numbers have their own toString
        if (num instanceof Rational) {
            return num.toString(r);
        }

        // R7RS special value formatting for inexact real numbers
        if (typeof num === 'number') {
            if (num === Infinity) return '+inf.0';
            if (num === -Infinity) return '-inf.0';
            if (Number.isNaN(num)) return '+nan.0';
            // Handle negative zero specially - JS toString() loses the sign
            if (Object.is(num, -0)) return '-0.0';

            // For inexact integer-valued numbers, show decimal point
            // to indicate inexactness per R7RS
            let s = num.toString(r);
            if (Number.isInteger(num) && !s.includes('.') && !s.includes('e')) {
                s += '.0';
            }
            return s;
        }

        // BigInt (exact integers) - no decimal point
        if (typeof num === 'bigint') {
            return num.toString(r);
        }

        return num.toString(r);
    },

    /**
     * Parses a string to a number.
     * @param {string} str - String to parse
     * @param {number} [radix] - Radix (2, 8, 10, 16)
     * @returns {number|boolean} Number or #f if invalid
     */
    'string->number': (str, radix) => {
        assertString('string->number', 1, str);
        const r = radix === undefined ? 10 : Number(radix);
        if (radix !== undefined) {
            assertInteger('string->number', 2, radix);
        }

        // Delegate to reader's parseNumber for robust Scheme numeric syntax support
        // Note: radix argument overrides any prefix in string if inconsistent?
        // R7RS: "If radix is not supplied, ... If radix is supplied, ... interpret with that radix"

        let prefix = "";

        // If radix is explicit, we prepend the corresponding prefix if not present?
        if (r === 2) prefix = "#b";
        else if (r === 8) prefix = "#o";
        else if (r === 10) prefix = "#d";
        else if (r === 16) prefix = "#x";

        try {
            // Simplified approach: use parseNumber directly.

            // If customized radix (not 2,8,10,16), full custom logic required.
            if (![2, 8, 10, 16].includes(r)) {
                // Fallback to old simple parsing for non-standard bases (integers only)
                const num = parseInt(str, r);
                if (isNaN(num)) return false;
                // Validate chars
                const validChars = '0123456789abcdefghijklmnopqrstuvwxyz'.slice(0, r);
                const cleanStr = str.replace(/^[+-]/, '');
                for (const ch of cleanStr.toLowerCase()) {
                    if (!validChars.includes(ch)) return false;
                }
                return num;
            }

            // Standard bases: Use parseNumber
            // If string starts with #, parseNumber handles it.
            // If string DOES NOT start with #, pretend it has #d (default) or the radix prefix.

            let targetStr = str;
            if (!str.trim().startsWith('#')) {
                targetStr = prefix + str;
            }
            // If explicit radix given, we check if logic is consistent
            // If (string->number "#x10" 10) -> Should fail or ignore prefix?
            // R7RS: "If the string has a radix prefix, and a radix argument is also supplied,
            // and they imply different radices, an error is signaled."
            // We return #f for now to be safe (or false).
            if (radix !== undefined) {
                const match = str.match(/^#[boxd]/i);
                if (match) {
                    const p = match[0].toLowerCase();
                    if (r === 2 && p !== '#b') return false;
                    if (r === 8 && p !== '#o') return false;
                    if (r === 10 && p !== '#d') return false;
                    if (r === 16 && p !== '#x') return false;
                }
            }

            const result = parseNumber(targetStr);
            if (result === null) return false;

            return result;

        } catch (e) {
            return false;
        }
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
    },

    // -------------------------------------------------------------------------
    // Case Conversion
    // -------------------------------------------------------------------------

    /**
     * Returns uppercase version of string.
     * @param {string} str - String
     * @returns {string} Uppercase string
     */
    'string-upcase': (str) => {
        assertString('string-upcase', 1, str);
        return str.toUpperCase();
    },

    /**
     * Returns lowercase version of string.
     * @param {string} str - String
     * @returns {string} Lowercase string
     */
    'string-downcase': (str) => {
        assertString('string-downcase', 1, str);
        return str.toLowerCase();
    },

    /**
     * Returns case-folded version of string.
     * For simple cases, this is the same as downcase.
     * @param {string} str - String
     * @returns {string} Folded string
     */
    'string-foldcase': (str) => {
        assertString('string-foldcase', 1, str);
        return str.toLowerCase();
    }
};
