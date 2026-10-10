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
// Unicode
// =============================================================================
// R7RS 6.6 defines the character predicates by Unicode's properties, and the
// case procedures by its mappings: a character's simple ones, one character
// to one, and a string's full ones (6.7). JavaScript has the properties, as
// regular expression classes, and the full mappings, `toUpperCase` and
// `toLowerCase`; the simple mappings and folding are made from those, and the
// few characters on which they differ are named below.

const ALPHABETIC = /^\p{Alphabetic}$/u;
const DECIMAL_DIGIT = /^\p{Nd}$/u;
const WHITE_SPACE = /^\p{White_Space}$/u;
const UPPERCASE = /^\p{Uppercase}$/u;
const LOWERCASE = /^\p{Lowercase}$/u;

/**
 * A decimal digit's value. Unicode encodes each script's decimal digits as
 * one run of code points from zero to nine, and runs of several -- the
 * mathematical digits -- one after another, so a digit's value is its
 * distance from the start of its run, modulo ten.
 * @param {number} code - The code point of a decimal digit.
 * @returns {number}
 */
function decimalDigitValue(code) {
    let start = code;
    while (start > 0 && DECIMAL_DIGIT.test(String.fromCodePoint(start - 1))) start--;
    return (code - start) % 10;
}

/**
 * The code point a full case mapping takes a character to, if it takes it
 * to one, else -1.
 * @param {string} mapped - The mapping's result.
 * @returns {number}
 */
function singleCodePoint(mapped) {
    const code = mapped.codePointAt(0);
    return mapped.length === (code > 0xFFFF ? 2 : 1) ? code : -1;
}

/**
 * A character's simple uppercase mapping. Where the full mapping is several
 * characters, the simple one is the character itself -- `(char-upcase #\ß)`
 * is #\ß, not the S of SS -- but for the Greek letters with ypogegrammeni,
 * which map to their forms with prosgegrammeni.
 * @param {number} code - The code point.
 * @returns {number}
 */
function simpleUpcase(code) {
    const mapped = singleCodePoint(String.fromCodePoint(code).toUpperCase());
    if (mapped >= 0) return mapped;
    if ((code >= 0x1F80 && code <= 0x1F87) || (code >= 0x1F90 && code <= 0x1F97) ||
        (code >= 0x1FA0 && code <= 0x1FA7)) return code + 8;
    if (code === 0x1FB3 || code === 0x1FC3 || code === 0x1FF3) return code + 9;
    return code;
}

/**
 * A character's simple lowercase mapping. The only character whose full
 * mapping is several characters, İ, maps to the first of them, i.
 * @param {number} code - The code point.
 * @returns {number}
 */
function simpleDowncase(code) {
    return String.fromCodePoint(code).toLowerCase().codePointAt(0);
}

/**
 * Whether a character folds to an uppercase letter: Cherokee's lowercase
 * letters, which Unicode added after its uppercase ones and folds to them.
 * @param {number} code - The code point.
 * @returns {boolean}
 */
function isCherokeeLowercase(code) {
    return (code >= 0xAB70 && code <= 0xABBF) || (code >= 0x13F8 && code <= 0x13FD);
}

/**
 * A character's simple case folding: its uppercase mapping's lowercase one,
 * which takes every sigma to σ and the long s to s. The dotted and dotless
 * Turkish i fold only to themselves, and Cherokee folds to its uppercase.
 * @param {number} code - The code point.
 * @returns {number}
 */
export function foldCodePoint(code) {
    if (code === 0x130 || code === 0x131) return code;
    if (isCherokeeLowercase(code)) return simpleUpcase(code);
    return simpleDowncase(simpleUpcase(code));
}

/**
 * A string's full case folding (R7RS 6.7 `string-foldcase`), character by
 * character so that no sigma is taken for a word's last: ß folds to ss,
 * ΜΈΛΟΣ to μέλοσ, ﬃ to ffi. As `foldCodePoint` for the Turkish i's and
 * Cherokee.
 * @param {string} text - The string.
 * @returns {string}
 */
export function foldString(text) {
    let folded = '';
    for (const c of text) {
        const code = c.codePointAt(0);
        if (code === 0x131) folded += c;
        else if (isCherokeeLowercase(code)) folded += String.fromCodePoint(simpleUpcase(code));
        else folded += c.toUpperCase().toLowerCase();
    }
    return folded;
}

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
 * Variadic character comparison helper. Characters are compared by their code
 * points, as R7RS orders them (`char->integer`): compared as strings, in
 * UTF-16, a character beyond U+FFFF came before one from U+E000 to U+FFFF.
 * @param {string} procName - Procedure name
 * @param {Function} compare - Comparison function (a, b) => boolean
 * @param {Array} args - Character arguments
 * @returns {boolean} True if comparison holds for all adjacent pairs
 */
function compareChars(procName, compare, args) {
    assertArity(procName, args, 2, Infinity);
    assertAllChars(procName, args);
    for (let i = 0; i < args.length - 1; i++) {
        if (!compare(args[i].codePoint, args[i + 1].codePoint)) return false;
    }
    return true;
}

/**
 * Variadic case-insensitive character comparison helper, by the code points
 * of the characters case-folded, as `compareChars` compares code points.
 * @param {string} procName - Procedure name
 * @param {Function} compare - Comparison function (a, b) => boolean
 * @param {Array} args - Character arguments
 * @returns {boolean} True if comparison holds for all adjacent pairs
 */
function compareCiChars(procName, compare, args) {
    assertArity(procName, args, 2, Infinity);
    assertAllChars(procName, args);
    for (let i = 0; i < args.length - 1; i++) {
        if (!compare(foldCodePoint(args[i].codePoint), foldCodePoint(args[i + 1].codePoint))) return false;
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
        return ALPHABETIC.test(char.toString());
    },

    /**
     * Returns #t if char is a decimal digit, in any script.
     * @param {string} char - Character to test.
     * @returns {boolean}
     */
    'char-numeric?': (char) => {
        assertChar('char-numeric?', 1, char);
        return DECIMAL_DIGIT.test(char.toString());
    },

    /**
     * Returns #t if char is whitespace.
     * @param {string} char - Character to test.
     * @returns {boolean}
     */
    'char-whitespace?': (char) => {
        assertChar('char-whitespace?', 1, char);
        return WHITE_SPACE.test(char.toString());
    },

    /**
     * Returns #t if char is uppercase.
     * @param {string} char - Character to test.
     * @returns {boolean}
     */
    'char-upper-case?': (char) => {
        assertChar('char-upper-case?', 1, char);
        return UPPERCASE.test(char.toString());
    },

    /**
     * Returns #t if char is lowercase.
     * @param {string} char - Character to test.
     * @returns {boolean}
     */
    'char-lower-case?': (char) => {
        assertChar('char-lower-case?', 1, char);
        return LOWERCASE.test(char.toString());
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
        return char.valueOf();
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
        return new Char(simpleUpcase(char.valueOf()));
    },

    /**
     * Returns the lowercase version of a character.
     * @param {string} char - Character.
     * @returns {string} Lowercase character.
     */
    'char-downcase': (char) => {
        assertChar('char-downcase', 1, char);
        return new Char(simpleDowncase(char.valueOf()));
    },

    /**
     * Returns the case-folded version of a character: Unicode's simple
     * folding (`foldCodePoint`).
     * @param {string} char - Character.
     * @returns {string} Folded character.
     */
    'char-foldcase': (char) => {
        assertChar('char-foldcase', 1, char);
        return new Char(foldCodePoint(char.valueOf()));
    },

    // -------------------------------------------------------------------------
    // Digit Value
    // -------------------------------------------------------------------------

    /**
     * Returns the digit value if char is a decimal digit, in any script
     * (R7RS 6.6: `(digit-value #\x0664)` is 4), else #f.
     * @param {string} char - Character.
     * @returns {number|boolean} Digit value or #f.
     */
    'digit-value': (char) => {
        assertChar('digit-value', 1, char);
        const code = char.valueOf();
        if (code >= 48 && code <= 57) return code - 48;
        return DECIMAL_DIGIT.test(char.toString()) ? decimalDigitValue(code) : false;
    }
};
