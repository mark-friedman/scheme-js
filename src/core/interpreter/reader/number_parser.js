/**
 * @fileoverview Number parsing for Scheme reader.
 * Handles integers, decimals, rationals, complex numbers, and R7RS prefixes.
 * 
 * Exact integers are represented as BigInt for arbitrary precision.
 * Inexact numbers are represented as JavaScript Number.
 */

import { Rational } from '../../primitives/rational.js';
import { Complex } from '../../primitives/complex.js';
import { SchemeReadError } from '../errors.js';

/**
 * Parses a numeric literal (integers, rationals, complex, with optional prefixes)
 * R7RS supports prefixes: #b (binary), #o (octal), #d (decimal), #x (hex), #e (exact), #i (inexact)
 * @param {string} token 
 * @returns {number|bigint|Rational|Complex|null}
 */
export function parseNumber(token, exactness) {
    // Normalize R7RS exponent markers (s, f, d, l) to 'e' globally before parsing
    // This handles 1s2 -> 1e2, 1s2+3d4i -> 1e2+3e4i, etc.
    // Use lookahead to ensure we only replace exponent markers followed by a sign or digit,
    // preventing "inf.0" from becoming "ine.0"
    if (/[sSfFdDlL]/.test(token) && !token.startsWith('#')) {
        token = token.replace(/[sSfFdDlL](?=[+-]?\d)/g, 'e');
    }

    // Handle prefixed numbers (#x, #o, #b, #d, #e, #i)
    if (token.startsWith('#')) {
        return parsePrefixedNumber(token);
    }

    // Helper to parse a real component string into a number, BigInt, or Rational
    const parseRealStr = (str) => {
        if (!str) return 0n;
        const lower = str.toLowerCase();
        if (lower.endsWith('inf.0')) {
            const val = lower.startsWith('-') ? -Infinity : Infinity;
            return val;
        }
        if (lower.endsWith('nan.0')) {
            return NaN;
        }
        // Handle rational components (e.g. 1/2) - return Rational if exact components
        if (str.includes('/')) {
            const parts = str.split('/');
            // If either part is decimal/scientific, it's inexact float division
            if (parts[0].includes('.') || parts[0].toLowerCase().includes('e') ||
                parts[1].includes('.') || parts[1].toLowerCase().includes('e')) {
                return parseFloat(parts[0]) / parseFloat(parts[1]);
            }
            // Exact rational - use BigInt for components
            return new Rational(BigInt(parts[0]), BigInt(parts[1]));
        }

        // Check for integer (exact) syntax: no decimal point, no exponent
        if (!str.includes('.') && !lower.includes('e')) {
            try {
                return BigInt(str);
            } catch (e) {
                // Fallback (shouldn't happen for valid integer syntax)
                return parseFloat(str);
            }
        }

        return parseFloat(str);
    };

    // Handle immediate special values
    if (/^[+-]?inf\.0$/i.test(token)) return parseRealStr(token);
    if (/^[+-]?nan\.0$/i.test(token)) return NaN;

    // Pattern for unsigned real numbers: rationals, integers, decimals, scientific notation, inf.0, nan.0
    // R7RS supports s, f, d, l as exponent markers equivalent to e
    const UNSIGNED_REAL = '(?:\\d+/\\d+|(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eEsSfFdDlL][+-]?\\d+)?|inf\\.0|nan\\.0)';

    // Pattern for signed real numbers (capturing group 1)
    const REAL_PATTERN = `([+-]?${UNSIGNED_REAL})`;

    // Complex: real+imag (e.g., 1+2i, 1e2+3e4i, +inf.0+inf.0i)
    // The regex captures: 1:real, 2:sign, 3:imag(unsigned)
    const complexRegex = new RegExp(`^${REAL_PATTERN}([+-])(${UNSIGNED_REAL})?i$`, 'i');
    const complexMatch = token.match(complexRegex);

    if (complexMatch) {
        const real = parseRealStr(complexMatch[1]);
        const sign = complexMatch[2] === '-' ? -1 : 1;
        const imagStr = complexMatch[3];

        let imagVal = imagStr ? parseRealStr(imagStr) : 1n;
        // Apply sign
        if (sign === -1) {
            if (imagVal instanceof Rational) imagVal = imagVal.negate();
            else if (typeof imagVal === 'bigint') imagVal = -imagVal;
            else imagVal = -imagVal;
        }

        // Determine overall exactness: exact only if both parts are exact
        const isExact = (typeof real !== 'number') && (typeof imagVal !== 'number') &&
            (!(real instanceof Rational) || real.exact !== false) &&
            (!(imagVal instanceof Rational) || imagVal.exact !== false);

        return new Complex(real, imagVal, isExact);
    }

    // Pure imaginary: +i, -i, 3i, +inf.0i
    // Captures: 1:real(signed) or sign
    const pureImagRegex = new RegExp(`^(${REAL_PATTERN}|[+-])i$`, 'i');
    const pureImagMatch = token.match(pureImagRegex);

    if (pureImagMatch) {
        const part = pureImagMatch[1];
        if (part === '+' || part === '') return new Complex(0n, 1n);
        if (part === '-') return new Complex(0n, -1n);

        return new Complex(0n, parseRealStr(part));
    }

    // Check for rational: 1/2, -3/4, etc.
    const rationalMatch = token.match(/^([+-]?\d+)\/(\d+)$/);
    if (rationalMatch) {
        const num = BigInt(rationalMatch[1]);
        const den = BigInt(rationalMatch[2]);
        if (den === 0n) {
            throw new SchemeReadError('division by zero', 'rational');
        }
        return new Rational(num, den);
    }

    // Check for integer (no decimal point, no exponent) -> BigInt (exact)
    if (/^[+-]?\d+$/.test(token)) {
        return BigInt(token);
    }

    // Regular number with decimal or exponent (inexact) -> Number
    let num = Number(token);
    if (!isNaN(num)) {
        return num;
    }

    return null; // Not a number
}

/**
 * Parses a decimal string (with optional exponent) as an exact Rational or BigInt.
 * Used when #e prefix is present.
 * @param {string} str
 * @returns {Rational|bigint}
 */
function parseDecimalAsExact(str) {
    const lower = str.toLowerCase();
    if (lower.includes('inf.0') || lower.includes('nan.0')) {
        throw new SchemeReadError('exactness prefix #e cannot be used with infinities or NaN', 'read');
    }

    // Normalize exponent
    const normalized = str.replace(/[sSfFdDlL](?=[+-]?\d)/g, 'e');

    // Split into coefficient and exponent
    // Use 'e' or 'E' (normalized above to 'e')
    let [coeffStr, expStr] = normalized.toLowerCase().split('e');
    let exp = expStr ? BigInt(expStr) : 0n;

    // Handle decimal point in coefficient
    let fractionalDigits = 0;
    if (coeffStr.includes('.')) {
        const parts = coeffStr.split('.');
        fractionalDigits = parts[1].length;
        coeffStr = parts[0] + parts[1];
    }

    let coeff = BigInt(coeffStr);

    // Effective exponent = explicit exponent - number of fractional digits
    // value = coeff * 10^(exp - fractionalDigits)
    let effectiveExp = exp - BigInt(fractionalDigits);

    if (effectiveExp >= 0n) {
        return coeff * (10n ** effectiveExp);
    } else {
        const denominator = 10n ** (-effectiveExp);
        return new Rational(coeff, denominator);
    }
}

/**
 * Extracts R7RS prefixes (#e, #i, #b, #o, #d, #x) from the token.
 * @param {string} token
 * @returns {{exactness: string|null, radix: number, rest: string}}
 */
function parsePrefixes(token) {
    let exactness = null;
    let radix = 10;
    let rest = token;

    // Parse up to 2 prefixes (one exactness, one radix)
    for (let i = 0; i < 2 && rest.startsWith('#'); i++) {
        const prefix = rest.substring(0, 2).toLowerCase();
        switch (prefix) {
            case '#e':
                exactness = 'exact';
                rest = rest.substring(2);
                break;
            case '#i':
                exactness = 'inexact';
                rest = rest.substring(2);
                break;
            case '#b':
                radix = 2;
                rest = rest.substring(2);
                break;
            case '#o':
                radix = 8;
                rest = rest.substring(2);
                break;
            case '#d':
                radix = 10;
                rest = rest.substring(2);
                break;
            case '#x':
                radix = 16;
                rest = rest.substring(2);
                break;
            default:
                // Not a known numeric prefix, stop parsing prefixes
                // This might happen for things like #\char if passed here,
                // though parsePrefixedNumber should only be called for numbers.
                return { exactness, radix, rest };
        }
    }
    return { exactness, radix, rest };
}

/**
 * Attempts to parse a rational number string with a given radix.
 * @param {string} str
 * @param {number} radix
 * @param {string|null} exactness
 * @returns {number|Rational|null}
 */
function parseRationalWithRadix(str, radix, exactness) {
    const rationalMatch = str.match(/^([+-]?[0-9a-fA-F]+)\/([0-9a-fA-F]+)$/);
    if (!rationalMatch) return null;

    const num = BigInt(parseInt(rationalMatch[1], radix));
    const den = BigInt(parseInt(rationalMatch[2], radix));
    if (den === 0n) throw new SchemeReadError('division by zero', 'rational');

    if (exactness === 'inexact') {
        return Number(num) / Number(den);
    }
    const rat = new Rational(num, den);
    return rat;
}

/**
 * Attempts to parse a complex number string with a given radix.
 * @param {string} str
 * @param {number} radix
 * @param {string|null} exactness
 * @returns {Complex|null}
 */
function parseComplexWithRadix(str, radix, exactness) {
    const complexMatch = str.match(/^([+-]?[0-9a-fA-F.]+)([+-])([0-9a-fA-F.]+)?i$/);
    if (!complexMatch) return null;

    const parsePart = (s) => {
        if (!s) return 0n;
        if (radix === 10 && (s.includes('.') || s.toLowerCase().includes('e'))) return parseFloat(s);
        return BigInt(parseInt(s, radix));
    };

    const realPart = parsePart(complexMatch[1]);
    const sign = complexMatch[2] === '-' ? -1 : 1;
    const imagStr = complexMatch[3] || '1';
    let imagPart = parsePart(imagStr);

    // Apply sign
    if (sign === -1) {
        if (typeof imagPart === 'bigint') imagPart = -imagPart;
        else imagPart = -imagPart;
    }

    const isResultExact = exactness === 'exact' ? true :
        (exactness === 'inexact' ? false :
            (typeof realPart !== 'number' && typeof imagPart !== 'number'));

    return new Complex(realPart, imagPart, isResultExact);
}

/**
 * Parses a real number (integer or decimal) with a given radix.
 * @param {string} str
 * @param {number} radix
 * @param {string|null} exactness
 * @returns {number|bigint|Rational|null}
 */
function parseRealWithRadix(str, radix, exactness) {
    let result;

    // Handle special values: +inf.0, -inf.0, +nan.0, -nan.0 (case-insensitive)
    const lowerStr = str.toLowerCase();
    if (/^[+-]?inf\.0$/.test(lowerStr)) {
        if (exactness === 'exact') {
             throw new SchemeReadError('exactness prefix #e cannot be used with infinities or NaN', 'read');
        }
        return lowerStr.startsWith('-') ? -Infinity : Infinity;
    }
    if (/^[+-]?nan\.0$/.test(lowerStr)) {
        if (exactness === 'exact') {
             throw new SchemeReadError('exactness prefix #e cannot be used with infinities or NaN', 'read');
        }
        return NaN;
    }

    if (radix === 10 && (str.includes('.') || str.toLowerCase().includes('e') || str.toLowerCase().includes('s') || str.toLowerCase().includes('f') || str.toLowerCase().includes('d') || str.toLowerCase().includes('l'))) {
        // Decimal with fractional part or exponent
        // Validate strict format: optional sign, digits, optional dot, optional digits, optional exponent
        if (!/^[+-]?(\d+(\.\d*)?|\.\d+)([eEsSfFdDlL][+-]?\d+)?$/.test(str)) {
            return null;
        }

        // Exact decimal parsing
        if (exactness === 'exact') {
            return parseDecimalAsExact(str);
        }

        const normalized = str.replace(/[sSfFdDlL](?=[+-]?\d)/g, 'e');
        result = parseFloat(normalized);
    } else {
        // Integer in given radix -> BigInt (exact)
        // Verify chars
        const validChars = '0123456789abcdefghijklmnopqrstuvwxyz'.slice(0, radix);
        const checkStr = str.replace(/^[+-]/, '').toLowerCase();
        for (const char of checkStr) {
            if (!validChars.includes(char)) return null;
        }

        // Use BigInt with prefix for correct parsing
        // Note: Node/V8 may not support BigInt("-0x...") directly, so we manual negate.
        if (radix !== 10) {
            const prefix = radix === 16 ? '0x' : (radix === 8 ? '0o' : '0b');
            try {
                if (str.startsWith('-')) {
                    result = -BigInt(prefix + str.slice(1));
                } else if (str.startsWith('+')) {
                    result = BigInt(prefix + str.slice(1));
                } else {
                    result = BigInt(prefix + str);
                }
            } catch (e) {
                return null;
            }
        } else {
            try {
                result = BigInt(str);
            } catch (e) {
                return null;
            }
        }
    }

    return result;
}

/**
 * Parses a number with R7RS prefix notation.
 * Handles #x (hex), #o (octal), #b (binary), #d (decimal), #e (exact), #i (inexact)
 * and combinations like #e#x10 or #x#e10
 * @param {string} token - Token starting with #
 * @returns {number|bigint|Rational|null}
 */
export function parsePrefixedNumber(token) {
    const { exactness, radix, rest } = parsePrefixes(token);

    // If still starts with #, it's not a valid number (e.g. invalid prefix combo)
    if (rest.startsWith('#')) {
        return null;
    }

    // Normalize alternative exponent markers for decimal numbers
    let workingStr = rest;
    if (radix === 10 && /^[+-]?(\d+\.?\d*|\.\d+)[sSfFdDlL][+-]?\d+$/.test(rest)) {
        workingStr = rest.replace(/[sSfFdDlL]/, 'e');
    }

    // 1. Try Rational
    const rational = parseRationalWithRadix(workingStr, radix, exactness);
    if (rational !== null) return rational;

    // 2. Try Complex
    const complex = parseComplexWithRadix(workingStr, radix, exactness);
    if (complex !== null) return complex;

    // 3. Try Real (Integer/Decimal)
    const result = parseRealWithRadix(workingStr, radix, exactness);

    // BigInt can never be NaN; only check for Number
    if (typeof result === 'number' && Number.isNaN(result)) {
         // If it's NaN, we must ensure it was explicitly parsed as NaN, otherwise it's a failure (e.g. invalid string)
         // parseRealWithRadix handles 'nan.0' explicitly.
         // However, parseFloat('foo') returns NaN.
         // We only want to return NaN if the input string *looks* like a NaN.
         if (/^[+-]?nan\.0$/i.test(rest)) {
             return NaN;
         }
         return null;
    }

    if (result === null) return null;

    // Apply exactness finalization
    if (exactness === 'inexact') {
        // Force to Number (inexact)
        return typeof result === 'bigint' ? Number(result) : result;
    } else if (exactness === 'exact' && typeof result === 'number' && Number.isInteger(result)) {
        // Convert float integer to BigInt
        return BigInt(result);
    } else if (typeof result === 'bigint') {
        // Already exact BigInt
        return result;
    }

    return result;
}
