/**
 * @fileoverview Number parsing for Scheme reader.
 * Handles integers, decimals, rationals, complex numbers, and R7RS prefixes.
 */

import { Rational } from '../../primitives/rational.js';
import { Complex } from '../../primitives/complex.js';
import { SchemeReadError } from '../errors.js';

/**
 * Parses a numeric literal (integers, rationals, complex, with optional prefixes)
 * R7RS supports prefixes: #b (binary), #o (octal), #d (decimal), #x (hex), #e (exact), #i (inexact)
 * @param {string} token 
 * @returns {number|Rational|Complex|null}
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

    // Helper to parse a real component string into a number or Rational
    const parseRealStr = (str) => {
        if (!str) return 0;
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
            return new Rational(parseInt(parts[0], 10), parseInt(parts[1], 10));
        }
        return parseFloat(str);
    };

    // Handle immediate special values
    if (/^[+-]?inf\.0$/i.test(token)) return parseRealStr(token);
    if (/^[+-]?nan\.0$/i.test(token)) return NaN;

    // Pattern for unsigned real numbers: rationals, integers, decimals, scientific notation, inf.0, nan.0
    const UNSIGNED_REAL = '(?:\\d+/\\d+|(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?|inf\\.0|nan\\.0)';

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

        let imagVal = imagStr ? parseRealStr(imagStr) : 1;
        // Apply sign
        if (sign === -1) {
            if (imagVal instanceof Rational) imagVal = imagVal.negate();
            else imagVal = -imagVal;
        }

        return new Complex(real, imagVal);
    }

    // Pure imaginary: +i, -i, 3i, +inf.0i
    // Captures: 1:real(signed) or sign
    const pureImagRegex = new RegExp(`^(${REAL_PATTERN}|[+-])i$`, 'i');
    const pureImagMatch = token.match(pureImagRegex);

    if (pureImagMatch) {
        const part = pureImagMatch[1];
        if (part === '+' || part === '') return new Complex(0, 1);
        if (part === '-') return new Complex(0, -1);

        return new Complex(0, parseRealStr(part));
    }

    // Check for rational: 1/2, -3/4, etc.
    const rationalMatch = token.match(/^([+-]?\d+)\/(\d+)$/);
    if (rationalMatch) {
        const num = parseInt(rationalMatch[1], 10);
        const den = parseInt(rationalMatch[2], 10);
        if (den === 0) {
            throw new SchemeReadError('division by zero', 'rational');
        }
        return new Rational(num, den);
    }

    // Regular number (integer or decimal)
    let num = Number(token);
    if (!isNaN(num)) {
        return num;
    }

    return null; // Not a number
}

/**
 * Parses a number with R7RS prefix notation.
 * Handles #x (hex), #o (octal), #b (binary), #d (decimal), #e (exact), #i (inexact)
 * and combinations like #e#x10 or #x#e10
 * @param {string} token - Token starting with #
 * @returns {number|Rational|null}
 */
export function parsePrefixedNumber(token) {
    let exactness = null; // 'exact', 'inexact', or null
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
                return null; // Not a numeric prefix
        }
    }

    // If still starts with #, it's not a valid number
    if (rest.startsWith('#')) {
        return null;
    }

    // Normalize alternative exponent markers for decimal numbers
    if (radix === 10 && /^[+-]?(\d+\.?\d*|\.\d+)[sSfFdDlL][+-]?\d+$/.test(rest)) {
        rest = rest.replace(/[sSfFdDlL]/, 'e');
    }

    // Handle rational with radix: #x10/2 means 16/2 = 8
    const rationalMatch = rest.match(/^([+-]?[0-9a-fA-F]+)\/([0-9a-fA-F]+)$/);
    if (rationalMatch) {
        const num = parseInt(rationalMatch[1], radix);
        const den = parseInt(rationalMatch[2], radix);
        if (isNaN(num) || isNaN(den)) return null;
        if (den === 0) throw new SchemeReadError('division by zero', 'rational');

        if (exactness === 'inexact') {
            return num / den;
        }
        const rat = new Rational(num, den);
        return rat;
    }

    // Handle complex with radix: #d10+11i
    const complexMatch = rest.match(/^([+-]?[0-9a-fA-F.]+)([+-])([0-9a-fA-F.]+)?i$/);
    if (complexMatch) {
        const real = radix === 10 ? parseFloat(complexMatch[1]) : parseInt(complexMatch[1], radix);
        const sign = complexMatch[2] === '-' ? -1 : 1;
        const imagPart = complexMatch[3] || '1';
        const imag = sign * (radix === 10 ? parseFloat(imagPart) : parseInt(imagPart, radix));
        return new Complex(real, imag);
    }

    // Parse as integer in the given radix
    let result;

    // Handle special values: +inf.0, -inf.0, +nan.0, -nan.0 (case-insensitive)
    const lowerRest = rest.toLowerCase();
    if (/^[+-]?inf\.0$/.test(lowerRest)) {
        return lowerRest.startsWith('-') ? -Infinity : Infinity;
    }
    if (/^[+-]?nan\.0$/.test(lowerRest)) {
        return NaN;
    }

    if (radix === 10 && (rest.includes('.') || rest.toLowerCase().includes('e') || rest.toLowerCase().includes('s') || rest.toLowerCase().includes('f') || rest.toLowerCase().includes('d') || rest.toLowerCase().includes('l'))) {
        // Decimal with fractional part or exponent
        // Validate strict format: optional sign, digits, optional dot, optional digits, optional exponent
        if (!/^[+-]?(\d+(\.\d*)?|\.\d+)([eEsSfFdDlL][+-]?\d+)?$/.test(rest)) {
            return null;
        }
        const normalized = rest.replace(/[sSfFdDlL](?=[+-]?\d)/g, 'e');
        result = parseFloat(normalized);
    } else {
        // Integer in given radix
        // Validate chars strictly
        const validChars = '0123456789abcdefghijklmnopqrstuvwxyz'.slice(0, radix);
        const checkRest = rest.replace(/^[+-]/, '').toLowerCase();
        for (const char of checkRest) {
            if (!validChars.includes(char)) return null;
        }
        result = parseInt(rest, radix);
    }

    if (isNaN(result)) {
        return null;
    }

    // Apply exactness
    if (exactness === 'inexact' && Number.isInteger(result)) {
        result = result + 0.0; // Ensure it's a float (though in JS all numbers are floats)
    } else if (exactness === 'exact' && Number.isInteger(result)) {
        // Already exact, return as-is or as Rational
        return result;
    }

    return result;
}
