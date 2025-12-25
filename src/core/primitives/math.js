/**
 * Math Primitives for Scheme.
 * 
 * Provides arithmetic and comparison operations for the Scheme runtime.
 * Implements R7RS §6.2 numeric operations.
 * 
 * NOTE: Only primitives that REQUIRE JavaScript are implemented here.
 * Higher-level numeric procedures are in core.scm.
 */

import { assertNumber, assertInteger, assertArity } from '../interpreter/type_check.js';
import { Values } from '../interpreter/values.js';

// =============================================================================
// Math Primitives (JavaScript-required)
// =============================================================================

/**
 * Math primitives exported to Scheme.
 */
export const mathPrimitives = {
    // =========================================================================
    // Arithmetic Operations
    // =========================================================================

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

    // =========================================================================
    // Binary Comparison Operations (variadic versions in Scheme)
    // =========================================================================

    /**
     * Binary numeric equality.
     * @param {number} a - First number.
     * @param {number} b - Second number.
     * @returns {boolean} True if equal.
     */
    '%num=': (a, b) => {
        assertNumber('%num=', 1, a);
        assertNumber('%num=', 2, b);
        return a === b;
    },

    /**
     * Binary less than.
     * @param {number} a - First number.
     * @param {number} b - Second number.
     * @returns {boolean} True if a < b.
     */
    '%num<': (a, b) => {
        assertNumber('%num<', 1, a);
        assertNumber('%num<', 2, b);
        return a < b;
    },

    /**
     * Binary greater than.
     * @param {number} a - First number.
     * @param {number} b - Second number.
     * @returns {boolean} True if a > b.
     */
    '%num>': (a, b) => {
        assertNumber('%num>', 1, a);
        assertNumber('%num>', 2, b);
        return a > b;
    },

    /**
     * Binary less than or equal.
     * @param {number} a - First number.
     * @param {number} b - Second number.
     * @returns {boolean} True if a <= b.
     */
    '%num<=': (a, b) => {
        assertNumber('%num<=', 1, a);
        assertNumber('%num<=', 2, b);
        return a <= b;
    },

    /**
     * Binary greater than or equal.
     * @param {number} a - First number.
     * @param {number} b - Second number.
     * @returns {boolean} True if a >= b.
     */
    '%num>=': (a, b) => {
        assertNumber('%num>=', 1, a);
        assertNumber('%num>=', 2, b);
        return a >= b;
    },

    // =========================================================================
    // Integer Division
    // =========================================================================

    /**
     * Modulo operation (result has same sign as divisor).
     * @param {number} a - Dividend.
     * @param {number} b - Divisor.
     * @returns {number} Modulo.
     */
    'modulo': (a, b) => {
        assertArity('modulo', [a, b], 2, 2);
        assertInteger('modulo', 1, a);
        assertInteger('modulo', 2, b);
        // JavaScript % gives remainder with sign of dividend
        // modulo should have sign of divisor
        const rem = a % b;
        if (rem === 0) return 0;
        return (rem > 0) === (b > 0) ? rem : rem + b;
    },

    /**
     * Quotient (integer division, truncates toward zero).
     * @param {number} a - Dividend.
     * @param {number} b - Divisor.
     * @returns {number} Integer quotient.
     */
    'quotient': (a, b) => {
        assertArity('quotient', [a, b], 2, 2);
        assertInteger('quotient', 1, a);
        assertInteger('quotient', 2, b);
        return Math.trunc(a / b);
    },

    /**
     * Remainder (result has same sign as dividend).
     * @param {number} a - Dividend.
     * @param {number} b - Divisor.
     * @returns {number} Remainder.
     */
    'remainder': (a, b) => {
        assertArity('remainder', [a, b], 2, 2);
        assertInteger('remainder', 1, a);
        assertInteger('remainder', 2, b);
        return a % b;
    },

    // =========================================================================
    // Type Predicates (require JavaScript typeof)
    // =========================================================================

    /**
     * Number type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a number.
     */
    'number?': (obj) => typeof obj === 'number',

    /**
     * Real number type predicate.
     * In our implementation, all numbers are real (no complex support).
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a real number.
     */
    'real?': (obj) => typeof obj === 'number',

    /**
     * Rational number type predicate.
     * In our implementation, finite numbers are rational.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a rational number.
     */
    'rational?': (obj) => typeof obj === 'number' && Number.isFinite(obj),

    /**
     * Integer type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is an integer.
     */
    'integer?': (obj) => typeof obj === 'number' && Number.isInteger(obj),

    /**
     * Exact integer type predicate.
     * In our implementation, all integers are "exact".
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is an exact integer.
     */
    'exact-integer?': (obj) => typeof obj === 'number' && Number.isInteger(obj),

    /**
     * Finite predicate.
     * @param {number} x - Number to check.
     * @returns {boolean} True if x is finite.
     */
    'finite?': (x) => {
        assertNumber('finite?', 1, x);
        return Number.isFinite(x);
    },

    /**
     * Infinite predicate.
     * @param {number} x - Number to check.
     * @returns {boolean} True if x is infinite.
     */
    'infinite?': (x) => {
        assertNumber('infinite?', 1, x);
        return !Number.isFinite(x) && !Number.isNaN(x);
    },

    /**
     * NaN predicate.
     * @param {number} x - Number to check.
     * @returns {boolean} True if x is NaN.
     */
    'nan?': (x) => {
        assertNumber('nan?', 1, x);
        return Number.isNaN(x);
    },

    // =========================================================================
    // Math.* Functions (require JavaScript Math object)
    // =========================================================================

    /**
     * Absolute value.
     * @param {number} x - Number.
     * @returns {number} Absolute value.
     */
    'abs': (x) => {
        assertNumber('abs', 1, x);
        return Math.abs(x);
    },

    /**
     * Floor (largest integer <= x).
     * @param {number} x - Number.
     * @returns {number} Floor of x.
     */
    'floor': (x) => {
        assertNumber('floor', 1, x);
        return Math.floor(x);
    },

    /**
     * Ceiling (smallest integer >= x).
     * @param {number} x - Number.
     * @returns {number} Ceiling of x.
     */
    'ceiling': (x) => {
        assertNumber('ceiling', 1, x);
        return Math.ceil(x);
    },

    /**
     * Truncate (integer part, toward zero).
     * @param {number} x - Number.
     * @returns {number} Truncated value.
     */
    'truncate': (x) => {
        assertNumber('truncate', 1, x);
        return Math.trunc(x);
    },

    /**
     * Exponentiation.
     * @param {number} base - Base.
     * @param {number} exponent - Exponent.
     * @returns {number} base^exponent.
     */
    'expt': (base, exponent) => {
        assertArity('expt', [base, exponent], 2, 2);
        assertNumber('expt', 1, base);
        assertNumber('expt', 2, exponent);
        return Math.pow(base, exponent);
    },

    /**
     * Square root.
     * @param {number} x - Number.
     * @returns {number} Square root.
     */
    'sqrt': (x) => {
        assertNumber('sqrt', 1, x);
        return Math.sqrt(x);
    },

    /**
     * Sine.
     * @param {number} x - Angle in radians.
     * @returns {number} Sine of x.
     */
    'sin': (x) => {
        assertNumber('sin', 1, x);
        return Math.sin(x);
    },

    /**
     * Cosine.
     * @param {number} x - Angle in radians.
     * @returns {number} Cosine of x.
     */
    'cos': (x) => {
        assertNumber('cos', 1, x);
        return Math.cos(x);
    },

    /**
     * Tangent.
     * @param {number} x - Angle in radians.
     * @returns {number} Tangent of x.
     */
    'tan': (x) => {
        assertNumber('tan', 1, x);
        return Math.tan(x);
    },

    /**
     * Arcsine.
     * @param {number} x - Value.
     * @returns {number} Arcsine in radians.
     */
    'asin': (x) => {
        assertNumber('asin', 1, x);
        return Math.asin(x);
    },

    /**
     * Arccosine.
     * @param {number} x - Value.
     * @returns {number} Arccosine in radians.
     */
    'acos': (x) => {
        assertNumber('acos', 1, x);
        return Math.acos(x);
    },

    /**
     * Arctangent. With two arguments, returns atan2(y, x).
     * @param {number} y - Y value (or angle if single arg).
     * @param {number} [x] - X value (optional).
     * @returns {number} Arctangent in radians.
     */
    'atan': (y, x) => {
        assertNumber('atan', 1, y);
        if (x === undefined) {
            return Math.atan(y);
        }
        assertNumber('atan', 2, x);
        return Math.atan2(y, x);
    },

    /**
     * Natural logarithm.
     * @param {number} x - Number.
     * @returns {number} Natural log of x.
     */
    'log': (x) => {
        assertNumber('log', 1, x);
        return Math.log(x);
    },

    /**
     * Exponential function (e^x).
     * @param {number} x - Exponent.
     * @returns {number} e^x.
     */
    'exp': (x) => {
        assertNumber('exp', 1, x);
        return Math.exp(x);
    },

    // =========================================================================
    // Multiple-Value Returning Procedures (R7RS §6.2.6)
    // =========================================================================

    /**
     * Exact integer square root.
     * Returns two values: the root and remainder such that k = s^2 + r
     * @param {number} k - Non-negative exact integer
     * @returns {Values} Two values: root and remainder
     */
    'exact-integer-sqrt': (k) => {
        assertInteger('exact-integer-sqrt', 1, k);
        if (k < 0) {
            throw new Error('exact-integer-sqrt: expected non-negative integer');
        }
        const s = Math.floor(Math.sqrt(k));
        const r = k - s * s;
        return new Values([s, r]);
    },

    /**
     * Floor division.
     * Returns two values: quotient and remainder such that
     * n1 = n2 * quotient + remainder, with remainder having same sign as n2.
     * @param {number} n1 - Dividend
     * @param {number} n2 - Divisor
     * @returns {Values} Two values: quotient and remainder
     */
    'floor/': (n1, n2) => {
        assertArity('floor/', [n1, n2], 2, 2);
        assertInteger('floor/', 1, n1);
        assertInteger('floor/', 2, n2);
        if (n2 === 0) {
            throw new Error('floor/: division by zero');
        }
        const q = Math.floor(n1 / n2);
        const r = n1 - n2 * q;
        return new Values([q, r]);
    },

    /**
     * Truncate division.
     * Returns two values: quotient and remainder such that
     * n1 = n2 * quotient + remainder, with remainder having same sign as n1.
     * @param {number} n1 - Dividend
     * @param {number} n2 - Divisor
     * @returns {Values} Two values: quotient and remainder
     */
    'truncate/': (n1, n2) => {
        assertArity('truncate/', [n1, n2], 2, 2);
        assertInteger('truncate/', 1, n1);
        assertInteger('truncate/', 2, n2);
        if (n2 === 0) {
            throw new Error('truncate/: division by zero');
        }
        const q = Math.trunc(n1 / n2);
        const r = n1 - n2 * q;
        return new Values([q, r]);
    },

    /**
     * Floor quotient (single value).
     * @param {number} n1 - Dividend
     * @param {number} n2 - Divisor
     * @returns {number} Floor quotient
     */
    'floor-quotient': (n1, n2) => {
        assertArity('floor-quotient', [n1, n2], 2, 2);
        assertInteger('floor-quotient', 1, n1);
        assertInteger('floor-quotient', 2, n2);
        if (n2 === 0) {
            throw new Error('floor-quotient: division by zero');
        }
        return Math.floor(n1 / n2);
    },

    /**
     * Floor remainder (single value).
     * @param {number} n1 - Dividend
     * @param {number} n2 - Divisor
     * @returns {number} Floor remainder
     */
    'floor-remainder': (n1, n2) => {
        assertArity('floor-remainder', [n1, n2], 2, 2);
        assertInteger('floor-remainder', 1, n1);
        assertInteger('floor-remainder', 2, n2);
        if (n2 === 0) {
            throw new Error('floor-remainder: division by zero');
        }
        const q = Math.floor(n1 / n2);
        return n1 - n2 * q;
    },

    /**
     * Truncate quotient (single value).
     * @param {number} n1 - Dividend
     * @param {number} n2 - Divisor
     * @returns {number} Truncate quotient
     */
    'truncate-quotient': (n1, n2) => {
        assertArity('truncate-quotient', [n1, n2], 2, 2);
        assertInteger('truncate-quotient', 1, n1);
        assertInteger('truncate-quotient', 2, n2);
        if (n2 === 0) {
            throw new Error('truncate-quotient: division by zero');
        }
        return Math.trunc(n1 / n2);
    },

    /**
     * Truncate remainder (single value).
     * @param {number} n1 - Dividend
     * @param {number} n2 - Divisor
     * @returns {number} Truncate remainder
     */
    'truncate-remainder': (n1, n2) => {
        assertArity('truncate-remainder', [n1, n2], 2, 2);
        assertInteger('truncate-remainder', 1, n1);
        assertInteger('truncate-remainder', 2, n2);
        if (n2 === 0) {
            throw new Error('truncate-remainder: division by zero');
        }
        return n1 % n2;
    },
};
