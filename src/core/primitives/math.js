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
import { Rational, isRational } from './rational.js';
import { Complex, isComplex, makeRectangular, makePolar } from './complex.js';

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
    'number?': (obj) => typeof obj === 'number' || isRational(obj) || isComplex(obj),

    /**
     * Complex number type predicate.
     * In R7RS, all numbers are complex.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a complex number.
     */
    'complex?': (obj) => typeof obj === 'number' || isRational(obj) || isComplex(obj),

    /**
     * Real number type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a real number.
     */
    'real?': (obj) => {
        if (typeof obj === 'number') return true;
        if (isRational(obj)) return true;
        if (isComplex(obj)) return obj.imag === 0;
        return false;
    },

    /**
     * Rational number type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a rational number.
     */
    'rational?': (obj) => {
        if (isRational(obj)) return true;
        if (typeof obj === 'number') return Number.isFinite(obj);
        if (isComplex(obj)) return obj.imag === 0 && Number.isFinite(obj.real);
        return false;
    },

    /**
     * Integer type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is an integer.
     */
    'integer?': (obj) => {
        if (typeof obj === 'number') return Number.isInteger(obj);
        if (isRational(obj)) return obj.denominator === 1;
        if (isComplex(obj)) return obj.imag === 0 && Number.isInteger(obj.real);
        return false;
    },

    /**
     * Exact integer type predicate.
     * In our implementation, all integers are "exact".
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is an exact integer.
     */
    'exact-integer?': (obj) => {
        if (typeof obj === 'number') return Number.isInteger(obj);
        if (isRational(obj)) return obj.denominator === 1;
        return false;
    },

    /**
     * Exact number type predicate.
     * Rationals are exact, JS numbers are inexact.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is exact.
     */
    'exact?': (obj) => {
        if (isRational(obj)) return true;
        if (typeof obj === 'number') return Number.isInteger(obj);
        return false;
    },

    /**
     * Inexact number type predicate.
     * JS floats are inexact.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is inexact.
     */
    'inexact?': (obj) => {
        if (typeof obj === 'number') return !Number.isInteger(obj);
        if (isComplex(obj)) return true; // Complex uses floats
        return false;
    },

    /**
     * Finite predicate.
     * @param {number} x - Number to check.
     * @returns {boolean} True if x is finite.
     */
    'finite?': (x) => {
        if (typeof x === 'number') return Number.isFinite(x);
        if (isRational(x)) return true;
        if (isComplex(x)) return Number.isFinite(x.real) && Number.isFinite(x.imag);
        throw new Error('finite?: expected number');
    },

    /**
     * Infinite predicate.
     * @param {number} x - Number to check.
     * @returns {boolean} True if x is infinite.
     */
    'infinite?': (x) => {
        if (typeof x === 'number') return !Number.isFinite(x) && !Number.isNaN(x);
        if (isRational(x)) return false;
        if (isComplex(x)) return !Number.isFinite(x.real) || !Number.isFinite(x.imag);
        throw new Error('infinite?: expected number');
    },

    /**
     * NaN predicate.
     * @param {number} x - Number to check.
     * @returns {boolean} True if x is NaN.
     */
    'nan?': (x) => {
        if (typeof x === 'number') return Number.isNaN(x);
        if (isRational(x)) return false;
        if (isComplex(x)) return Number.isNaN(x.real) || Number.isNaN(x.imag);
        throw new Error('nan?: expected number');
    },

    // =========================================================================
    // Rational Number Procedures
    // =========================================================================

    /**
     * Returns the numerator of a rational.
     * @param {Rational|number} q - Rational number.
     * @returns {number} Numerator.
     */
    'numerator': (q) => {
        if (isRational(q)) return q.numerator;
        if (typeof q === 'number' && Number.isInteger(q)) return q;
        throw new Error('numerator: expected rational number');
    },

    /**
     * Returns the denominator of a rational.
     * @param {Rational|number} q - Rational number.
     * @returns {number} Denominator.
     */
    'denominator': (q) => {
        if (isRational(q)) return q.denominator;
        if (typeof q === 'number' && Number.isInteger(q)) return 1;
        throw new Error('denominator: expected rational number');
    },

    // =========================================================================
    // Complex Number Procedures (scheme complex)
    // =========================================================================

    /**
     * Creates a complex from rectangular coordinates.
     * @param {number} x - Real part.
     * @param {number} y - Imaginary part.
     * @returns {Complex}
     */
    'make-rectangular': (x, y) => {
        assertNumber('make-rectangular', 1, x);
        assertNumber('make-rectangular', 2, y);
        const realVal = typeof x === 'number' ? x : x.toNumber();
        const imagVal = typeof y === 'number' ? y : y.toNumber();
        return makeRectangular(realVal, imagVal);
    },

    /**
     * Creates a complex from polar coordinates.
     * @param {number} r - Magnitude.
     * @param {number} theta - Angle in radians.
     * @returns {Complex}
     */
    'make-polar': (r, theta) => {
        assertNumber('make-polar', 1, r);
        assertNumber('make-polar', 2, theta);
        const mag = typeof r === 'number' ? r : r.toNumber();
        const ang = typeof theta === 'number' ? theta : theta.toNumber();
        return makePolar(mag, ang);
    },

    /**
     * Returns the real part of a complex number.
     * @param {Complex|number} z - Complex number.
     * @returns {number}
     */
    'real-part': (z) => {
        if (isComplex(z)) return z.real;
        if (typeof z === 'number') return z;
        if (isRational(z)) return z.toNumber();
        throw new Error('real-part: expected number');
    },

    /**
     * Returns the imaginary part of a complex number.
     * @param {Complex|number} z - Complex number.
     * @returns {number}
     */
    'imag-part': (z) => {
        if (isComplex(z)) return z.imag;
        if (typeof z === 'number') return 0;
        if (isRational(z)) return 0;
        throw new Error('imag-part: expected number');
    },

    /**
     * Returns the magnitude of a complex number.
     * @param {Complex|number} z - Complex number.
     * @returns {number}
     */
    'magnitude': (z) => {
        if (isComplex(z)) return z.magnitude();
        if (typeof z === 'number') return Math.abs(z);
        if (isRational(z)) return Math.abs(z.toNumber());
        throw new Error('magnitude: expected number');
    },

    /**
     * Returns the angle (argument) of a complex number.
     * @param {Complex|number} z - Complex number.
     * @returns {number}
     */
    'angle': (z) => {
        if (isComplex(z)) return z.angle();
        if (typeof z === 'number') return z >= 0 ? 0 : Math.PI;
        if (isRational(z)) return z.toNumber() >= 0 ? 0 : Math.PI;
        throw new Error('angle: expected number');
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
        // Handle Rational numbers
        if (isRational(x)) {
            x = x.toNumber();
        }
        assertNumber('floor', 1, x);
        return Math.floor(x);
    },

    /**
     * Ceiling (smallest integer >= x).
     * @param {number} x - Number.
     * @returns {number} Ceiling of x.
     */
    'ceiling': (x) => {
        // Handle Rational numbers
        if (isRational(x)) {
            x = x.toNumber();
        }
        assertNumber('ceiling', 1, x);
        return Math.ceil(x);
    },

    /**
     * Truncate (integer part, toward zero).
     * @param {number} x - Number.
     * @returns {number} Truncated value.
     */
    'truncate': (x) => {
        // Handle Rational numbers
        if (isRational(x)) {
            x = x.toNumber();
        }
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

    /**
     * Returns the square of a number.
     * @param {number} z - Number to square.
     * @returns {number} z * z
     */
    'square': (z) => {
        assertNumber('square', 1, z);
        if (isComplex(z)) {
            // (a+bi)^2 = a^2 - b^2 + 2abi
            const a = z.real, b = z.imag;
            return makeRectangular(a * a - b * b, 2 * a * b);
        }
        return z * z;
    },

    /**
     * Converts a number to its inexact equivalent.
     * @param {number|Rational} z - Number to convert.
     * @returns {number} Inexact equivalent.
     */
    'inexact': (z) => {
        // Handle Rational before assertNumber (which only accepts JS numbers)
        if (isRational(z)) return z.toNumber();
        assertNumber('inexact', 1, z);
        if (typeof z === 'number') return z + 0.0; // Force float
        if (isComplex(z)) return z; // Already inexact
        return z;
    },

    /**
     * Converts a number to its exact equivalent if possible.
     * @param {number} z - Number to convert.
     * @returns {number|Rational} Exact equivalent.
     */
    'exact': (z) => {
        assertNumber('exact', 1, z);
        if (typeof z === 'number') {
            if (Number.isInteger(z)) return z;
            // Try to convert to rational if possible
            // For now just return integer part for non-integers
            return Math.round(z);
        }
        if (isRational(z)) return z; // Already exact
        if (isComplex(z)) {
            throw new Error('exact: cannot convert complex to exact');
        }
        return z;
    },
};
