/**
 * Math Primitives for Scheme.
 * 
 * Provides arithmetic and comparison operations for the Scheme runtime.
 * Implements R7RS §6.2 numeric operations.
 * 
 * NOTE: Only primitives that REQUIRE JavaScript are implemented here.
 * Higher-level numeric procedures are in core/primitives/scheme
 */

import { assertNumber, assertInteger, assertArity } from '../interpreter/type_check.js';
import { Values } from '../interpreter/values.js';
import { Rational, isRational } from './rational.js';
import { Complex, isComplex, makeRectangular, makePolar } from './complex.js';
import { SchemeTypeError, SchemeRangeError, SchemeError } from '../interpreter/errors.js';

// =============================================================================
// Generic Arithmetic Helpers
// =============================================================================

function toComplex(n) {
    if (isComplex(n)) return n;
    if (isRational(n)) return makeRectangular(n.toNumber(), 0);
    return makeRectangular(n, 0);
}

function toRational(n) {
    if (isRational(n)) return n;
    if (Number.isInteger(n)) return Rational.fromNumber(n);
    throw new SchemeTypeError('toRational', 1, 'exact integer', n);
}

function genericAdd(a, b) {
    // If either is complex, result is complex (inexact in our implementation)
    if (isComplex(a) || isComplex(b)) {
        return toComplex(a).add(toComplex(b));
    }
    // If either is rational or both are integers
    if (isRational(a) || isRational(b)) {
        // If any is inexact float, result is float
        if ((typeof a === 'number' && !Number.isInteger(a)) ||
            (typeof b === 'number' && !Number.isInteger(b))) {
            return (isRational(a) ? a.toNumber() : a) + (isRational(b) ? b.toNumber() : b);
        }
        // Exact arithmetic
        const res = toRational(a).add(toRational(b));
        return res.denominator === 1 ? res.numerator : res;
    }
    // Primitive numbers
    return a + b;
}

function genericSub(a, b) {
    if (isComplex(a) || isComplex(b)) {
        return toComplex(a).subtract(toComplex(b));
    }
    if (isRational(a) || isRational(b)) {
        if ((typeof a === 'number' && !Number.isInteger(a)) ||
            (typeof b === 'number' && !Number.isInteger(b))) {
            return (isRational(a) ? a.toNumber() : a) - (isRational(b) ? b.toNumber() : b);
        }
        const res = toRational(a).subtract(toRational(b));
        return res.denominator === 1 ? res.numerator : res;
    }
    return a - b;
}

function genericMul(a, b) {
    if (isComplex(a) || isComplex(b)) {
        return toComplex(a).multiply(toComplex(b));
    }
    if (isRational(a) || isRational(b)) {
        if ((typeof a === 'number' && !Number.isInteger(a)) ||
            (typeof b === 'number' && !Number.isInteger(b))) {
            return (isRational(a) ? a.toNumber() : a) * (isRational(b) ? b.toNumber() : b);
        }
        const res = toRational(a).multiply(toRational(b));
        return res.denominator === 1 ? res.numerator : res;
    }
    return a * b;
}

function genericDiv(a, b) {
    if (isComplex(a) || isComplex(b)) {
        return toComplex(a).divide(toComplex(b));
    }
    // Division involves rationals if explicit or if both are integers (and result not integer)
    // Actually standard division of integers implies Rational result in Scheme unless Inexact
    if ((typeof a === 'number' && !Number.isInteger(a)) ||
        (typeof b === 'number' && !Number.isInteger(b))) {
        // Floating point division
        const va = isRational(a) ? a.toNumber() : a;
        const vb = isRational(b) ? b.toNumber() : b;
        return va / vb;
    }
    // Exact division (Integers or Rationals)
    const res = toRational(a).divide(toRational(b));
    return res.denominator === 1 ? res.numerator : res;
}

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
        return args.reduce((a, b) => genericAdd(a, b), 0);
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
        if (rest.length === 0) {
            // Negation
            return genericSub(0, first);
        }
        return rest.reduce((a, b) => genericSub(a, b), first);
    },

    /**
     * Multiplication. Returns the product of all arguments.
     * @param {...number} args - Numbers to multiply.
     * @returns {number} Product of all arguments.
     */
    '*': (...args) => {
        args.forEach((arg, i) => assertNumber('*', i + 1, arg));
        return args.reduce((a, b) => genericMul(a, b), 1);
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
        let res;
        if (rest.length === 0) {
            // Reciprocal
            res = genericDiv(1, first);
        } else {
            res = rest.reduce((a, b) => genericDiv(a, b), first);
        }
        if (res instanceof Rational) return res;
        return res;
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
        throw new SchemeTypeError('finite?', 1, 'number', x);
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
        throw new SchemeTypeError('infinite?', 1, 'number', x);
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
        throw new SchemeTypeError('nan?', 1, 'number', x);
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
        throw new SchemeTypeError('numerator', 1, 'rational number', q);
    },

    /**
     * Returns the denominator of a rational.
     * @param {Rational|number} q - Rational number.
     * @returns {number} Denominator.
     */
    'denominator': (q) => {
        if (isRational(q)) return q.denominator;
        if (typeof q === 'number' && Number.isInteger(q)) return 1;
        throw new SchemeTypeError('denominator', 1, 'rational number', q);
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
        return makeRectangular(x, y);
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
        throw new SchemeTypeError('real-part', 1, 'number', z);
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
        throw new SchemeTypeError('imag-part', 1, 'number', z);
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
        throw new SchemeTypeError('magnitude', 1, 'number', z);
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
        throw new SchemeTypeError('angle', 1, 'number', z);
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
        if (isComplex(x)) return x.magnitude();
        if (isRational(x)) return x.abs();
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

        // Convert to floats for now unless we implement exact expt
        const b = isRational(base) ? base.toNumber() : (isComplex(base) ? base.toNumber() : base); // Complex toNumber throws if not real
        const e = isRational(exponent) ? exponent.toNumber() : (isComplex(exponent) ? exponent.toNumber() : exponent);

        return Math.pow(b, e);
    },

    /**
     * Square root.
     * @param {number} x - Number.
     * @returns {number} Square root.
     */
    'sqrt': (x) => {
        assertNumber('sqrt', 1, x);
        // TODO: Complex sqrt
        if (isComplex(x)) throw new SchemeError('sqrt: complex not fully supported', [x], 'sqrt');
        const val = isRational(x) ? x.toNumber() : x; // Returns inexact
        return Math.sqrt(val);
    },

    /**
     * Sine.
     * @param {number} x - Angle in radians.
     * @returns {number} Sine of x.
     */
    'sin': (x) => {
        assertNumber('sin', 1, x);
        if (isComplex(x)) throw new SchemeError('sin: complex not fully supported', [x], 'sin');
        const val = isRational(x) ? x.toNumber() : x;
        return Math.sin(val);
    },

    /**
     * Cosine.
     * @param {number} x - Angle in radians.
     * @returns {number} Cosine of x.
     */
    'cos': (x) => {
        assertNumber('cos', 1, x);
        if (isComplex(x)) throw new SchemeError('cos: complex not fully supported', [x], 'cos');
        const val = isRational(x) ? x.toNumber() : x;
        return Math.cos(val);
    },

    /**
     * Tangent.
     * @param {number} x - Angle in radians.
     * @returns {number} Tangent of x.
     */
    'tan': (x) => {
        assertNumber('tan', 1, x);
        if (isComplex(x)) throw new SchemeError('tan: complex not fully supported', [x], 'tan');
        const val = isRational(x) ? x.toNumber() : x;
        return Math.tan(val);
    },

    /**
     * Arcsine.
     * @param {number} x - Value.
     * @returns {number} Arcsine in radians.
     */
    'asin': (x) => {
        assertNumber('asin', 1, x);
        const val = isRational(x) ? x.toNumber() : x;
        return Math.asin(val);
    },

    /**
     * Arccosine.
     * @param {number} x - Value.
     * @returns {number} Arccosine in radians.
     */
    'acos': (x) => {
        assertNumber('acos', 1, x);
        const val = isRational(x) ? x.toNumber() : x;
        return Math.acos(val);
    },

    /**
     * Arctangent. With two arguments, returns atan2(y, x).
     * @param {number} y - Y value (or angle if single arg).
     * @param {number} [x] - X value (optional).
     * @returns {number} Arctangent in radians.
     */
    'atan': (y, x) => {
        assertNumber('atan', 1, y);
        const vy = isRational(y) ? y.toNumber() : y;

        if (x === undefined) {
            return Math.atan(vy);
        }
        assertNumber('atan', 2, x);
        const vx = isRational(x) ? x.toNumber() : x;
        return Math.atan2(vy, vx);
    },

    /**
     * Natural logarithm.
     * @param {number} x - Number.
     * @returns {number} Natural log of x.
     */
    'log': (x) => {
        assertNumber('log', 1, x);
        if (isComplex(x)) throw new SchemeError('log: complex not fully supported', [x], 'log');
        const val = isRational(x) ? x.toNumber() : x;
        return Math.log(val);
    },

    /**
     * Exponential function (e^x).
     * @param {number} x - Exponent.
     * @returns {number} e^x.
     */
    'exp': (x) => {
        assertNumber('exp', 1, x);
        if (isComplex(x)) throw new SchemeError('exp: complex not fully supported', [x], 'exp');
        const val = isRational(x) ? x.toNumber() : x;
        return Math.exp(val);
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
            throw new SchemeRangeError('exact-integer-sqrt', 'k', 0, Infinity, k);
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
            throw new SchemeRangeError('floor/', 'divisor', -Infinity, -1, 0);
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
            throw new SchemeRangeError('truncate/', 'divisor', -Infinity, -1, 0);
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
            throw new SchemeRangeError('floor-quotient', 'divisor', -Infinity, -1, 0);
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
            throw new SchemeRangeError('floor-remainder', 'divisor', -Infinity, -1, 0);
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
            throw new SchemeRangeError('truncate-quotient', 'divisor', -Infinity, -1, 0);
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
            throw new SchemeRangeError('truncate-remainder', 'divisor', -Infinity, -1, 0);
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
            throw new SchemeTypeError('exact', 1, 'real number', z);
        }
        return z;
    },
};
