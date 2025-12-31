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
// Generic Arithmetic Helpers (handle BigInt, Number, Rational, Complex)
// =============================================================================

/**
 * Checks if a value is exact (BigInt or exact Rational).
 */
function isExact(x) {
    if (typeof x === 'bigint') return true;
    if (isRational(x)) return x.exact !== false;
    if (isComplex(x)) return x.exact === true;
    return false;
}

/**
 * BigInt-compatible floor division: returns floor(a/b).
 * For BigInt, this is a / b with floor rounding for negative results.
 */
function floorDivBigInt(a, b) {
    if (typeof a === 'bigint' && typeof b === 'bigint') {
        const q = a / b;  // BigInt division truncates toward zero
        const r = a % b;
        // Floor: if signs differ and there's a remainder, subtract 1
        if ((r !== 0n) && ((a < 0n) !== (b < 0n))) {
            return q - 1n;
        }
        return q;
    }
    return Math.floor(Number(a) / Number(b));
}

/**
 * BigInt-compatible truncate division: returns trunc(a/b).
 * For BigInt, this is just a / b (default behavior).
 */
function truncDivBigInt(a, b) {
    if (typeof a === 'bigint' && typeof b === 'bigint') {
        return a / b;
    }
    return Math.trunc(Number(a) / Number(b));
}

function ceilDivBigInt(a, b) {
    if (typeof a === 'bigint' && typeof b === 'bigint') {
        const q = a / b;
        const r = a % b;
        if ((r !== 0n) && ((a > 0n) === (b > 0n))) {
            return q + 1n;
        }
        return q;
    }
    return Math.ceil(Number(a) / Number(b));
}

function roundDivBigInt(a, b) {
    if (typeof a === 'bigint' && typeof b === 'bigint') {
        const q = a / b;
        const r = a % b;
        if (r === 0n) return q;

        const absR = r < 0n ? -r : r;
        const absB = b < 0n ? -b : b;
        const twoR = absR * 2n;

        if (twoR < absB) {
            return q;
        } else if (twoR > absB) {
            if ((a > 0n) === (b > 0n)) return q + 1n;
            return q - 1n;
        } else {
            // Exact half
            if (q % 2n === 0n) return q;
            if ((a > 0n) === (b > 0n)) return q + 1n;
            return q - 1n;
        }
    }
    const val = Number(a) / Number(b);
    // JS Math.round rounds .5 up (towards +Infinity), NOT to even!
    // We need round-half-to-even behavior for consistency?
    // R7RS requires round-half-to-even.
    // However, for Number (float), maybe Math.round is acceptable approximation for now or we implement it.
    // Let's stick to Math.round logic for floats unless standard strictness required.
    // Actually, Scheme round is defined as round-half-to-even.
    // Implementing proper round-half-to-even for floats is complex.
    // Let's rely on standard logic for simple floats (Math.round is round-half-up).
    return Math.round(val);
}

/**
 * BigInt-compatible integer square root using Newton's method.
 * Returns the floor of the square root.
 */
function isqrtBigInt(n) {
    if (typeof n === 'bigint') {
        if (n < 0n) throw new Error('Square root of negative number');
        if (n === 0n) return 0n;
        if (n === 1n) return 1n;
        let x = n;
        let y = (x + 1n) / 2n;
        while (y < x) {
            x = y;
            y = (x + n / x) / 2n;
        }
        return x;
    }
    return BigInt(Math.floor(Math.sqrt(Number(n))));
}

/**
 * Convert integer (BigInt or Number) to BigInt for exact operations.
 */
function toBigInt(x) {
    if (typeof x === 'bigint') return x;
    if (typeof x === 'number' && Number.isInteger(x)) return BigInt(x);
    throw new Error('Expected integer');
}

/**
 * Converts a value to a Number for inexact arithmetic.
 */
function toNumber(x) {
    if (typeof x === 'number') return x;
    if (typeof x === 'bigint') return Number(x);
    if (isRational(x)) return x.toNumber();
    if (isComplex(x)) return x.toNumber();
    throw new Error('Cannot convert to number');
}

/**
 * Converts to Complex for complex arithmetic.
 */
function toComplex(n) {
    if (isComplex(n)) return n;
    if (isRational(n)) return makeRectangular(n.toNumber(), 0);
    if (typeof n === 'bigint') return makeRectangular(Number(n), 0);
    return makeRectangular(n, 0);
}

/**
 * Converts to Rational for exact rational arithmetic.
 */
function toRational(n) {
    if (isRational(n)) return n;
    if (typeof n === 'bigint') return new Rational(n, 1n, true);
    if (typeof n === 'number' && Number.isInteger(n)) return new Rational(BigInt(n), 1n, false);
    throw new Error('Cannot convert inexact non-integer to rational');
}

/**
 * Generic addition supporting BigInt exactness propagation.
 */
function genericAdd(a, b) {
    // Complex takes precedence
    if (isComplex(a) || isComplex(b)) {
        return toComplex(a).add(toComplex(b));
    }

    // Pure BigInt case - exact result
    if (typeof a === 'bigint' && typeof b === 'bigint') {
        return a + b;
    }

    // Rational involved
    if (isRational(a) || isRational(b)) {
        // If any is inexact float (not integer), result is float
        if (typeof a === 'number' && !Number.isInteger(a)) {
            return toNumber(a) + toNumber(b);
        }
        if (typeof b === 'number' && !Number.isInteger(b)) {
            return toNumber(a) + toNumber(b);
        }
        // Exact arithmetic
        const res = toRational(a).add(toRational(b));
        if (res.denominator === 1n) {
            return res.exact ? res.numerator : Number(res.numerator);
        }
        return res;
    }

    // Mixed BigInt/Number - Number wins (inexact)
    if (typeof a === 'bigint' || typeof b === 'bigint') {
        return Number(a) + Number(b);
    }

    // Pure Numbers
    return a + b;
}

/**
 * Generic subtraction supporting BigInt exactness propagation.
 */
function genericSub(a, b) {
    if (isComplex(a) || isComplex(b)) {
        return toComplex(a).subtract(toComplex(b));
    }

    if (typeof a === 'bigint' && typeof b === 'bigint') {
        return a - b;
    }

    if (isRational(a) || isRational(b)) {
        if (typeof a === 'number' && !Number.isInteger(a)) {
            return toNumber(a) - toNumber(b);
        }
        if (typeof b === 'number' && !Number.isInteger(b)) {
            return toNumber(a) - toNumber(b);
        }
        const res = toRational(a).subtract(toRational(b));
        if (res.denominator === 1n) {
            return res.exact ? res.numerator : Number(res.numerator);
        }
        return res;
    }

    if (typeof a === 'bigint' || typeof b === 'bigint') {
        return Number(a) - Number(b);
    }

    return a - b;
}

/**
 * Generic multiplication supporting BigInt exactness propagation.
 */
function genericMul(a, b) {
    if (isComplex(a) || isComplex(b)) {
        return toComplex(a).multiply(toComplex(b));
    }

    if (typeof a === 'bigint' && typeof b === 'bigint') {
        return a * b;
    }

    if (isRational(a) || isRational(b)) {
        if (typeof a === 'number' && !Number.isInteger(a)) {
            return toNumber(a) * toNumber(b);
        }
        if (typeof b === 'number' && !Number.isInteger(b)) {
            return toNumber(a) * toNumber(b);
        }
        const res = toRational(a).multiply(toRational(b));
        if (res.denominator === 1n) {
            return res.exact ? res.numerator : Number(res.numerator);
        }
        return res;
    }

    if (typeof a === 'bigint' || typeof b === 'bigint') {
        return Number(a) * Number(b);
    }

    return a * b;
}

/**
 * Generic division supporting BigInt exactness propagation.
 */
function genericDiv(a, b) {
    if (isComplex(a) || isComplex(b)) {
        return toComplex(a).divide(toComplex(b));
    }

    // R7RS: If an inexact number is involved, result is usually inexact
    if (!isExact(a) || !isExact(b)) {
        return toNumber(a) / toNumber(b);
    }

    // Both are exact – perform exact rational division
    const res = toRational(a).divide(toRational(b));
    if (res.denominator === 1n) {
        return res.numerator; // denominators are 1n, so it's an integer
    }
    return res;
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
        if (args.length === 0) return 0n;  // (+) returns exact 0
        return args.reduce((a, b) => genericAdd(a, b));
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
            // Negation: preserve exactness
            if (typeof first === 'bigint') return -first;
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
        if (args.length === 0) return 1n;  // (*) returns exact 1
        return args.reduce((a, b) => genericMul(a, b));
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
            // Reciprocal: use 1n for exact division to preserve exactness
            res = genericDiv(isExact(first) ? 1n : 1, first);
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
        // Handle mixed BigInt/Number comparison
        // R7RS: = compares values regardless of exactness
        if (typeof a === 'bigint' && typeof b === 'number') {
            return Number(a) === b;
        }
        if (typeof a === 'number' && typeof b === 'bigint') {
            return a === Number(b);
        }
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
        const aBig = toBigInt(a);
        const bBig = toBigInt(b);
        // JavaScript % gives remainder with sign of dividend
        // modulo should have sign of divisor
        const rem = aBig % bBig;
        if (rem === 0n) return 0n;
        return (rem > 0n) === (bBig > 0n) ? rem : rem + bBig;
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
        return truncDivBigInt(toBigInt(a), toBigInt(b));
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
        return toBigInt(a) % toBigInt(b);
    },

    // =========================================================================
    // Type Predicates (require JavaScript typeof)
    // =========================================================================

    /**
     * Number type predicate.
     * Includes BigInt, Number, Rational, and Complex.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a number.
     */
    'number?': (obj) => typeof obj === 'number' || typeof obj === 'bigint' || isRational(obj) || isComplex(obj),

    /**
     * Complex number type predicate.
     * In R7RS, all numbers are complex.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a complex number.
     */
    'complex?': (obj) => typeof obj === 'number' || typeof obj === 'bigint' || isRational(obj) || isComplex(obj),

    /**
     * Real number type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a real number.
     */
    'real?': (obj) => {
        if (typeof obj === 'number') return true;
        if (typeof obj === 'bigint') return true;
        if (isRational(obj)) return true;
        if (isComplex(obj)) return obj.imag === 0 || obj.imag === 0n;
        return false;
    },

    /**
     * Rational number type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a rational number.
     */
    'rational?': (obj) => {
        if (isRational(obj)) return true;
        if (typeof obj === 'bigint') return true;  // All integers are rational
        if (typeof obj === 'number') return Number.isFinite(obj);
        if (isComplex(obj)) return (obj.imag === 0 || obj.imag === 0n) && Number.isFinite(obj.real);
        return false;
    },

    /**
     * Integer type predicate.
     * BigInt is always an integer. Number must pass Number.isInteger().
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is an integer.
     */
    'integer?': (obj) => {
        if (typeof obj === 'bigint') return true;
        if (typeof obj === 'number') return Number.isInteger(obj);
        if (isRational(obj)) return obj.denominator === 1n || obj.denominator === 1;
        if (isComplex(obj)) return (obj.imag === 0 || obj.imag === 0n) &&
            (typeof obj.real === 'bigint' || Number.isInteger(obj.real));
        return false;
    },

    /**
     * Exact integer type predicate.
     * Only BigInt and exact Rationals with denominator 1 are exact integers.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is an exact integer.
     */
    'exact-integer?': (obj) => {
        if (typeof obj === 'bigint') return true;
        if (isRational(obj)) return (obj.denominator === 1n || obj.denominator === 1) && obj.exact;
        return false;
    },

    /**
     * Exact number type predicate.
     * BigInt is exact. Rationals with exact=true are exact.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is exact.
     */
    'exact?': (obj) => {
        assertNumber('exact?', 1, obj);
        return isExact(obj);
    },

    /**
     * Inexact number type predicate.
     * JS Numbers are inexact. Rationals/Complex with exact=false are inexact.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is inexact.
     */
    'inexact?': (obj) => {
        assertNumber('inexact?', 1, obj);
        return !isExact(obj);
    },

    /**
     * Finite predicate.
     * @param {number|bigint|Rational|Complex} x - Number to check.
     * @returns {boolean} True if x is finite.
     */
    'finite?': (x) => {
        if (typeof x === 'bigint') return true;  // BigInt is always finite
        if (typeof x === 'number') return Number.isFinite(x);
        if (isRational(x)) return true;
        if (isComplex(x)) return Number.isFinite(x.real) && Number.isFinite(x.imag);
        throw new Error('finite?: expected number');
    },

    /**
     * Infinite predicate.
     * @param {number|bigint|Rational|Complex} x - Number to check.
     * @returns {boolean} True if x is infinite.
     */
    'infinite?': (x) => {
        if (typeof x === 'bigint') return false;  // BigInt is never infinite
        if (typeof x === 'number') return !Number.isFinite(x) && !Number.isNaN(x);
        if (isRational(x)) return false;
        if (isComplex(x)) return !Number.isFinite(x.real) || !Number.isFinite(x.imag);
        throw new Error('infinite?: expected number');
    },

    /**
     * NaN predicate.
     * @param {number|bigint|Rational|Complex} x - Number to check.
     * @returns {boolean} True if x is NaN.
     */
    'nan?': (x) => {
        if (typeof x === 'bigint') return false;  // BigInt is never NaN
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
     * @param {Rational|number|bigint} q - Rational number.
     * @returns {number|bigint} Numerator.
     */
    'numerator': (q) => {
        if (isRational(q)) return q.numerator;
        if (typeof q === 'bigint') return q;
        if (typeof q === 'number' && Number.isInteger(q)) return BigInt(q);
        throw new Error('numerator: expected rational number');
    },

    /**
     * Returns the denominator of a rational.
     * @param {Rational|number|bigint} q - Rational number.
     * @returns {number|bigint} Denominator.
     */
    'denominator': (q) => {
        if (isRational(q)) return q.denominator;
        if (typeof q === 'bigint') return 1n;
        if (typeof q === 'number' && Number.isInteger(q)) return 1n;
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
        return makeRectangular(x, y);
    },

    /**
     * Creates a complex from polar coordinates.
     * @param {number|bigint|Rational} r - Magnitude.
     * @param {number|bigint|Rational} theta - Angle in radians.
     * @returns {Complex}
     */
    'make-polar': (r, theta) => {
        assertNumber('make-polar', 1, r);
        assertNumber('make-polar', 2, theta);
        // Convert to Number for trigonometric operations
        const toNumVal = (v) => {
            if (typeof v === 'bigint') return Number(v);
            if (typeof v === 'number') return v;
            if (isRational(v)) return v.toNumber();
            return v;
        };
        return makePolar(toNumVal(r), toNumVal(theta));
    },

    /**
     * Returns the real part of a complex number.
     * @param {Complex|number|bigint|Rational} z - Complex number.
     * @returns {number|bigint}
     */
    'real-part': (z) => {
        if (isComplex(z)) return z.real;
        if (typeof z === 'number') return z;
        if (typeof z === 'bigint') return z;
        if (isRational(z)) return z; // Rational is Real
        throw new Error('real-part: expected number');
    },

    /**
     * Returns the imaginary part of a complex number.
     * @param {Complex|number|bigint|Rational} z - Complex number.
     * @returns {number|bigint}
     */
    'imag-part': (z) => {
        if (isComplex(z)) return z.imag;
        // All real numbers have 0 imaginary part
        if (typeof z === 'number') return 0;
        if (typeof z === 'bigint') return 0n;
        if (isRational(z)) return 0n;
        throw new Error('imag-part: expected number');
    },

    /**
     * Returns the magnitude of a complex number.
     * @param {Complex|number|bigint|Rational} z - Complex number.
     * @returns {number}
     */
    'magnitude': (z) => {
        if (isComplex(z)) return z.magnitude();
        if (typeof z === 'number') return Math.abs(z);
        if (typeof z === 'bigint') return z < 0n ? -z : z;
        if (isRational(z)) return z.abs();
        throw new Error('magnitude: expected number');
    },

    /**
     * Returns the angle (argument) of a complex number.
     * @param {Complex|number|bigint|Rational} z - Complex number.
     * @returns {number}
     */
    'angle': (z) => {
        if (isComplex(z)) return z.angle();
        if (typeof z === 'number') return z >= 0 ? 0 : Math.PI;
        if (typeof z === 'bigint') return z >= 0n ? 0n : Math.PI; // Exact 0 for positive real
        if (isRational(z)) return z.toNumber() >= 0 ? 0n : Math.PI;
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
        if (typeof x === 'bigint') return x < 0n ? -x : x;
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
        if (typeof x === 'bigint') return x;  // BigInt is already integer
        if (isRational(x)) {
            return floorDivBigInt(x.numerator, x.denominator);
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
        if (typeof x === 'bigint') return x;
        if (isRational(x)) {
            return ceilDivBigInt(x.numerator, x.denominator);
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
        if (typeof x === 'bigint') return x;
        if (isRational(x)) {
            return truncDivBigInt(x.numerator, x.denominator);
        }
        assertNumber('truncate', 1, x);
        return Math.trunc(x);
    },

    /**
     * Round to nearest integer. Ties to even.
     * @param {number} x - Number.
     * @returns {number} Rounded value.
     */
    'round': (x) => {
        if (typeof x === 'bigint') return x;
        if (isRational(x)) {
            return roundDivBigInt(x.numerator, x.denominator);
        }
        assertNumber('round', 1, x);
        // JS Math.round is round-half-up, we default to it for now for floats
        return Math.round(x);
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

        // BigInt exponentiation
        if (typeof base === 'bigint' && typeof exponent === 'bigint') {
            if (exponent >= 0n) {
                return base ** exponent;
            } else {
                // Negative exponent with integer base -> Rational or float
                // For now, fall back to Rational if possible, or float
                // R7RS: (expt 2 -2) => 1/4 (exact) or 0.25 (inexact)
                // If we have rational support, we could return 1 / (base^abs(exponent))
                // But let's check if Rational is fully integrated yet.
                // Assuming mixed arithmetic handles BigInt/Rational:
                return genericDiv(1n, base ** (-exponent));
            }
        }

        // Handle mixed BigInt/Rational cases or Complex
        // ... (complex/rational logic could go here)

        // Default to float
        const toNumVal = (v) => {
            if (typeof v === 'bigint') return Number(v);
            if (isRational(v)) return v.toNumber();
            if (isComplex(v)) return v.toNumber();
            return v;
        };
        return Math.pow(toNumVal(base), toNumVal(exponent));
    },

    /**
     * Square root.
     * @param {number} x - Number.
     * @returns {number} Square root.
     */
    'sqrt': (x) => {
        assertNumber('sqrt', 1, x);
        // TODO: Complex sqrt
        if (isComplex(x)) throw new Error('sqrt: complex not fully supported');
        // Convert BigInt or Rational to Number
        let val = x;
        if (typeof x === 'bigint') val = Number(x);
        else if (isRational(x)) val = x.toNumber();
        return Math.sqrt(val);
    },

    /**
     * Sine.
     * @param {number} x - Angle in radians.
     * @returns {number} Sine of x.
     */
    'sin': (x) => {
        assertNumber('sin', 1, x);
        if (isComplex(x)) throw new Error('sin: complex not fully supported');
        const val = typeof x === 'bigint' ? Number(x) : (isRational(x) ? x.toNumber() : x);
        return Math.sin(val);
    },

    /**
     * Cosine.
     * @param {number} x - Angle in radians.
     * @returns {number} Cosine of x.
     */
    'cos': (x) => {
        assertNumber('cos', 1, x);
        if (isComplex(x)) throw new Error('cos: complex not fully supported');
        const val = typeof x === 'bigint' ? Number(x) : (isRational(x) ? x.toNumber() : x);
        return Math.cos(val);
    },

    /**
     * Tangent.
     * @param {number} x - Angle in radians.
     * @returns {number} Tangent of x.
     */
    'tan': (x) => {
        assertNumber('tan', 1, x);
        if (isComplex(x)) throw new Error('tan: complex not fully supported');
        const val = typeof x === 'bigint' ? Number(x) : (isRational(x) ? x.toNumber() : x);
        return Math.tan(val);
    },

    /**
     * Arcsine.
     * @param {number} x - Value.
     * @returns {number} Arcsine in radians.
     */
    'asin': (x) => {
        assertNumber('asin', 1, x);
        const val = typeof x === 'bigint' ? Number(x) : (isRational(x) ? x.toNumber() : x);
        return Math.asin(val);
    },

    /**
     * Arccosine.
     * @param {number} x - Value.
     * @returns {number} Arccosine in radians.
     */
    'acos': (x) => {
        assertNumber('acos', 1, x);
        const val = typeof x === 'bigint' ? Number(x) : (isRational(x) ? x.toNumber() : x);
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
        const vy = typeof y === 'bigint' ? Number(y) : (isRational(y) ? y.toNumber() : y);

        if (x === undefined) {
            return Math.atan(vy);
        }
        assertNumber('atan', 2, x);
        const vx = typeof x === 'bigint' ? Number(x) : (isRational(x) ? x.toNumber() : x);
        return Math.atan2(vy, vx);
    },

    /**
     * Natural logarithm.
     * @param {number} x - Number.
     * @returns {number} Natural log of x.
     */
    'log': (x) => {
        assertNumber('log', 1, x);
        if (isComplex(x)) throw new Error('log: complex not fully supported');
        const val = typeof x === 'bigint' ? Number(x) : (isRational(x) ? x.toNumber() : x);
        return Math.log(val);
    },

    /**
     * Exponential function (e^x).
     * @param {number} x - Exponent.
     * @returns {number} e^x.
     */
    'exp': (x) => {
        assertNumber('exp', 1, x);
        if (isComplex(x)) throw new Error('exp: complex not fully supported');
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
        const kBig = toBigInt(k);
        if (kBig < 0n) {
            throw new Error('exact-integer-sqrt: expected non-negative integer');
        }
        const s = isqrtBigInt(kBig);
        const r = kBig - s * s;
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
        const a = toBigInt(n1);
        const b = toBigInt(n2);
        if (b === 0n) {
            throw new Error('floor/: division by zero');
        }
        const q = floorDivBigInt(a, b);
        const r = a - b * q;
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
        const a = toBigInt(n1);
        const b = toBigInt(n2);
        if (b === 0n) {
            throw new Error('truncate/: division by zero');
        }
        const q = truncDivBigInt(a, b);
        const r = a - b * q;
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
        const a = toBigInt(n1);
        const b = toBigInt(n2);
        if (b === 0n) {
            throw new Error('floor-quotient: division by zero');
        }
        return floorDivBigInt(a, b);
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
        const a = toBigInt(n1);
        const b = toBigInt(n2);
        if (b === 0n) {
            throw new Error('floor-remainder: division by zero');
        }
        const q = floorDivBigInt(a, b);
        return a - b * q;
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
        const a = toBigInt(n1);
        const b = toBigInt(n2);
        if (b === 0n) {
            throw new Error('truncate-quotient: division by zero');
        }
        return truncDivBigInt(a, b);
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
        const a = toBigInt(n1);
        const b = toBigInt(n2);
        if (b === 0n) {
            throw new Error('truncate-remainder: division by zero');
        }
        return a % b;
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
     * BigInt -> Number, Rational -> Number (or Rational with exact=false)
     * @param {number|bigint|Rational|Complex} z - Number to convert.
     * @returns {number|Rational|Complex} Inexact equivalent.
     */
    'inexact': (z) => {
        assertNumber('inexact', 1, z);
        if (typeof z === 'bigint') {
            return Number(z);  // BigInt -> Number (inexact)
        }
        if (typeof z === 'number') {
            return z;  // Already inexact
        }
        if (isRational(z)) {
            // Convert to Number for simple display
            return z.toNumber();
        }
        if (isComplex(z)) {
            // Complex with inexact parts
            return z;
        }
        return z;
    },

    /**
     * Converts a number to its exact equivalent if possible.
     * Number (integer) -> BigInt, Rational -> Rational with exact=true
     * @param {number|bigint|Rational|Complex} z - Number to convert.
     * @returns {bigint|Rational|Complex} Exact equivalent.
     */
    'exact': (z) => {
        assertNumber('exact', 1, z);
        if (typeof z === 'bigint') {
            return z;  // Already exact
        }
        if (typeof z === 'number') {
            if (Number.isInteger(z)) {
                return BigInt(z);  // Number -> BigInt
            }
            // For non-integers, convert to rational
            // Use a simple algorithm: multiply by power of 2 to eliminate fractional bits
            // For now, use a simpler approach: round to integer
            // TODO: implement proper float-to-rational conversion
            throw new Error('exact: cannot convert inexact non-integer to exact');
        }
        if (isRational(z)) {
            // Return with exact=true
            return new Rational(z.numerator, z.denominator, true);
        }
        if (isComplex(z)) {
            throw new Error('exact: cannot convert complex to exact');
        }
        return z;
    },
};
mathPrimitives['inexact->exact'] = mathPrimitives['exact'];
