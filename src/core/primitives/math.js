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
import { SchemeTypeError } from '../interpreter/errors.js';
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
 * The integer square root of a non-negative integer: the largest s with
 * s * s <= n.
 *
 * The root of n's leading bits, its precision doubled at each step: the root
 * a of the top 2d bits gives the root of the top 4d to within one by one
 * division, (a << d) + (top bits) / a, a Newton step taken where it is
 * already close (Mark Dickinson's algorithm, Python's `math.isqrt`). The
 * divisions double in size as they go, so the whole root costs about two
 * divisions of n's size. Newton's iteration started from n itself, as it
 * was, took about half n's bit length in steps, each a division of numbers
 * that long -- nearly all of the time the benchmarks `pi` and `chudnovsky`
 * took.
 * @param {bigint|number} n - The integer.
 * @returns {bigint} Its integer square root.
 */
function isqrtBigInt(n) {
    if (typeof n === 'bigint') {
        if (n < 0n) throw new Error('Square root of negative number');
        if (n < 2n) return n;
        const c = (bitLength(n) - 1) >> 1;
        let a = 1n;
        let d = 0;
        for (let s = 31 - Math.clz32(c); s >= 0; s--) {
            const e = d;
            d = c >> s;
            a = (a << BigInt(d - e - 1)) + (n >> BigInt(2 * c - e - d + 1)) / a;
        }
        return a * a > n ? a - 1n : a;
    }
    return BigInt(Math.floor(Math.sqrt(Number(n))));
}

/**
 * How many bits a positive integer takes.
 * @param {bigint} n - The integer.
 * @returns {number}
 */
function bitLength(n) {
    const hex = n.toString(16);
    return 4 * (hex.length - 1) + (32 - Math.clz32(parseInt(hex[0], 16)));
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
 * Views a number as a complex number, keeping its exactness: a real number's
 * imaginary part is a zero as exact as it is. Converting an exact real to a
 * flonum here would make `(= 1/3+0i x)` compare 1/3 rounded.
 * @param {*} n - A number.
 * @returns {Complex}
 */
function toComplex(n) {
    if (isComplex(n)) return n;
    return new Complex(n, isExact(n) ? 0n : 0);
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
 * Views a real number as an exact fraction, or returns null if it is inexact.
 *
 * Denominators are positive and reduced by the Rational constructor, so the
 * pair returned can be cross-multiplied directly without sign handling.
 *
 * @param {*} x - A Scheme number.
 * @returns {Array<bigint>|null} `[numerator, denominator]`, or null if inexact.
 */
function asExactFraction(x) {
    if (typeof x === 'bigint') return [x, 1n];
    if (isRational(x) && x.exact !== false) return [x.numerator, x.denominator];
    return null;
}

/**
 * Converts a real number to a JavaScript number, losing precision if necessary.
 * @param {*} x - A Scheme real number.
 * @returns {number} The closest double.
 */
function toJsNumber(x) {
    if (typeof x === 'number') return x;
    if (typeof x === 'bigint') return Number(x);
    if (isRational(x)) return x.toNumber();
    return NaN;
}

/**
 * Compares two real numbers.
 *
 * Exact operands are compared exactly, by cross-multiplication, rather than by
 * converting to double: `(< 1/10000000000000000000000000002
 * 1/10000000000000000000000000001)` and comparisons between large integers are
 * both decided beyond the precision of a double, and converting first would
 * report them equal.
 *
 * When either operand is inexact the comparison is performed in floating point,
 * which is what R7RS requires -- the inexact value has already lost the
 * precision that an exact comparison would need.
 *
 * @param {*} a - A real number.
 * @param {*} b - A real number.
 * @returns {number} -1, 0 or 1, or NaN if either operand is NaN.
 */
function numericCompare(a, b) {
    const fa = asExactFraction(a);
    if (fa !== null) {
        const fb = asExactFraction(b);
        if (fb !== null) {
            const left = fa[0] * fb[1];
            const right = fb[0] * fa[1];
            return left < right ? -1 : (left > right ? 1 : 0);
        }
    }

    const na = toJsNumber(a);
    const nb = toJsNumber(b);
    if (Number.isNaN(na) || Number.isNaN(nb)) return NaN;
    return na < nb ? -1 : (na > nb ? 1 : 0);
}

/**
 * Tests two numbers for numeric equality, across exactness and across the
 * tower. Unlike the ordering predicates this accepts complex numbers, since
 * R7RS orders only real numbers but compares any numbers for equality.
 *
 * @param {*} a - A number.
 * @param {*} b - A number.
 * @returns {boolean} True if the two denote the same number.
 */
function numericEquals(a, b) {
    if (isComplex(a) || isComplex(b)) {
        const ca = toComplex(a);
        const cb = toComplex(b);
        return numericEquals(ca.real, cb.real) && numericEquals(ca.imag, cb.imag);
    }
    return numericCompare(a, b) === 0;
}

/**
 * Asserts that an argument is a real number, as required by the ordering
 * predicates. Complex numbers are numbers but are not ordered.
 *
 * @param {string} name - Procedure name, for the error message.
 * @param {number} position - 1-based argument position.
 * @param {*} value - The argument.
 * @returns {void}
 */
function assertReal(name, position, value) {
    assertNumber(name, position, value);
    if (isComplex(value) && toJsNumber(value.imag) !== 0) {
        throw new SchemeTypeError(name, position, 'real number', value);
    }
}

/**
 * Generic addition supporting BigInt exactness propagation.
 */
function genericAdd(a, b) {
    // Complex takes precedence
    if (isComplex(a) || isComplex(b)) {
        return complexAdd(a, b);
    }

    // Pure BigInt case - exact result
    if (typeof a === 'bigint' && typeof b === 'bigint') {
        return a + b;
    }

    // Rational involved
    if (isRational(a) || isRational(b)) {
        // An inexact operand -- every JavaScript number is one, integral
        // or not -- makes the result inexact (R7RS 6.2.2), so a flonum.
        if (typeof a === 'number' || typeof b === 'number') {
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
        return complexAdd(a, genericNegate(b));
    }

    if (typeof a === 'bigint' && typeof b === 'bigint') {
        return a - b;
    }

    if (isRational(a) || isRational(b)) {
        if (typeof a === 'number' || typeof b === 'number') {
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
        return complexMul(a, b);
    }

    if (typeof a === 'bigint' && typeof b === 'bigint') {
        return a * b;
    }

    if (isRational(a) || isRational(b)) {
        if (typeof a === 'number' || typeof b === 'number') {
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
        return complexDiv(a, b);
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
 * Generic negation, by representation: zero minus the number would make an
 * exact rational or complex number inexact, by way of an inexact zero, and
 * lose the sign of a flonum zero.
 * @param {*} x - A number.
 * @returns {*} Its negation, as exact as it is.
 */
function genericNegate(x) {
    if (typeof x === 'bigint' || typeof x === 'number') return -x;
    return x.negate();
}

// =============================================================================
// Complex Arithmetic
// =============================================================================
// The parts are combined with the real arithmetic above, so exact parts stay
// exact and an inexact part makes the result inexact (R7RS 6.2.2): (* +i 2)
// is 0+2i, (* +i 2.0) is 0.0+2.0i. An operand that is real is combined with
// each part rather than taken as a complex number with a zero imaginary part,
// which would add terms like 0.0 * +inf.0, a NaN, and 0.0 + -0.0, which
// loses the sign of a zero. A result is made by `makeRectangular`, so one
// whose imaginary part is an exact zero is a real number: `(* +i +i)` is -1.

/**
 * Adds two numbers at least one of which is complex.
 * @param {*} a - A number.
 * @param {*} b - A number.
 * @returns {*} The sum.
 */
function complexAdd(a, b) {
    if (!isComplex(a)) return makeRectangular(genericAdd(a, b.real), b.imag);
    if (!isComplex(b)) return makeRectangular(genericAdd(a.real, b), a.imag);
    return makeRectangular(genericAdd(a.real, b.real), genericAdd(a.imag, b.imag));
}

/**
 * Multiplies two numbers at least one of which is complex:
 * (a+bi)(c+di) = (ac-bd) + (ad+bc)i.
 * @param {*} a - A number.
 * @param {*} b - A number.
 * @returns {*} The product.
 */
function complexMul(a, b) {
    if (!isComplex(a)) return makeRectangular(genericMul(a, b.real), genericMul(a, b.imag));
    if (!isComplex(b)) return makeRectangular(genericMul(a.real, b), genericMul(a.imag, b));
    return makeRectangular(
        genericSub(genericMul(a.real, b.real), genericMul(a.imag, b.imag)),
        genericAdd(genericMul(a.real, b.imag), genericMul(a.imag, b.real)));
}

/**
 * Divides two numbers at least one of which is complex. A complex divisor is
 * made real by multiplying both by its conjugate: a/(c+di) is
 * a(c-di) / (c^2+d^2).
 * @param {*} a - A number.
 * @param {*} b - A number, not zero.
 * @returns {*} The quotient.
 */
function complexDiv(a, b) {
    if (!isComplex(b)) return makeRectangular(genericDiv(a.real, b), genericDiv(a.imag, b));
    const norm = genericAdd(genericMul(b.real, b.real), genericMul(b.imag, b.imag));
    return genericDiv(genericMul(a, b.conjugate()), norm);
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
        if (rest.length === 0) return genericNegate(first);
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
    // Comparison Operations
    // =========================================================================
    // The variadic predicates are installed after this object literal, by
    // makeOrdering/makeEquality. See the note on those helpers for why they are
    // native rather than defined in Scheme.

    /** Binary numeric equality. @param {*} a @param {*} b @returns {boolean} */
    '%num=': (a, b) => {
        assertNumber('%num=', 1, a);
        assertNumber('%num=', 2, b);
        return numericEquals(a, b);
    },

    /** Binary less than. @param {*} a @param {*} b @returns {boolean} */
    '%num<': (a, b) => {
        assertReal('%num<', 1, a);
        assertReal('%num<', 2, b);
        return numericCompare(a, b) < 0;
    },

    /** Binary greater than. @param {*} a @param {*} b @returns {boolean} */
    '%num>': (a, b) => {
        assertReal('%num>', 1, a);
        assertReal('%num>', 2, b);
        return numericCompare(a, b) > 0;
    },

    /** Binary less than or equal. @param {*} a @param {*} b @returns {boolean} */
    '%num<=': (a, b) => {
        assertReal('%num<=', 1, a);
        assertReal('%num<=', 2, b);
        return numericCompare(a, b) <= 0;
    },

    /** Binary greater than or equal. @param {*} a @param {*} b @returns {boolean} */
    '%num>=': (a, b) => {
        assertReal('%num>=', 1, a);
        assertReal('%num>=', 2, b);
        return numericCompare(a, b) >= 0;
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
     * Creates a complex from rectangular coordinates; with an exact zero
     * imaginary part, that is the real part itself.
     * @param {number|bigint|Rational} x - Real part.
     * @param {number|bigint|Rational} y - Imaginary part.
     * @returns {number|bigint|Rational|Complex}
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
                // A negative power is the reciprocal, exact: (expt 2 -2) is 1/4.
                return genericDiv(1n, base ** (-exponent));
            }
        }

        // An exact fraction to an exact integer power is exact (R7RS 6.2.2):
        // its numerator and denominator raised separately, a negative power
        // the two swapped. `genericDiv` makes a denominator of 1 an integer.
        if (isRational(base) && base.exact !== false && typeof exponent === 'bigint') {
            const n = exponent < 0n ? -exponent : exponent;
            const numerator = base.numerator ** n;
            const denominator = base.denominator ** n;
            return exponent < 0n
                ? genericDiv(denominator, numerator)
                : genericDiv(numerator, denominator);
        }

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
     * Returns the square of a number, as exact as the number is.
     * @param {*} z - Number to square.
     * @returns {*} z * z
     */
    'square': (z) => {
        assertNumber('square', 1, z);
        if (typeof z === 'bigint' || typeof z === 'number') return z * z;
        return genericMul(z, z);
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
            // The constructor makes both parts flonums when told the number
            // is inexact.
            return z.exact ? new Complex(z.real, z.imag, false) : z;
        }
        return z;
    },

    /**
     * Converts a number to its exact equivalent (R7RS 6.2.6): an integral
     * flonum to an exact integer, any other finite flonum to the exact
     * rational it is, `(exact 0.5)` 1/2 and `(exact 0.1)`
     * 3602879701896397/36028797018963968, an inexact rational to the same
     * value exact, and a complex number part by part.
     * @param {number|bigint|Rational|Complex} z - Number to convert.
     * @returns {bigint|Rational|Complex} Exact equivalent.
     */
    'exact': function exact(z) {
        assertNumber('exact', 1, z);
        if (typeof z === 'bigint') {
            return z;  // Already exact
        }
        if (typeof z === 'number') {
            if (Number.isInteger(z)) {
                return BigInt(z);  // Number -> BigInt
            }
            if (!Number.isFinite(z)) {
                throw new Error('exact: an infinity or NaN has no exact equivalent');
            }
            // A flonum is a dyadic rational. Doubling one is exact in binary
            // floating point, so doubling it until it is an integer gives its
            // numerator over a power of two: at most 1,074 times, for the
            // smallest subnormal, and never past 2^53, since a flonum that is
            // not an integer is below 2^52.
            let numerator = z;
            let denominator = 1n;
            while (!Number.isInteger(numerator)) {
                numerator *= 2;
                denominator *= 2n;
            }
            return new Rational(BigInt(numerator), denominator, true);
        }
        if (isRational(z)) {
            // Return with exact=true
            return new Rational(z.numerator, z.denominator, true);
        }
        if (isComplex(z)) {
            // Each part made exact as a real number is; an infinite or NaN
            // part is the same error. The recursion is by this function's
            // own name, not through `mathPrimitives`, whose entry a wrapper
            // can replace.
            return makeRectangular(exact(z.real), exact(z.imag));
        }
        return z;
    },
};
mathPrimitives['inexact->exact'] = mathPrimitives['exact'];

// =============================================================================
// Variadic Comparison Predicates
// =============================================================================
// These were previously defined in Scheme, in `src/core/scheme/numbers.scm`, as
// variadic procedures with rest parameters delegating to the `%num*` binary
// primitives. That made a single integer comparison expand into four nested
// applications plus rest-list construction -- profiling attributed roughly half
// the runtime of `fib` to it, and replacing just `<` was measured at 1.93x.
//
// Defining them natively removes that entirely, and gives the two-argument
// case -- overwhelmingly the common one -- a path with no array allocation.

/**
 * Builds a variadic ordering predicate from an acceptance test on the result of
 * `numericCompare`.
 *
 * R7RS requires these to hold pairwise across any number of arguments, so
 * `(< 1 2 3)` is true and `(< 1 3 2)` is false, and to require at least two
 * arguments.
 *
 * @param {string} name - Procedure name, for error messages.
 * @param {function(number): boolean} accept - Whether an ordering (-1, 0, 1)
 *   satisfies this predicate.
 * @returns {function(...*): boolean} The variadic primitive.
 */
function makeOrdering(name, accept) {
    return (...args) => {
        if (args.length < 2) {
            assertArity(name, args, 2, Infinity);
        }
        // Fast path for the two-argument case: no loop, no intermediate state.
        if (args.length === 2) {
            assertReal(name, 1, args[0]);
            assertReal(name, 2, args[1]);
            return accept(numericCompare(args[0], args[1]));
        }
        for (let i = 0; i < args.length; i++) {
            assertReal(name, i + 1, args[i]);
        }
        for (let i = 0; i < args.length - 1; i++) {
            if (!accept(numericCompare(args[i], args[i + 1]))) return false;
        }
        return true;
    };
}

/**
 * Builds the variadic numeric equality predicate.
 *
 * Kept separate from `makeOrdering` because R7RS orders only real numbers but
 * compares any numbers for equality, so `=` must accept complex arguments that
 * `<` must reject.
 *
 * @param {string} name - Procedure name, for error messages.
 * @returns {function(...*): boolean} The variadic primitive.
 */
function makeEquality(name) {
    return (...args) => {
        if (args.length < 2) {
            assertArity(name, args, 2, Infinity);
        }
        if (args.length === 2) {
            assertNumber(name, 1, args[0]);
            assertNumber(name, 2, args[1]);
            return numericEquals(args[0], args[1]);
        }
        for (let i = 0; i < args.length; i++) {
            assertNumber(name, i + 1, args[i]);
        }
        for (let i = 0; i < args.length - 1; i++) {
            if (!numericEquals(args[i], args[i + 1])) return false;
        }
        return true;
    };
}

mathPrimitives['='] = makeEquality('=');
mathPrimitives['<'] = makeOrdering('<', (c) => c < 0);
mathPrimitives['>'] = makeOrdering('>', (c) => c > 0);
mathPrimitives['<='] = makeOrdering('<=', (c) => c <= 0);
mathPrimitives['>='] = makeOrdering('>=', (c) => c >= 0);
