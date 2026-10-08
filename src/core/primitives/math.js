/**
 * Math Primitives for Scheme.
 * 
 * Provides arithmetic and comparison operations for the Scheme runtime.
 * Implements R7RS §6.2 numeric operations.
 * 
 * NOTE: Only primitives that REQUIRE JavaScript are implemented here.
 * Higher-level numeric procedures are in core.scm.
 */

import { assertNumber, assertInteger, assertArity, isNumber } from '../interpreter/type_check.js';
import { SchemeError, SchemeTypeError } from '../interpreter/errors.js';
import { Values } from '../interpreter/values.js';
import { Rational, isRational, bitLength, ratioToNumber } from './rational.js';
import { Complex, isComplex, makeRectangular, makePolar } from './complex.js';
import {
    toTower, fromTower, addReals, subReals, mulReals, lessReals, lessEqualReals, equalReals, Flonum,
    inexactReal, heldDouble, mulNumbers
} from '../interpreter/number_representation.js';

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
    if (isComplex(value) && !isExactZero(value.imag)) {
        throw new SchemeTypeError(name, position, 'real number', value);
    }
}

/**
 * Whether a number is an exact zero. A complex number is real only when its
 * imaginary part is one: R7RS 6.2.6 has `(real? -2.5+0i)` true but
 * `(real? -2.5+0.0i)` false.
 * @param {*} x - A real number, a complex number's part.
 * @returns {boolean}
 */
function isExactZero(x) {
    return x === 0n || (isRational(x) && x.exact !== false && x.numerator === 0n);
}

/**
 * Whether a complex number's part is an infinity.
 * @param {*} x - The part: a double, or exact.
 * @returns {boolean}
 */
function isInfinite(x) {
    return x === Infinity || x === -Infinity;
}

/**
 * Whether a complex number's part is an infinity or a NaN.
 * @param {*} x - The part: a double, or exact.
 * @returns {boolean}
 */
function isInfiniteOrNaN(x) {
    return typeof x === 'number' && !Number.isFinite(x);
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

// =============================================================================
// Integer Division Helpers
// =============================================================================

/**
 * Checks the arguments of an integer division (R7RS 6.2.6): two integers, the
 * divisor not zero.
 * @param {string} name - The procedure's name, for the error message.
 * @param {*} n1 - The dividend.
 * @param {*} n2 - The divisor.
 * @returns {void}
 */
function assertDivision(name, n1, n2) {
    assertInteger(name, 1, n1);
    assertInteger(name, 2, n2);
    if (n2 === 0n || n2 === 0) {
        throw new SchemeError(`${name}: division by zero`, [n1, n2], name);
    }
}

/**
 * An integer division's result, computed exactly, made inexact if either
 * argument was: R7RS 6.2.6 has `(truncate/ -5.0 -2)` => 2.0 -1.0. The
 * division is done on BigInts even then, since an inexact integer can be far
 * past 2^53, where a double's remainder is no longer exact.
 * @param {bigint} result - The quotient or remainder.
 * @param {*} n1 - The dividend.
 * @param {*} n2 - The divisor.
 * @returns {bigint|number}
 */
function divisionResult(result, n1, n2) {
    return typeof n1 === 'number' || typeof n2 === 'number' ? Number(result) : result;
}

// =============================================================================
// Functions of Real Numbers
// =============================================================================
// The transcendental functions and `expt` are JavaScript's Math functions,
// which take doubles, so an exact argument is converted to the double nearest
// it, by `toNumber`. Two kinds of argument need more than that: an exact
// number too large or too small for a double, whose logarithm, square root and
// powers are computed from its exact value, and a real number whose value is
// not real -- the square root of -4 -- which R7RS 6.2.6 defines as a complex
// number and Math as NaN. R7RS 6.2.4 allows the NaN only where there are no
// complex numbers.

/**
 * The real number a function of real numbers is given: a complex argument
 * takes the functions of complex numbers below, so one that reaches here is
 * real only with an exact zero imaginary part, and is otherwise an error.
 * @param {string} name - The procedure's name, for the error message.
 * @param {number} position - The argument's position, from 1.
 * @param {*} z - The argument.
 * @returns {number|bigint|Rational} The real number.
 */
function realArgument(name, position, z) {
    assertReal(name, position, z);
    return isComplex(z) ? z.real : z;
}

/**
 * The negation of a real number, in its own representation.
 * @param {number|bigint|Rational} x - The number.
 * @returns {number|bigint|Rational}
 */
function negateReal(x) {
    return isRational(x) ? x.negate() : -x;
}

/** The smallest positive double with all 53 bits of precision. */
const MIN_NORMAL = 2 ** -1022;

/**
 * An exact positive number whose nearest double has lost its magnitude, split
 * as m * 2^k, m a double between 1/2 and 2. An integer of 400 digits converts
 * to +inf.0, and its reciprocal to 0.0, but its logarithm, square root and
 * powers are doubles, which m and k give. Null for a number whose double
 * serves: an inexact one, zero, or one in the normal range of doubles.
 * @param {number|bigint|Rational} x - A real number, not negative.
 * @returns {Array<number>|null} `[m, k]`, or null.
 */
function splitBeyondDoubles(x) {
    const fraction = asExactFraction(x);
    if (fraction === null || fraction[0] === 0n) return null;
    const d = toNumber(x);
    if (d >= MIN_NORMAL && d < Infinity) return null;
    const [num, den] = fraction;
    const k = bitLength(num) - bitLength(den);
    const m = k >= 0
        ? ratioToNumber(num, den << BigInt(k))
        : ratioToNumber(num << BigInt(-k), den);
    return [m, k];
}

/**
 * The natural logarithm of a real number that is not negative.
 * @param {number|bigint|Rational} x - The number.
 * @returns {number}
 */
function logNonNegative(x) {
    const split = splitBeyondDoubles(x);
    if (split === null) return Math.log(toNumber(x));
    return Math.log(split[0]) + split[1] * Math.LN2;
}

/**
 * The natural logarithm of a real number. A negative number's is the complex
 * log|x| + pi i, whose imaginary part is in (-pi, pi] as R7RS 6.2.6 requires;
 * -0.0 counts as negative there, so `(log -0.0)` is -inf.0+pi i, as R7RS has it.
 * @param {number|bigint|Rational} x - The number.
 * @returns {number|Complex}
 */
function logReal(x) {
    if (numericCompare(x, 0n) < 0 || Object.is(x, -0)) {
        return makeRectangular(logNonNegative(negateReal(x)), Math.PI);
    }
    return logNonNegative(x);
}

/**
 * The exact square root of an exact number that is not negative, or null if
 * it has none: an integer's if it is a perfect square, a rational's if its
 * numerator and denominator both are.
 * @param {number|bigint|Rational} x - The number.
 * @returns {bigint|Rational|null}
 */
function exactSqrt(x) {
    const fraction = asExactFraction(x);
    if (fraction === null) return null;
    const [num, den] = fraction;
    const rootNum = isqrtBigInt(num);
    if (rootNum * rootNum !== num) return null;
    if (den === 1n) return rootNum;
    const rootDen = isqrtBigInt(den);
    return rootDen * rootDen === den ? new Rational(rootNum, rootDen) : null;
}

/**
 * The square root of a real number that is not negative: exact for an exact
 * square, as R7RS 6.2.6's `(sqrt 9)` => 3 has it, and otherwise a double.
 * @param {number|bigint|Rational} x - The number.
 * @returns {number|bigint|Rational}
 */
function sqrtNonNegative(x) {
    const root = exactSqrt(x);
    if (root !== null) return root;
    const split = splitBeyondDoubles(x);
    if (split === null) return Math.sqrt(toNumber(x));
    // An odd power of two leaves one factor of two with m.
    const [m, k] = split;
    return k % 2 === 0
        ? Math.sqrt(m) * 2 ** (k / 2)
        : Math.sqrt(2 * m) * 2 ** ((k - 1) / 2);
}

/**
 * A real number that is not negative raised to a real power, as a double.
 * @param {number|bigint|Rational} x - The base.
 * @param {number} e - The exponent.
 * @returns {number}
 */
function powNonNegative(x, e) {
    const split = splitBeyondDoubles(x);
    if (split === null) return Math.pow(toNumber(x), e);
    return Math.pow(split[0], e) * Math.pow(2, split[1] * e);
}

/**
 * The exact number a real number is (R7RS 6.2.6): an integral flonum an exact
 * integer, any other finite flonum the exact rational it is -- `(exact 0.5)`
 * is 1/2 and `(exact 0.1)` 3602879701896397/36028797018963968 -- and an
 * inexact rational the same value exact.
 * @param {number|bigint|Rational} x - The real number.
 * @param {*} z - The argument `exact` was given, of which x is a part, for
 *   the error.
 * @returns {bigint|Rational}
 */
function exactReal(x, z) {
    if (typeof x === 'bigint') return x;
    if (isRational(x)) {
        return x.denominator === 1n ? x.numerator : new Rational(x.numerator, x.denominator, true);
    }
    if (Number.isInteger(x)) return BigInt(x);
    if (!Number.isFinite(x)) {
        throw new SchemeError('exact: an infinity or NaN has no exact equivalent', [z], 'exact');
    }
    // A flonum is a dyadic rational. Doubling one is exact in binary floating
    // point, so doubling it until it is an integer gives its numerator over a
    // power of two: at most 1,074 times, for the smallest subnormal, and never
    // past 2^53, since a flonum that is not an integer is below 2^52.
    let numerator = x;
    let denominator = 1n;
    while (!Number.isInteger(numerator)) {
        numerator *= 2;
        denominator *= 2n;
    }
    return new Rational(BigInt(numerator), denominator, true);
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

/**
 * A rational number's numerator and denominator in lowest terms, as exact
 * integers: an inexact number's are those of the exact number it is, which
 * `numerator` and `denominator` give back inexact -- R7RS 6.2.6 has
 * `(denominator (inexact (/ 6 4)))` => 2.0.
 * @param {string} name - The procedure's name, for the error.
 * @param {*} q - The number.
 * @returns {Array<bigint>} `[numerator, denominator]`.
 * @throws {SchemeTypeError} If q is not a rational number.
 */
function fractionOf(name, q) {
    if (typeof q === 'bigint') return [q, 1n];
    if (isRational(q)) return [q.numerator, q.denominator];
    if (typeof q === 'number' && Number.isFinite(q)) {
        const exact = exactReal(q, q);
        return typeof exact === 'bigint' ? [exact, 1n] : [exact.numerator, exact.denominator];
    }
    throw new SchemeTypeError(name, 1, 'rational number', q);
}

// =============================================================================
// Functions of Complex Numbers
// =============================================================================
// R7RS 6.2.6 defines the elementary functions on every complex number: e^z
// as e^x (cos y + i sin y) for z = x + iy; log z as log|z| + i angle z, whose
// imaginary part is in (-pi, pi] -- -pi itself below the negative reals,
// where the imaginary part is -0.0; sqrt z as the root with a positive real
// part, or a zero one and an imaginary part that is not negative; and from
// those asin z = -i log(iz + sqrt(1 - z^2)), acos z = pi/2 - asin z,
// atan z = (log(1 + iz) - log(1 - iz)) / 2i, and z1^z2 = e^(z2 log z1). A
// complex argument's value is computed on its parts as doubles, as Math
// computes a real one's, and is an inexact complex number however near its
// imaginary part is to zero; a real argument keeps the functions of real
// numbers above, which give a real value where there is one. Each function
// here takes and gives a complex number as the pair [x, y] of its parts.

/**
 * A number's parts as doubles.
 * @param {number|bigint|Rational|Complex} z - The number.
 * @returns {Array<number>} `[x, y]`.
 */
function partsOf(z) {
    return isComplex(z) ? [toNumber(z.real), toNumber(z.imag)] : [toNumber(z), 0];
}

/**
 * The inexact complex number with the given parts.
 * @param {Array<number>} parts - `[x, y]`.
 * @returns {Complex}
 */
function complexOf([x, y]) {
    return new Complex(x, y, false);
}

/** @param {Array<number>} a @param {Array<number>} b @returns {Array<number>} a b */
function cMul([a, b], [c, d]) {
    return [a * c - b * d, a * d + b * c];
}

/** @param {Array<number>} z @returns {Array<number>} e^z */
function cExp([x, y]) {
    const m = Math.exp(x);
    return [m * Math.cos(y), m * Math.sin(y)];
}

/** @param {Array<number>} z @returns {Array<number>} log z, its imaginary part in (-pi, pi] */
function cLog([x, y]) {
    return [Math.log(Math.hypot(x, y)), Math.atan2(y, x)];
}

/**
 * The principal square root, computed so that neither part is the
 * difference of two near numbers. Where its real part is zero -- a negative
 * real, whether its imaginary part is 0.0 or -0.0 -- the imaginary part is
 * not negative, as R7RS requires of sqrt.
 * @param {Array<number>} z
 * @returns {Array<number>} sqrt z
 */
function cSqrt([x, y]) {
    if (x === 0 && y === 0) return [0, 0];
    const r = Math.hypot(x, y);
    if (x >= 0) {
        const t = Math.sqrt((r + x) / 2);
        return [t, y / (2 * t)];
    }
    const t = Math.sqrt((r - x) / 2);
    return [Math.abs(y) / (2 * t), y < 0 ? -t : t];
}

/** @param {Array<number>} z @returns {Array<number>} sin z */
function cSin([x, y]) {
    return [Math.sin(x) * Math.cosh(y), Math.cos(x) * Math.sinh(y)];
}

/** @param {Array<number>} z @returns {Array<number>} cos z */
function cCos([x, y]) {
    return [Math.cos(x) * Math.cosh(y), -Math.sin(x) * Math.sinh(y)];
}

/**
 * The tangent, as (sin 2x + i sinh 2y) / (cos 2x + cosh 2y). Far from the
 * real axis, where cosh 2y overflows, it is i or -i, its real part
 * 2 sin 2x e^(-2|y|).
 * @param {Array<number>} z
 * @returns {Array<number>} tan z
 */
function cTan([x, y]) {
    if (Math.abs(y) > 20) return [2 * Math.sin(2 * x) * Math.exp(-2 * Math.abs(y)), Math.sign(y)];
    const d = Math.cos(2 * x) + Math.cosh(2 * y);
    return [Math.sin(2 * x) / d, Math.sinh(2 * y) / d];
}

/** @param {Array<number>} z @returns {Array<number>} asin z = -i log(iz + sqrt(1 - z^2)) */
function cAsin([x, y]) {
    const [u, v] = cSqrt([1 - (x * x - y * y), -(2 * x * y)]);
    const [lx, ly] = cLog([u - y, v + x]);
    return [ly, -lx];
}

/** @param {Array<number>} z @returns {Array<number>} acos z = pi/2 - asin z */
function cAcos(z) {
    const [ax, ay] = cAsin(z);
    return [Math.PI / 2 - ax, -ay];
}

/** @param {Array<number>} z @returns {Array<number>} atan z = (log(1 + iz) - log(1 - iz)) / 2i */
function cAtan([x, y]) {
    const [ax, ay] = cLog([1 - y, x]);
    const [bx, by] = cLog([1 + y, -x]);
    return [(ay - by) / 2, -(ax - bx) / 2];
}

/**
 * z1^z2 where either is complex. An exact integer power is a product, by
 * squaring, so an exact base's is exact and an inexact one's has none of
 * the rounding of exp and log; zero to a power is 1 if the power is zero and
 * 0 if its real part is positive, inexact if either is, and otherwise an
 * error (R7RS 6.2.6); any other power is e^(z2 log z1).
 * @param {number|bigint|Rational|Complex} base - z1.
 * @param {number|bigint|Rational|Complex} exponent - z2.
 * @returns {number|bigint|Rational|Complex}
 */
function complexExpt(base, exponent) {
    const exact = isExact(base) && isExact(exponent);
    if (typeof exponent === 'bigint') {
        const one = isExact(base) ? 1n : 1;
        let result = one;
        let square = base;
        for (let k = exponent < 0n ? -exponent : exponent; k > 0n; k >>= 1n) {
            if (k & 1n) result = genericMul(result, square);
            if (k > 1n) square = genericMul(square, square);
        }
        return exponent < 0n ? genericDiv(one, result) : result;
    }
    const [bx, by] = partsOf(base);
    if (bx === 0 && by === 0) {
        const [ex, ey] = partsOf(exponent);
        if (ex === 0 && ey === 0) return exact ? 1n : 1;
        if (ex > 0) return exact ? 0n : 0;
        throw new SchemeError('expt: zero to a power whose real part is not positive', [base, exponent], 'expt');
    }
    return complexOf(cExp(cMul(partsOf(exponent), cLog([bx, by]))));
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
 * Whether a number is held as doubles: an inexact real, or an inexact complex
 * number, whose parts are doubles. Two such operands are combined with
 * JavaScript's operators directly, as the real arithmetic combines doubles,
 * and exactly as below: through it, arithmetic on inexact complex numbers
 * (benchmarks/r7rs/src/mbrotZ.scm) was a fifth slower.
 * @param {*} x - A number.
 * @returns {boolean}
 */
function heldAsDoubles(x) {
    return typeof x === 'number' || (isComplex(x) && !x.exact);
}

/**
 * Adds two numbers at least one of which is complex.
 * @param {*} a - A number.
 * @param {*} b - A number.
 * @returns {*} The sum.
 */
function complexAdd(a, b) {
    if (heldAsDoubles(a) && heldAsDoubles(b)) {
        if (!isComplex(a)) return new Complex(a + b.real, b.imag, false);
        if (!isComplex(b)) return new Complex(a.real + b, a.imag, false);
        return new Complex(a.real + b.real, a.imag + b.imag, false);
    }
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
    if (heldAsDoubles(a) && heldAsDoubles(b)) {
        if (!isComplex(a)) return new Complex(a * b.real, a * b.imag, false);
        if (!isComplex(b)) return new Complex(a.real * b, a.imag * b, false);
        return new Complex(a.real * b.real - a.imag * b.imag, a.real * b.imag + a.imag * b.real, false);
    }
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
 *
 * Each takes the number of arguments R7RS gives it, and raises an arity error
 * for any other number. One of a fixed number of arguments takes them as named
 * parameters and tests `arguments.length`, which costs a call with the right
 * count nothing -- compiled code calls these directly -- where a rest
 * parameter, or an array built to pass to `assertArity`, would allocate on
 * every call.
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
    '-': function (first, ...rest) {
        if (arguments.length === 0) assertArity('-', arguments, 1, Infinity);
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
    '/': function (first, ...rest) {
        if (arguments.length === 0) assertArity('/', arguments, 1, Infinity);
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
    // Each is exact on exact integers and inexact if either argument is
    // inexact; see `divisionResult`. The three that loops spend their time in
    // take two exact integers, the divisor not zero, first.

    /**
     * Modulo operation (result has same sign as divisor).
     * @param {bigint|number} a - Dividend.
     * @param {bigint|number} b - Divisor.
     * @returns {bigint|number} Modulo.
     */
    'modulo': function (a, b) {
        if (arguments.length !== 2) assertArity('modulo', arguments, 2);
        if (typeof a === 'bigint' && typeof b === 'bigint' && b !== 0n) {
            const rem = a % b;
            return rem === 0n || (rem > 0n) === (b > 0n) ? rem : rem + b;
        }
        assertDivision('modulo', a, b);
        const aBig = toBigInt(a);
        const bBig = toBigInt(b);
        // JavaScript % gives remainder with sign of dividend
        // modulo should have sign of divisor
        const rem = aBig % bBig;
        const result = rem === 0n || (rem > 0n) === (bBig > 0n) ? rem : rem + bBig;
        return divisionResult(result, a, b);
    },

    /**
     * Quotient (integer division, truncates toward zero).
     * @param {bigint|number} a - Dividend.
     * @param {bigint|number} b - Divisor.
     * @returns {bigint|number} Integer quotient.
     */
    'quotient': function (a, b) {
        if (arguments.length !== 2) assertArity('quotient', arguments, 2);
        if (typeof a === 'bigint' && typeof b === 'bigint' && b !== 0n) return a / b;
        assertDivision('quotient', a, b);
        return divisionResult(truncDivBigInt(toBigInt(a), toBigInt(b)), a, b);
    },

    /**
     * Remainder (result has same sign as dividend).
     * @param {bigint|number} a - Dividend.
     * @param {bigint|number} b - Divisor.
     * @returns {bigint|number} Remainder.
     */
    'remainder': function (a, b) {
        if (arguments.length !== 2) assertArity('remainder', arguments, 2);
        if (typeof a === 'bigint' && typeof b === 'bigint' && b !== 0n) return a % b;
        assertDivision('remainder', a, b);
        return divisionResult(toBigInt(a) % toBigInt(b), a, b);
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
    'number?': function (obj) {
        if (arguments.length !== 1) assertArity('number?', arguments, 1);
        return typeof obj === 'number' || typeof obj === 'bigint' || isRational(obj) || isComplex(obj);
    },

    /**
     * Complex number type predicate.
     * In R7RS, all numbers are complex.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a complex number.
     */
    'complex?': function (obj) {
        if (arguments.length !== 1) assertArity('complex?', arguments, 1);
        return typeof obj === 'number' || typeof obj === 'bigint' || isRational(obj) || isComplex(obj);
    },

    /**
     * Real number type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a real number.
     */
    'real?': function (obj) {
        if (arguments.length !== 1) assertArity('real?', arguments, 1);
        if (typeof obj === 'number') return true;
        if (typeof obj === 'bigint') return true;
        if (isRational(obj)) return true;
        if (isComplex(obj)) return isExactZero(obj.imag);
        return false;
    },

    /**
     * Rational number type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a rational number.
     */
    'rational?': function (obj) {
        if (arguments.length !== 1) assertArity('rational?', arguments, 1);
        if (isRational(obj)) return true;
        if (typeof obj === 'bigint') return true;  // All integers are rational
        if (typeof obj === 'number') return Number.isFinite(obj);
        if (isComplex(obj)) {
            return isExactZero(obj.imag) && (typeof obj.real !== 'number' || Number.isFinite(obj.real));
        }
        return false;
    },

    /**
     * Integer type predicate.
     * BigInt is always an integer. Number must pass Number.isInteger().
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is an integer.
     */
    'integer?': function (obj) {
        if (arguments.length !== 1) assertArity('integer?', arguments, 1);
        if (typeof obj === 'bigint') return true;
        if (typeof obj === 'number') return Number.isInteger(obj);
        if (isRational(obj)) return obj.denominator === 1n || obj.denominator === 1;
        if (isComplex(obj)) {
            return isExactZero(obj.imag) && (typeof obj.real === 'bigint' || Number.isInteger(obj.real));
        }
        return false;
    },

    /**
     * Exact integer type predicate.
     * Only BigInt and exact Rationals with denominator 1 are exact integers.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is an exact integer.
     */
    'exact-integer?': function (obj) {
        if (arguments.length !== 1) assertArity('exact-integer?', arguments, 1);
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
    'exact?': function (obj) {
        if (arguments.length !== 1) assertArity('exact?', arguments, 1);
        assertNumber('exact?', 1, obj);
        return isExact(obj);
    },

    /**
     * Inexact number type predicate.
     * JS Numbers are inexact. Rationals/Complex with exact=false are inexact.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is inexact.
     */
    'inexact?': function (obj) {
        if (arguments.length !== 1) assertArity('inexact?', arguments, 1);
        assertNumber('inexact?', 1, obj);
        return !isExact(obj);
    },

    /**
     * Finite predicate.
     * @param {number|bigint|Rational|Complex} x - Number to check.
     * @returns {boolean} True if x is finite.
     */
    'finite?': function (x) {
        if (arguments.length !== 1) assertArity('finite?', arguments, 1);
        if (typeof x === 'bigint') return true;  // BigInt is always finite
        if (typeof x === 'number') return Number.isFinite(x);
        if (isRational(x)) return true;
        // A part may be exact, and is then finite.
        if (isComplex(x)) return !isInfiniteOrNaN(x.real) && !isInfiniteOrNaN(x.imag);
        throw new Error('finite?: expected number');
    },

    /**
     * Infinite predicate.
     * @param {number|bigint|Rational|Complex} x - Number to check.
     * @returns {boolean} True if x is infinite.
     */
    'infinite?': function (x) {
        if (arguments.length !== 1) assertArity('infinite?', arguments, 1);
        if (typeof x === 'bigint') return false;  // BigInt is never infinite
        if (typeof x === 'number') return !Number.isFinite(x) && !Number.isNaN(x);
        if (isRational(x)) return false;
        if (isComplex(x)) return isInfinite(x.real) || isInfinite(x.imag);
        throw new Error('infinite?: expected number');
    },

    /**
     * NaN predicate.
     * @param {number|bigint|Rational|Complex} x - Number to check.
     * @returns {boolean} True if x is NaN.
     */
    'nan?': function (x) {
        if (arguments.length !== 1) assertArity('nan?', arguments, 1);
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
    'numerator': function (q) {
        if (arguments.length !== 1) assertArity('numerator', arguments, 1);
        const numerator = fractionOf('numerator', q)[0];
        return isExact(q) ? numerator : Number(numerator);
    },

    /**
     * Returns the denominator of a rational.
     * @param {Rational|number|bigint} q - Rational number.
     * @returns {number|bigint} Denominator.
     */
    'denominator': function (q) {
        if (arguments.length !== 1) assertArity('denominator', arguments, 1);
        const denominator = fractionOf('denominator', q)[1];
        return isExact(q) ? denominator : Number(denominator);
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
    'make-rectangular': function (x, y) {
        if (arguments.length !== 2) assertArity('make-rectangular', arguments, 2);
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
    'make-polar': function (r, theta) {
        if (arguments.length !== 2) assertArity('make-polar', arguments, 2);
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
    'real-part': function (z) {
        if (arguments.length !== 1) assertArity('real-part', arguments, 1);
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
    'imag-part': function (z) {
        if (arguments.length !== 1) assertArity('imag-part', arguments, 1);
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
    'magnitude': function (z) {
        if (arguments.length !== 1) assertArity('magnitude', arguments, 1);
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
    'angle': function (z) {
        if (arguments.length !== 1) assertArity('angle', arguments, 1);
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
    'abs': function (x) {
        if (arguments.length !== 1) assertArity('abs', arguments, 1);
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
    'floor': function (x) {
        if (arguments.length !== 1) assertArity('floor', arguments, 1);
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
    'ceiling': function (x) {
        if (arguments.length !== 1) assertArity('ceiling', arguments, 1);
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
    'truncate': function (x) {
        if (arguments.length !== 1) assertArity('truncate', arguments, 1);
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
    'round': function (x) {
        if (arguments.length !== 1) assertArity('round', arguments, 1);
        if (typeof x === 'bigint') return x;
        if (isRational(x)) {
            return roundDivBigInt(x.numerator, x.denominator);
        }
        assertNumber('round', 1, x);
        // Math.round takes a half up; R7RS 6.2.6 takes it to the even
        // integer. A half is exactly representable, so the test is exact.
        const r = Math.round(x);
        return r - x === 0.5 && r % 2 !== 0 ? r - 1 : r;
    },

    /**
     * Exponentiation: z1^z2, which R7RS 6.2.6 defines as e^(z2 log z1).
     * Exact for an exact base and an exact integer exponent; otherwise a
     * double, or for a negative base and an exponent that is not an integer,
     * the complex |z1|^z2 (cos(pi z2) + i sin(pi z2)), since log z1 is
     * log|z1| + pi i.
     * @param {number|bigint|Rational} base - Base.
     * @param {number|bigint|Rational} exponent - Exponent.
     * @returns {number|bigint|Rational|Complex} base^exponent.
     */
    'expt': function (base, exponent) {
        if (arguments.length !== 2) assertArity('expt', arguments, 2);
        if (typeof base === 'bigint' && typeof exponent === 'bigint') {
            return exponent >= 0n ? base ** exponent : genericDiv(1n, base ** -exponent);
        }
        if (isComplex(base) || isComplex(exponent)) {
            assertNumber('expt', 1, base);
            assertNumber('expt', 2, exponent);
            return complexExpt(base, exponent);
        }
        const x = realArgument('expt', 1, base);
        const y = realArgument('expt', 2, exponent);
        if (typeof y === 'bigint' && isRational(x) && x.exact !== false) {
            const k = y < 0n ? -y : y;
            const num = x.numerator ** k;
            const den = x.denominator ** k;
            return y < 0n ? genericDiv(den, num) : genericDiv(num, den);
        }
        const e = toNumber(y);
        // Compared exactly: a negative rational too small for a double
        // converts to -0.0, which is not below zero. A NaN base is not
        // negative either.
        if (!(numericCompare(x, 0n) < 0)) return powNonNegative(x, e);
        // An infinite or NaN exponent, as IEEE 754 has it.
        if (!Number.isFinite(e)) return Math.pow(toNumber(x), e);
        const magnitude = powNonNegative(negateReal(x), e);
        // An integer power of a negative base: its parity gives the sign.
        if (Number.isInteger(e)) return e % 2 === 0 ? magnitude : -magnitude;
        return makeRectangular(magnitude * Math.cos(Math.PI * e), magnitude * Math.sin(Math.PI * e));
    },

    /**
     * Square root: exact for an exact square, and for a negative number the
     * imaginary i sqrt(-z), as R7RS 6.2.6's `(sqrt -1)` => +i has it.
     * @param {number|bigint|Rational} z - Number.
     * @returns {number|bigint|Rational|Complex} Square root.
     */
    'sqrt': function (z) {
        if (arguments.length !== 1) assertArity('sqrt', arguments, 1);
        if (typeof z === 'number' && z >= 0) return Math.sqrt(z);
        if (isComplex(z)) return complexOf(cSqrt(partsOf(z)));
        const x = realArgument('sqrt', 1, z);
        // Compared exactly, as in `expt`; -0.0 is not below zero, and is its
        // own root.
        if (numericCompare(x, 0n) < 0) return makeRectangular(0n, sqrtNonNegative(negateReal(x)));
        return sqrtNonNegative(x);
    },

    /**
     * Sine.
     * @param {number|bigint|Rational} z - Angle in radians.
     * @returns {number} Sine of z.
     */
    'sin': function (z) {
        if (arguments.length !== 1) assertArity('sin', arguments, 1);
        if (isComplex(z)) return complexOf(cSin(partsOf(z)));
        return Math.sin(toNumber(realArgument('sin', 1, z)));
    },

    /**
     * Cosine.
     * @param {number|bigint|Rational} z - Angle in radians.
     * @returns {number} Cosine of z.
     */
    'cos': function (z) {
        if (arguments.length !== 1) assertArity('cos', arguments, 1);
        if (isComplex(z)) return complexOf(cCos(partsOf(z)));
        return Math.cos(toNumber(realArgument('cos', 1, z)));
    },

    /**
     * Tangent.
     * @param {number|bigint|Rational} z - Angle in radians.
     * @returns {number} Tangent of z.
     */
    'tan': function (z) {
        if (arguments.length !== 1) assertArity('tan', arguments, 1);
        if (isComplex(z)) return complexOf(cTan(partsOf(z)));
        return Math.tan(toNumber(realArgument('tan', 1, z)));
    },

    /**
     * Arcsine. Beyond [-1, 1] it is complex: R7RS 6.2.6 defines asin z as
     * -i log(iz + sqrt(1 - z^2)), which for a real x > 1 is pi/2 - i acosh x,
     * and asin is odd.
     * @param {number|bigint|Rational} z - Value.
     * @returns {number|Complex} Arcsine in radians.
     */
    'asin': function (z) {
        if (arguments.length !== 1) assertArity('asin', arguments, 1);
        if (isComplex(z)) return complexOf(cAsin(partsOf(z)));
        const x = toNumber(realArgument('asin', 1, z));
        if (x > 1) return makeRectangular(Math.PI / 2, -Math.acosh(x));
        if (x < -1) return makeRectangular(-Math.PI / 2, Math.acosh(-x));
        return Math.asin(x);
    },

    /**
     * Arccosine. Beyond [-1, 1] it is complex: R7RS 6.2.6 defines acos z as
     * pi/2 - asin z.
     * @param {number|bigint|Rational} z - Value.
     * @returns {number|Complex} Arccosine in radians.
     */
    'acos': function (z) {
        if (arguments.length !== 1) assertArity('acos', arguments, 1);
        if (isComplex(z)) return complexOf(cAcos(partsOf(z)));
        const x = toNumber(realArgument('acos', 1, z));
        if (x > 1) return makeRectangular(0, Math.acosh(x));
        if (x < -1) return makeRectangular(Math.PI, -Math.acosh(-x));
        return Math.acos(x);
    },

    /**
     * Arctangent. With two arguments, the angle of the point (x, y), which
     * R7RS 6.2.6 requires to be real.
     * @param {number|bigint|Rational} y - z, or the point's y.
     * @param {number|bigint|Rational} [x] - The point's x.
     * @returns {number} Arctangent in radians.
     */
    'atan': function (y, x) {
        const count = arguments.length;
        if (count !== 1 && count !== 2) assertArity('atan', arguments, 1, 2);
        if (count === 1) {
            if (isComplex(y)) return complexOf(cAtan(partsOf(y)));
            return Math.atan(toNumber(realArgument('atan', 1, y)));
        }
        assertReal('atan', 1, y);
        assertReal('atan', 2, x);
        return Math.atan2(toNumber(y), toNumber(x));
    },

    /**
     * Logarithm: natural with one argument, and with two the logarithm of the
     * first in the base of the second, log z1 / log z2. A negative number's is
     * complex; see `logReal`.
     * @param {number|bigint|Rational} z1 - The number.
     * @param {number|bigint|Rational} [z2] - The base.
     * @returns {number|Complex} The logarithm.
     */
    'log': function (z1, z2) {
        const count = arguments.length;
        if (count !== 1 && count !== 2) assertArity('log', arguments, 1, 2);
        const logOf = (z, position) =>
            isComplex(z) ? complexOf(cLog(partsOf(z))) : logReal(realArgument('log', position, z));
        const z = logOf(z1, 1);
        if (count === 1) return z;
        return genericDiv(z, logOf(z2, 2));
    },

    /**
     * Exponential function (e^z).
     * @param {number|bigint|Rational} z - Exponent.
     * @returns {number} e^z.
     */
    'exp': function (z) {
        if (arguments.length !== 1) assertArity('exp', arguments, 1);
        if (isComplex(z)) return complexOf(cExp(partsOf(z)));
        return Math.exp(toNumber(realArgument('exp', 1, z)));
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
    'exact-integer-sqrt': function (k) {
        if (arguments.length !== 1) assertArity('exact-integer-sqrt', arguments, 1);
        // R7RS 6.2.6 takes an exact integer only: an inexact one's root
        // would have to be inexact, and this procedure's are exact.
        if (typeof k !== 'bigint' || k < 0n) {
            throw new SchemeTypeError('exact-integer-sqrt', 1, 'non-negative exact integer', k);
        }
        const s = isqrtBigInt(k);
        return new Values([s, k - s * s]);
    },

    /**
     * Floor division.
     * Returns two values: quotient and remainder such that
     * n1 = n2 * quotient + remainder, with remainder having same sign as n2.
     * @param {bigint|number} n1 - Dividend
     * @param {bigint|number} n2 - Divisor
     * @returns {Values} Two values: quotient and remainder
     */
    'floor/': function (n1, n2) {
        if (arguments.length !== 2) assertArity('floor/', arguments, 2);
        assertDivision('floor/', n1, n2);
        const a = toBigInt(n1);
        const b = toBigInt(n2);
        const q = floorDivBigInt(a, b);
        return new Values([divisionResult(q, n1, n2), divisionResult(a - b * q, n1, n2)]);
    },

    /**
     * Truncate division.
     * Returns two values: quotient and remainder such that
     * n1 = n2 * quotient + remainder, with remainder having same sign as n1.
     * @param {bigint|number} n1 - Dividend
     * @param {bigint|number} n2 - Divisor
     * @returns {Values} Two values: quotient and remainder
     */
    'truncate/': function (n1, n2) {
        if (arguments.length !== 2) assertArity('truncate/', arguments, 2);
        assertDivision('truncate/', n1, n2);
        const a = toBigInt(n1);
        const b = toBigInt(n2);
        const q = truncDivBigInt(a, b);
        return new Values([divisionResult(q, n1, n2), divisionResult(a - b * q, n1, n2)]);
    },

    /**
     * Floor quotient (single value).
     * @param {bigint|number} n1 - Dividend
     * @param {bigint|number} n2 - Divisor
     * @returns {bigint|number} Floor quotient
     */
    'floor-quotient': function (n1, n2) {
        if (arguments.length !== 2) assertArity('floor-quotient', arguments, 2);
        assertDivision('floor-quotient', n1, n2);
        return divisionResult(floorDivBigInt(toBigInt(n1), toBigInt(n2)), n1, n2);
    },

    /**
     * Floor remainder (single value).
     * @param {bigint|number} n1 - Dividend
     * @param {bigint|number} n2 - Divisor
     * @returns {bigint|number} Floor remainder
     */
    'floor-remainder': function (n1, n2) {
        if (arguments.length !== 2) assertArity('floor-remainder', arguments, 2);
        assertDivision('floor-remainder', n1, n2);
        const a = toBigInt(n1);
        const b = toBigInt(n2);
        return divisionResult(a - b * floorDivBigInt(a, b), n1, n2);
    },

    /**
     * Truncate quotient (single value).
     * @param {bigint|number} n1 - Dividend
     * @param {bigint|number} n2 - Divisor
     * @returns {bigint|number} Truncate quotient
     */
    'truncate-quotient': function (n1, n2) {
        if (arguments.length !== 2) assertArity('truncate-quotient', arguments, 2);
        assertDivision('truncate-quotient', n1, n2);
        return divisionResult(truncDivBigInt(toBigInt(n1), toBigInt(n2)), n1, n2);
    },

    /**
     * Truncate remainder (single value).
     * @param {bigint|number} n1 - Dividend
     * @param {bigint|number} n2 - Divisor
     * @returns {bigint|number} Truncate remainder
     */
    'truncate-remainder': function (n1, n2) {
        if (arguments.length !== 2) assertArity('truncate-remainder', arguments, 2);
        assertDivision('truncate-remainder', n1, n2);
        return divisionResult(toBigInt(n1) % toBigInt(n2), n1, n2);
    },

    /**
     * Returns the square of a number. A rational or complex number is
     * multiplied by the tower's multiplication: JavaScript's `*` on the
     * objects that represent them gives NaN, or throws.
     * @param {number|bigint|Rational|Complex} z - Number to square.
     * @returns {number|bigint|Rational|Complex} z * z
     */
    'square': function (z) {
        if (arguments.length !== 1) assertArity('square', arguments, 1);
        assertNumber('square', 1, z);
        if (typeof z === 'bigint' || typeof z === 'number') return z * z;
        return genericMul(z, z);
    },

    /**
     * Converts a number to its inexact equivalent: the nearest double, and
     * for a complex number the nearest double of each part.
     * @param {number|bigint|Rational|Complex} z - Number to convert.
     * @returns {number|Rational|Complex} Inexact equivalent.
     */
    'inexact': function (z) {
        if (arguments.length !== 1) assertArity('inexact', arguments, 1);
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
            // An exact complex number's parts converted, as the constructor
            // does for one made inexact.
            return z.exact ? new Complex(z.real, z.imag, false) : z;
        }
        return z;
    },

    /**
     * Converts a number to its exact equivalent (R7RS 6.2.6): a real number
     * as `exactReal` converts it, and a complex number part by part.
     * @param {number|bigint|Rational|Complex} z - Number to convert.
     * @returns {bigint|Rational|Complex} Exact equivalent.
     */
    'exact': function exact(z) {
        if (arguments.length !== 1) assertArity('exact', arguments, 1);
        assertNumber('exact', 1, z);
        if (!isComplex(z)) return exactReal(z, z);
        // Each part converted; an exact zero imaginary part leaves the real
        // number, as `(exact 3.0+0.0i)` is 3.
        const real = exactReal(z.real, z);
        const imag = exactReal(z.imag, z);
        return imag === 0n ? real : makeRectangular(real, imag);
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

// =============================================================================
// The primitives, in Scheme's representation of numbers
// =============================================================================
//
// Every primitive above computes in the numeric tower's representation, an
// exact integer a BigInt and an inexact real a JavaScript number
// (number_representation.js). Each is wrapped here to take its arguments into
// that representation and give its result back in Scheme's; and the ones
// arithmetic spends its time in take every real held as a number, a BigInt or
// a Flonum directly first, converting nothing.

/**
 * A primitive of the tower's representation, taking and giving Scheme's.
 * @param {Function} fn - The primitive.
 * @returns {Function}
 */
function towered(fn) {
    return (...args) => {
        for (let i = 0; i < args.length; i++) args[i] = toTower(args[i]);
        let result;
        try {
            result = fn(...args);
        } catch (e) {
            // An error names the arguments it was given, which are the
            // caller's in Scheme's representation, not the tower's.
            if (e instanceof SchemeError && Array.isArray(e.irritants)) e.irritants = e.irritants.map(fromTower);
            throw e;
        }
        return result instanceof Values ? new Values(result.values.map(fromTower)) : fromTower(result);
    };
}

for (const [name, fn] of Object.entries(mathPrimitives)) mathPrimitives[name] = towered(fn);

/**
 * A variadic arithmetic primitive that folds its arguments with `combine`
 * while it applies to them (`addReals` and the rest), and otherwise does what
 * `general` does. A lone argument is checked by `general`. Two numbers that
 * are not both reals held as numbers, BigInts or Flonums -- a complex number,
 * a rational -- are combined by the tower's operation on two numbers,
 * `tower`, directly: through `general`, the wrapper and the variadic
 * primitive cost complex arithmetic (benchmarks/r7rs/src/mbrotZ.scm) half
 * again its time.
 * @param {Function} general - The primitive for any numbers.
 * @param {function(*, *): *} combine - Two reals' result, or `undefined`.
 * @param {number|undefined} empty - The result for no arguments, or
 *   `undefined` for a primitive that needs at least one, whose call with none
 *   is then `general`'s arity error.
 * @param {function(*, *): *} tower - The tower's operation on two numbers,
 *   in its representation.
 * @returns {Function}
 */
function foldingReals(general, combine, empty, tower) {
    return (...args) => {
        if (args.length === 0) return empty === undefined ? general() : empty;
        if (args.length === 1) return general(...args);
        if (args.length === 2) {
            const a = args[0], b = args[1];
            const r = combine(a, b);
            if (r !== undefined) return r;
            if (isNumber(a) && isNumber(b)) return fromTower(tower(toTower(a), toTower(b)));
            return general(a, b);
        }
        let acc = args[0];
        for (let i = 1; i < args.length; i++) {
            acc = combine(acc, args[i]);
            if (acc === undefined) return general(...args);
        }
        return acc;
    };
}

/**
 * A comparison of two reals by `compare` where it applies (`lessReals` and
 * the rest), and otherwise, and for any other number of arguments, what
 * `general` does.
 * @param {Function} general - The primitive for any numbers.
 * @param {function(*, *): (boolean|undefined)} compare - Two reals' answer.
 * @returns {Function}
 */
function comparingReals(general, compare) {
    return (...args) => {
        if (args.length === 2) {
            const answer = compare(args[0], args[1]);
            if (answer !== undefined) return answer;
        }
        return general(...args);
    };
}

/**
 * An integer division of two exact integers held as numbers -- `a % b` is
 * exact for doubles, and so is the division of `a - a % b` by `b`, a
 * multiple of it -- and otherwise what `general` does. Of two parameters, not
 * a rest parameter, as the division primitives take two: V8 makes a smaller
 * function of it, `benchmarks/r7rs/src/bv2string.scm`, whose random-number
 * generator divides 27 million times, 12% faster in Node and 5% in Chrome.
 * @param {Function} general - The primitive for any numbers.
 * @param {function(number, number): number} divide - Two integers' result.
 * @returns {Function}
 */
function dividingIntegers(general, divide) {
    return function (a, b) {
        return arguments.length === 2 && typeof a === 'number' && typeof b === 'number'
            && Number.isInteger(a) && Number.isInteger(b) && b !== 0
            ? divide(a, b) + 0
            : general(...arguments);
    };
}

/** Every primitive as wrapped, before any is given a direct path. */
const general = { ...mathPrimitives };

{
    mathPrimitives['+'] = foldingReals(general['+'], addReals, 0, genericAdd);
    mathPrimitives['*'] = foldingReals(general['*'], mulReals, 1, genericMul);
    const subtract = foldingReals(general['-'], subReals, undefined, genericSub);
    mathPrimitives['-'] = (...args) => {
        if (args.length === 1 && typeof args[0] === 'number') {
            const x = args[0];
            return Number.isInteger(x) ? 0 - x : -x;
        }
        return subtract(...args);
    };
    mathPrimitives['='] = comparingReals(general['='], equalReals);
    mathPrimitives['<'] = comparingReals(general['<'], lessReals);
    mathPrimitives['>'] = comparingReals(general['>'], (a, b) => lessReals(b, a));
    mathPrimitives['<='] = comparingReals(general['<='], lessEqualReals);
    mathPrimitives['>='] = comparingReals(general['>='], (a, b) => lessEqualReals(b, a));
    mathPrimitives['quotient'] = dividingIntegers(general['quotient'], (a, b) => (a - a % b) / b);
    mathPrimitives['remainder'] = dividingIntegers(general['remainder'], (a, b) => a % b);
    mathPrimitives['modulo'] = dividingIntegers(general['modulo'], (a, b) => {
        const r = a % b;
        return r !== 0 && (r < 0) !== (b < 0) ? r + b : r;
    });
}

// =============================================================================
// Numbers and boxes taken directly
// =============================================================================
//
// A wrapped primitive converts each argument -- an exact integer to a BigInt
// -- and its result back, in a function every primitive shares, whose call of
// the primitive V8 therefore cannot inline: `(inexact x)` cost 33 ns a call,
// against 4.6 before exact integers were numbers, and two such calls a point
// made benchmarks/r7rs/src/mbrot.scm 40% slower. Each primitive here takes an
// argument held as a number, a Flonum or a BigInt as it is, in a function of
// its own, and leaves anything else, and every error, to the wrapped one.
// Each answers what the wrapped one does: an inexact result through
// `inexactReal`, which boxes it if it is an integer.

/**
 * The double held by an inexact real, or undefined for any other value.
 * @param {*} x - The value.
 * @returns {number|undefined}
 */
function inexactDouble(x) {
    if (typeof x === 'number') return Number.isInteger(x) ? undefined : x;
    return x instanceof Flonum ? x.value : undefined;
}

/**
 * Whether a value is a real held as a number, a Flonum or a BigInt.
 * @param {*} x - The value.
 * @returns {boolean}
 */
function isHeldReal(x) {
    return typeof x === 'number' || x instanceof Flonum || typeof x === 'bigint';
}

/**
 * A one-argument function of the reals JavaScript's Math computes, which
 * converts an exact argument to its nearest double as the wrapped primitive
 * does, for an argument held as a number or a Flonum.
 * @param {string} name - The primitive's name.
 * @param {function(number): number} f - The Math function.
 * @param {function(number): boolean} [inDomain] - Whether the function is
 *   real there; elsewhere the wrapped primitive gives the complex value.
 * @returns {Function}
 */
function directMath(name, f, inDomain) {
    const wrapped = general[name];
    return function (x) {
        if (arguments.length === 1) {
            const d = heldDouble(x);
            if (d !== undefined && (inDomain === undefined || inDomain(d))) return inexactReal(f(d));
        }
        return wrapped.apply(null, arguments);
    };
}

mathPrimitives['inexact'] = function inexact(z) {
    if (arguments.length === 1) {
        if (typeof z === 'number') return Number.isInteger(z) ? inexactReal(z) : z;
        if (z instanceof Flonum) return z;
    }
    return general['inexact'].apply(null, arguments);
};

mathPrimitives['exact'] = function exact(z) {
    if (arguments.length === 1) {
        if (typeof z === 'number' && Number.isInteger(z)) return z;
        // `+ 0` makes -0.0 exact zero.
        if (z instanceof Flonum && Number.isSafeInteger(z.value)) return z.value + 0;
    }
    return general['exact'].apply(null, arguments);
};
mathPrimitives['inexact->exact'] = mathPrimitives['exact'];

mathPrimitives['abs'] = function abs(x) {
    if (arguments.length === 1) {
        if (typeof x === 'number') return x < 0 ? -x : x;
        if (x instanceof Flonum) return inexactReal(Math.abs(x.value));
    }
    return general['abs'].apply(null, arguments);
};

mathPrimitives['magnitude'] = function magnitude(z) {
    if (arguments.length === 1) {
        if (typeof z === 'number') return z < 0 ? -z : z;
        if (z instanceof Flonum) return inexactReal(Math.abs(z.value));
        if (z instanceof Complex) return fromTower(z.magnitude());
    }
    return general['magnitude'].apply(null, arguments);
};

mathPrimitives['real-part'] = function realPart(z) {
    if (arguments.length === 1) {
        if (isHeldReal(z)) return z;
        if (z instanceof Complex) return fromTower(z.real);
    }
    return general['real-part'].apply(null, arguments);
};

mathPrimitives['imag-part'] = function imagPart(z) {
    if (arguments.length === 1) {
        // An inexact real's imaginary part is inexact zero, as the tower has it.
        if (typeof z === 'number') return Number.isInteger(z) ? 0 : inexactReal(0);
        if (z instanceof Flonum) return inexactReal(0);
        if (typeof z === 'bigint') return 0;
        if (z instanceof Complex) return fromTower(z.imag);
    }
    return general['imag-part'].apply(null, arguments);
};

/**
 * A rounding of a real to an integer: an exact integer is its own, an
 * inexact integer too, and a fraction is rounded by `round`.
 * @param {string} name - The primitive's name.
 * @param {function(number): number} round - The rounding of a double.
 * @returns {Function}
 */
function directRounding(name, round) {
    const wrapped = general[name];
    return function (x) {
        if (arguments.length === 1) {
            if (typeof x === 'number') return Number.isInteger(x) ? x : inexactReal(round(x));
            if (x instanceof Flonum) return x;
        }
        return wrapped.apply(null, arguments);
    };
}

mathPrimitives['floor'] = directRounding('floor', Math.floor);
mathPrimitives['ceiling'] = directRounding('ceiling', Math.ceil);
mathPrimitives['truncate'] = directRounding('truncate', Math.trunc);
mathPrimitives['round'] = directRounding('round', (x) => {
    // Math.round takes a half up; R7RS 6.2.6 takes it to the even integer.
    const r = Math.round(x);
    return r - x === 0.5 && r % 2 !== 0 ? r - 1 : r;
});

mathPrimitives['square'] = function square(z) {
    if (arguments.length === 1) {
        if (typeof z === 'number') return Number.isInteger(z) ? mulNumbers(z, z) : inexactReal(z * z);
        if (z instanceof Flonum) return inexactReal(z.value * z.value);
    }
    return general['square'].apply(null, arguments);
};

// An exact argument's square root and logarithm are the wrapped primitive's:
// a square's root is exact, and an exact number beyond a double's range is
// computed from its exact value.
mathPrimitives['sqrt'] = function sqrt(z) {
    if (arguments.length === 1) {
        const d = inexactDouble(z);
        // -0.0 is not below zero, and is its own root.
        if (d !== undefined && d >= 0) return inexactReal(Math.sqrt(d));
    }
    return general['sqrt'].apply(null, arguments);
};

mathPrimitives['log'] = function log(z) {
    if (arguments.length === 1) {
        const d = inexactDouble(z);
        if (d !== undefined && d > 0) return inexactReal(Math.log(d));
    }
    return general['log'].apply(null, arguments);
};

mathPrimitives['exp'] = directMath('exp', Math.exp);
mathPrimitives['sin'] = directMath('sin', Math.sin);
mathPrimitives['cos'] = directMath('cos', Math.cos);
mathPrimitives['tan'] = directMath('tan', Math.tan);
mathPrimitives['asin'] = directMath('asin', Math.asin, (d) => d >= -1 && d <= 1);
mathPrimitives['acos'] = directMath('acos', Math.acos, (d) => d >= -1 && d <= 1);
mathPrimitives['atan'] = function atan(y, x) {
    const dy = heldDouble(y);
    if (dy !== undefined) {
        if (arguments.length === 1) return inexactReal(Math.atan(dy));
        const dx = heldDouble(x);
        if (arguments.length === 2 && dx !== undefined) return inexactReal(Math.atan2(dy, dx));
    }
    return general['atan'].apply(null, arguments);
};

mathPrimitives['/'] = function divide(a, b) {
    // Two reals, either inexact, are divided as doubles, as the tower
    // divides them -- by an exact zero too, which only an exact division
    // refuses.
    if (arguments.length === 2) {
        const da = heldDouble(a), db = heldDouble(b);
        if (da !== undefined && db !== undefined && (inexactDouble(a) !== undefined || inexactDouble(b) !== undefined)) {
            return inexactReal(da / db);
        }
    }
    return general['/'].apply(null, arguments);
};

// The predicates on numbers. A number is an exact integer if it is an
// integer, and otherwise inexact; a Flonum is an inexact integer; a BigInt an
// exact integer.

/**
 * A predicate on numbers that answers for a real held as a number, a Flonum
 * or a BigInt by `answer`, and leaves anything else to the wrapped one.
 * @param {string} name - The primitive's name.
 * @param {function(*): boolean} answer - The answer for such a real.
 * @returns {Function}
 */
function directPredicate(name, answer) {
    const wrapped = general[name];
    return function (x) {
        if (arguments.length === 1 && isHeldReal(x)) return answer(x);
        return wrapped.apply(null, arguments);
    };
}

mathPrimitives['number?'] = directPredicate('number?', () => true);
mathPrimitives['complex?'] = directPredicate('complex?', () => true);
mathPrimitives['real?'] = directPredicate('real?', () => true);
mathPrimitives['rational?'] = directPredicate('rational?', (x) => typeof x !== 'number' || Number.isFinite(x));
mathPrimitives['integer?'] = directPredicate('integer?', (x) => typeof x !== 'number' || Number.isInteger(x));
mathPrimitives['exact-integer?'] = directPredicate('exact-integer?',
    (x) => typeof x === 'number' ? Number.isInteger(x) : typeof x === 'bigint');
mathPrimitives['exact?'] = directPredicate('exact?',
    (x) => typeof x === 'number' ? Number.isInteger(x) : typeof x === 'bigint');
mathPrimitives['inexact?'] = directPredicate('inexact?',
    (x) => typeof x === 'number' ? !Number.isInteger(x) : x instanceof Flonum);
mathPrimitives['nan?'] = directPredicate('nan?', (x) => typeof x === 'number' && Number.isNaN(x));
mathPrimitives['finite?'] = directPredicate('finite?', (x) => typeof x !== 'number' || Number.isFinite(x));
mathPrimitives['infinite?'] = directPredicate('infinite?', (x) => x === Infinity || x === -Infinity);
