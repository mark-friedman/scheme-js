/**
 * @fileoverview How Scheme's numbers are represented, and the conversions
 * between that and the representation the numeric tower computes in.
 *
 * A Scheme number is, by kind:
 *
 *  - an **exact integer**: a JavaScript number that is an integer in the safe
 *    range, |n| <= 2^53 - 1, or else, outside it, a `BigInt` -- never a
 *    `BigInt` inside it, so that each exact integer has one representation,
 *    and `===`, a `Map` and a hash table see equal integers as one;
 *  - an **exact non-integer rational**: a `Rational`;
 *  - an **inexact real**: a JavaScript number that is not an integer -- a
 *    fraction, an infinity or NaN -- or, for an inexact real whose value is an
 *    integer, 3.0 or -0.0, a `Flonum` holding it;
 *  - a **non-real complex**: a `Complex`.
 *
 * So a loop over small exact integers runs on JavaScript numbers, as a loop
 * written in JavaScript does, and an integral number arriving from JavaScript
 * is exact, as it always was.
 *
 * The numeric tower -- `math.js`, `rational.js` and `complex.js` -- computes
 * in the representation it was written in, which is simpler to compute in: an
 * exact integer a `BigInt`, whatever its size, and an inexact real a
 * JavaScript number, whatever its value (`toTower`, `fromTower`). A primitive
 * takes the common case, numbers, directly, and converts for the rest.
 */

import { Rational } from '../primitives/rational.js';

/**
 * An inexact real whose value is an integer: 3.0, -0.0, 1e300. A JavaScript
 * number with an integral value is an exact integer, so an inexact one needs
 * a box to be told apart.
 */
export class Flonum {
    /**
     * @param {number} value - The value, an integer as a double.
     */
    constructor(value) {
        this.value = value;
    }
}

/** The largest exact integer held as a JavaScript number, as a `BigInt`. */
const MAX_SAFE = BigInt(Number.MAX_SAFE_INTEGER);

/**
 * The exact integer a `BigInt` denotes, as Scheme holds it: a number in the
 * safe range, else the `BigInt`.
 * @param {bigint} b - The integer.
 * @returns {number|bigint}
 */
export function exactInteger(b) {
    return b >= -MAX_SAFE && b <= MAX_SAFE ? Number(b) : b;
}

/**
 * The inexact real a double denotes, as Scheme holds it: a box for an integral
 * value, which as a bare number would be exact; the number otherwise.
 * @param {number} n - The double.
 * @returns {number|Flonum}
 */
export function inexactReal(n) {
    return Number.isInteger(n) ? new Flonum(n) : n;
}

/**
 * Whether a value is an exact integer.
 * @param {*} x - The value.
 * @returns {boolean}
 */
export function isExactInteger(x) {
    return typeof x === 'bigint' || (typeof x === 'number' && Number.isInteger(x));
}

/**
 * Whether a value is an inexact real held unboxed or boxed: a non-integral
 * number, or a `Flonum`.
 * @param {*} x - The value.
 * @returns {boolean}
 */
export function isInexactReal(x) {
    return (typeof x === 'number' && !Number.isInteger(x)) || x instanceof Flonum;
}

/**
 * A Scheme number, as the numeric tower computes in it: an exact integer a
 * `BigInt`, an inexact real a JavaScript number; anything else as it is.
 * @param {*} x - The value.
 * @returns {*}
 */
export function toTower(x) {
    if (typeof x === 'number') return Number.isInteger(x) ? BigInt(x) : x;
    if (x instanceof Flonum) return x.value;
    return x;
}

/**
 * A value the numeric tower made, as Scheme holds it: a `BigInt` an exact
 * integer, a JavaScript number an inexact real, an exact rational of
 * denominator 1 its integer; anything else as it is.
 * @param {*} v - The value.
 * @returns {*}
 */
export function fromTower(v) {
    if (typeof v === 'bigint') return exactInteger(v);
    if (typeof v === 'number') return inexactReal(v);
    if (v instanceof Rational && v.denominator === 1n && v.exact !== false) return exactInteger(v.numerator);
    return v;
}

/**
 * An exact integer as a JavaScript number, for an index or a count, which is
 * one already unless it is too large to be an index.
 * @param {number|bigint} k - The integer.
 * @returns {number}
 */
export function exactToNumber(k) {
    return typeof k === 'number' ? k : Number(k);
}

/**
 * A real number's value as a double: an inexact one's own, an exact one's
 * nearest.
 * @param {*} x - A real number.
 * @returns {number}
 */
export function realToDouble(x) {
    if (typeof x === 'number') return x;
    if (x instanceof Flonum) return x.value;
    if (typeof x === 'bigint') return Number(x);
    if (x instanceof Rational) return x.toNumber();
    return NaN;
}

// =============================================================================
// Arithmetic on two JavaScript numbers
// =============================================================================
//
// What `+`, `-` and `*` do when both operands are JavaScript numbers, the
// common case, which the primitives and compiled code take first: the double
// result is right unless it is an integer, when it is exact only if both
// operands were -- and then only if it is in the safe range, beyond which the
// double has rounded and the exact result is taken again as a BigInt -- and is
// otherwise an inexact integer, boxed.

/**
 * The sum of two JavaScript numbers, as a Scheme number.
 * @param {number} a - One.
 * @param {number} b - The other.
 * @returns {number|bigint|Flonum}
 */
export function addNumbers(a, b) {
    const r = a + b;
    if (!Number.isInteger(r)) return r;
    if (Number.isInteger(a) && Number.isInteger(b)) {
        return Number.isSafeInteger(r) ? r : exactInteger(BigInt(a) + BigInt(b));
    }
    return new Flonum(r);
}

/**
 * The difference of two JavaScript numbers, as a Scheme number.
 * @param {number} a - The minuend.
 * @param {number} b - The subtrahend.
 * @returns {number|bigint|Flonum}
 */
export function subNumbers(a, b) {
    const r = a - b;
    if (!Number.isInteger(r)) return r;
    if (Number.isInteger(a) && Number.isInteger(b)) {
        return Number.isSafeInteger(r) ? r : exactInteger(BigInt(a) - BigInt(b));
    }
    return new Flonum(r);
}

/**
 * The product of two JavaScript numbers, as a Scheme number. An exact zero
 * times a negative integer is exact zero, not the double -0.
 * @param {number} a - One.
 * @param {number} b - The other.
 * @returns {number|bigint|Flonum}
 */
export function mulNumbers(a, b) {
    const r = a * b;
    if (!Number.isInteger(r)) return r;
    if (Number.isInteger(a) && Number.isInteger(b)) {
        if (Number.isSafeInteger(r)) return r === 0 ? 0 : r;
        return exactInteger(BigInt(a) * BigInt(b));
    }
    return new Flonum(r);
}

// =============================================================================
// Arithmetic and comparison on any two reals held as numbers, BigInts or boxes
// =============================================================================
//
// What `+`, `-`, `*` and the comparisons do for every operand held as a
// JavaScript number, a BigInt or a Flonum -- every real but a non-integer
// rational -- without the conversions the numeric tower needs: two exact
// integers in BigInt arithmetic, the result held as Scheme holds it; any
// inexact operand in double arithmetic, the result inexact. Each gives
// `undefined` for anything else -- a rational, a complex, a wrong type -- which
// the caller passes to the primitive. Compiled code calls them for its inline
// arithmetic (`R.add` and the rest in src/compiler/runtime.js), and the
// primitives take them first.

/**
 * A real's value as a double, for one known to be held as a number, a BigInt
 * or a Flonum.
 * @param {number|bigint|Flonum} x - The real.
 * @returns {number}
 */
function doubleOf(x) {
    return typeof x === 'number' ? x : (typeof x === 'bigint' ? Number(x) : x.value);
}

/**
 * How a value is held, for arithmetic: 1 an exact integer, 2 an inexact real
 * held as a number or a Flonum, 0 anything else.
 * @param {*} x - The value.
 * @returns {number}
 */
function realKind(x) {
    if (typeof x === 'number') return Number.isInteger(x) ? 1 : 2;
    if (typeof x === 'bigint') return 1;
    return x instanceof Flonum ? 2 : 0;
}

/**
 * The sum of two reals held as numbers, BigInts or Flonums, or `undefined`.
 * @param {*} a - One.
 * @param {*} b - The other.
 * @returns {*}
 */
export function addReals(a, b) {
    if (typeof a === 'number' && typeof b === 'number') return addNumbers(a, b);
    const ka = realKind(a), kb = realKind(b);
    if (ka === 0 || kb === 0) return undefined;
    if (ka === 1 && kb === 1) return exactInteger(BigInt(a) + BigInt(b));
    return inexactReal(doubleOf(a) + doubleOf(b));
}

/**
 * The difference of two reals held as numbers, BigInts or Flonums, or
 * `undefined`.
 * @param {*} a - The minuend.
 * @param {*} b - The subtrahend.
 * @returns {*}
 */
export function subReals(a, b) {
    if (typeof a === 'number' && typeof b === 'number') return subNumbers(a, b);
    const ka = realKind(a), kb = realKind(b);
    if (ka === 0 || kb === 0) return undefined;
    if (ka === 1 && kb === 1) return exactInteger(BigInt(a) - BigInt(b));
    return inexactReal(doubleOf(a) - doubleOf(b));
}

/**
 * The product of two reals held as numbers, BigInts or Flonums, or
 * `undefined`.
 * @param {*} a - One.
 * @param {*} b - The other.
 * @returns {*}
 */
export function mulReals(a, b) {
    if (typeof a === 'number' && typeof b === 'number') return mulNumbers(a, b);
    const ka = realKind(a), kb = realKind(b);
    if (ka === 0 || kb === 0) return undefined;
    if (ka === 1 && kb === 1) return exactInteger(BigInt(a) * BigInt(b));
    return inexactReal(doubleOf(a) * doubleOf(b));
}

/**
 * Two reals' values for comparing, or null if either is held otherwise. A
 * number and a BigInt compare exactly as they are, in JavaScript; a Flonum by
 * its value.
 * @param {*} x - A real.
 * @returns {number|bigint|null}
 */
function comparable(x) {
    if (typeof x === 'number' || typeof x === 'bigint') return x;
    return x instanceof Flonum ? x.value : null;
}

/**
 * Whether one real is less than another, for reals held as numbers, BigInts
 * or Flonums; `undefined` for anything else.
 * @param {*} a - One.
 * @param {*} b - The other.
 * @returns {boolean|undefined}
 */
export function lessReals(a, b) {
    if (typeof a === 'number' && typeof b === 'number') return a < b;
    const x = comparable(a), y = comparable(b);
    return x === null || y === null ? undefined : x < y;
}

/**
 * Whether two reals held as numbers, BigInts or Flonums are `=`, across
 * exactness; `undefined` for anything else. JavaScript's `==` compares a
 * number with a BigInt exactly, and NaN with nothing.
 * @param {*} a - One.
 * @param {*} b - The other.
 * @returns {boolean|undefined}
 */
export function equalReals(a, b) {
    if (typeof a === 'number' && typeof b === 'number') return a === b;
    const x = comparable(a), y = comparable(b);
    // eslint-disable-next-line eqeqeq
    return x === null || y === null ? undefined : x == y;
}

/**
 * Whether one real is less than or equal to another, for reals held as
 * numbers, BigInts or Flonums; `undefined` for anything else.
 * @param {*} a - One.
 * @param {*} b - The other.
 * @returns {boolean|undefined}
 */
export function lessEqualReals(a, b) {
    if (typeof a === 'number' && typeof b === 'number') return a <= b;
    const x = comparable(a), y = comparable(b);
    return x === null || y === null ? undefined : x <= y;
}
