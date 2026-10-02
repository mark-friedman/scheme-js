/**
 * @fileoverview The JavaScript core under SRFI 151 bitwise operations.
 *
 * Everything SRFI 151 defines is Scheme, in `src/extras/scheme/bitwise.scm`,
 * except what needs JavaScript's `BigInt` operators: an exact integer is a
 * `BigInt`, whose `&`, `|`, `^` and shifts already treat it as an infinite
 * two's-complement bit string, which is exactly SRFI 151's model, and which
 * Scheme arithmetic could only imitate a bit at a time. Population count and
 * length are here for the same reason: both read the integer's binary digits.
 *
 * The names are `%`-prefixed and `151.sld` exports them under SRFI 151's own,
 * so that a program sees them only by importing the library. The associative
 * operations take any number of arguments themselves rather than through a
 * Scheme wrapper, since a wrapper's rest list would be allocated on every call
 * and they are what a bit set's union, intersection and difference are made of.
 */

import { isRational } from '../../core/primitives/rational.js';
import { SchemeTypeError } from '../../core/interpreter/errors.js';

/**
 * An argument as a `BigInt`, which it must be an exact integer to become.
 * @param {string} who - The procedure, for the message.
 * @param {number} position - The argument's position, from 1.
 * @param {*} value - The argument.
 * @returns {bigint} Its value.
 */
function exactInteger(who, position, value) {
  if (typeof value === 'bigint') return value;
  if (isRational(value) && value.exact !== false && value.denominator === 1n) return value.numerator;
  throw new SchemeTypeError(who, position, 'exact integer', value);
}

/**
 * The number of 1 bits in a non-negative integer.
 * @param {bigint} n - The integer, at least 0.
 * @returns {bigint} The count.
 */
function popCount(n) {
  let count = 0n;
  for (const digit of n.toString(2)) if (digit === '1') count++;
  return count;
}

/**
 * The bitwise primitives: the operations that need `BigInt`'s operators.
 */
export const bitwisePrimitives = {
  /**
   * SRFI 151's `bitwise-and`: the bits set in every argument.
   * @param {...bigint} is - Exact integers.
   * @returns {bigint} -1 for none.
   */
  '%bitwise-and': (...is) => is.reduce((acc, i, k) => acc & exactInteger('bitwise-and', k + 1, i), -1n),

  /**
   * SRFI 151's `bitwise-ior`: the bits set in any argument.
   * @param {...bigint} is - Exact integers.
   * @returns {bigint} 0 for none.
   */
  '%bitwise-ior': (...is) => is.reduce((acc, i, k) => acc | exactInteger('bitwise-ior', k + 1, i), 0n),

  /**
   * SRFI 151's `bitwise-xor`: the bits set in an odd number of arguments.
   * @param {...bigint} is - Exact integers.
   * @returns {bigint} 0 for none.
   */
  '%bitwise-xor': (...is) => is.reduce((acc, i, k) => acc ^ exactInteger('bitwise-xor', k + 1, i), 0n),

  /**
   * SRFI 151's `arithmetic-shift`: shifted left by `count`, or right, rounding
   * towards negative infinity, by minus `count`.
   * @param {bigint} i - An exact integer.
   * @param {bigint} count - An exact integer.
   * @returns {bigint}
   */
  '%arithmetic-shift': (i, count) => {
    const n = exactInteger('arithmetic-shift', 1, i);
    const k = exactInteger('arithmetic-shift', 2, count);
    return k >= 0n ? n << k : n >> -k;
  },

  /**
   * SRFI 151's `bit-count`: how many bits differ from the sign -- the 1 bits
   * of a non-negative integer, the 0 bits of a negative one.
   * @param {bigint} i - An exact integer.
   * @returns {bigint}
   */
  '%bit-count': (i) => {
    const n = exactInteger('bit-count', 1, i);
    return popCount(n >= 0n ? n : -n - 1n);
  },

  /**
   * SRFI 151's `integer-length`: how many bits the integer needs beside its
   * sign.
   * @param {bigint} i - An exact integer.
   * @returns {bigint}
   */
  '%integer-length': (i) => {
    const n = exactInteger('integer-length', 1, i);
    const magnitude = n >= 0n ? n : -n - 1n;
    return magnitude === 0n ? 0n : BigInt(magnitude.toString(2).length);
  }
};
