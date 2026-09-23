/**
 * @fileoverview The JavaScript core under SRFI 125 hash tables.
 *
 * Everything a hash table *does* is Scheme, in
 * `src/extras/scheme/hash_table.scm`. What lives here is the one thing Scheme
 * cannot express: a store with constant-time lookup by key. It is a JavaScript
 * `Map` with the key normalised first, so that the `Map`'s own notion of
 * sameness coincides with a Scheme equivalence.
 *
 * ## Which equivalences a store can hold
 *
 * `Map` compares keys by SameValueZero: identity for objects, value for
 * strings, numbers and BigInts. That is already `eq?` here, and nearly `eqv?`.
 * Four equivalences can therefore be stored natively, with no Scheme procedure
 * called on any lookup:
 *
 *  - `eq`: the key as it is.
 *  - `eqv`: the key as it is, except for the values where SameValueZero and
 *    `eqv?` disagree. `-0.0` and `0.0` are one `Map` key but distinct under
 *    `eqv?`; characters, exact rationals and complex numbers are objects that
 *    `eqv?` compares by value. Those are stored under a canonical string, in a
 *    second `Map` so that no canonical string can collide with a string key.
 *  - `string`: the key as it is, which must be a string. Scheme strings are
 *    JavaScript strings, so the `Map` compares their contents.
 *  - `string-ci`: the key folded to lower case, which is exactly what
 *    `string-ci=?` compares. The original key is kept for `hash-table-keys`.
 *
 * Any other equivalence -- `equal?`, or a comparator the library does not
 * recognise -- has to call a Scheme predicate to compare keys. That is done in
 * Scheme, which buckets its entries in an `eqv` store keyed by hash value, so
 * JavaScript never calls back into Scheme and a continuation captured inside a
 * user's predicate behaves as it would anywhere else.
 *
 * The standard hash functions that need primitive access to their argument --
 * the bits of a float, the code units of a string -- are here too; the rest
 * are Scheme.
 */

import { cons } from '../../core/interpreter/cons.js';
import { Char } from '../../core/primitives/char_class.js';
import { Rational } from '../../core/primitives/rational.js';
import { Complex } from '../../core/primitives/complex.js';
import { Symbol } from '../../core/interpreter/symbol.js';
import { assertString } from '../../core/interpreter/type_check.js';
import { SchemeTypeError } from '../../core/interpreter/errors.js';

// =============================================================================
// The store
// =============================================================================

/** The equivalences a store can hold natively. */
const KINDS = new Set(['eq', 'eqv', 'string', 'string-ci']);

/**
 * The canonical form of a key whose `eqv?` identity is not its `Map` identity.
 *
 * @param {*} key - A key.
 * @returns {(string|null)} A string that two keys share exactly when they are
 *   `eqv?`, or null when the key can be used in a `Map` as it is.
 */
function eqvCanonical(key) {
  if (typeof key === 'number') {
    return Object.is(key, -0) ? '-0' : null;
  }
  if (key instanceof Char) {
    return `c${key.codePoint}`;
  }
  if (key instanceof Rational) {
    return `q${key.numerator}/${key.denominator}${key.exact ? 'e' : 'i'}`;
  }
  if (key instanceof Complex) {
    return `z${partCanonical(key.real)},${partCanonical(key.imag)}`;
  }
  return null;
}

/**
 * The canonical form of one part of a complex number.
 * @param {(number|bigint|Rational)} part - A real part or imaginary part.
 * @returns {string} A string that two parts share exactly when they are `eqv?`.
 */
function partCanonical(part) {
  if (typeof part === 'bigint') return `${part}n`;
  if (typeof part === 'number') return Object.is(part, -0) ? '-0' : String(part);
  return eqvCanonical(part);
}

/**
 * A key-value store with constant-time lookup under one of four equivalences.
 *
 * Keys the `Map` can compare directly go in `direct`, which maps key to value.
 * Keys that need a canonical form go in `canonical`, which maps that form to a
 * `[key, value]` pair so the key the caller stored can be given back.
 */
export class HashStore {
  /**
   * @param {string} kind - One of `eq`, `eqv`, `string`, `string-ci`.
   */
  constructor(kind) {
    this.kind = kind;
    /** @type {Map<*, *>} */
    this.direct = new Map();
    /** @type {Map<string, Array>} */
    this.canonical = new Map();
  }

  /**
   * The canonical form of a key in this store, or null to use it as it is.
   * @param {*} key - A key.
   * @param {string} who - The procedure name, for error messages.
   * @returns {(string|null)}
   */
  canonicalize(key, who) {
    switch (this.kind) {
      case 'eq':
        return null;
      case 'eqv':
        return eqvCanonical(key);
      case 'string':
        assertString(who, 2, key);
        return null;
      default:
        assertString(who, 2, key);
        return key.toLowerCase();
    }
  }

  /** @returns {number} The number of associations. */
  get size() {
    return this.direct.size + this.canonical.size;
  }
}

/**
 * Checks that a value is a store.
 * @param {string} who - The procedure name, for error messages.
 * @param {*} store - The value.
 * @returns {HashStore}
 */
function assertStore(who, store) {
  if (!(store instanceof HashStore)) {
    throw new SchemeTypeError(who, 1, 'hash store', store);
  }
  return store;
}

// =============================================================================
// Hash functions
// =============================================================================

/**
 * One more than the largest hash any function here returns.
 *
 * Small enough that combining hashes in Scheme stays cheap, large enough that
 * no table in practice runs out of distinct values.
 */
export const HASH_BOUND = 2 ** 30;

/**
 * FNV-1a over the UTF-16 code units of a string.
 * @param {string} str - The string.
 * @returns {number} A hash below `HASH_BOUND`.
 */
function hashString(str) {
  let h = 0x811c9dc5;
  for (let i = 0; i < str.length; i++) {
    h ^= str.charCodeAt(i);
    h = Math.imul(h, 0x01000193);
  }
  return (h >>> 0) % HASH_BOUND;
}

/** Scratch space for reading the bits of a float. */
const FLOAT_BITS = new DataView(new ArrayBuffer(8));

/**
 * Hashes a real number so that numbers equal under `=` hash equally.
 *
 * Every real is reduced to the nearest double first. Two numbers that are `=`
 * reduce to the same double -- that is what makes `(= 1 1.0)` and
 * `(= 1/2 0.5)` share a hash -- and numbers that are not `=` sometimes do too,
 * which is only a collision.
 *
 * @param {(number|bigint|Rational)} x - A real number.
 * @returns {number} A hash below `HASH_BOUND`.
 */
function hashReal(x) {
  let d;
  if (typeof x === 'number') d = x;
  else if (typeof x === 'bigint') d = Number(x);
  else d = Number(x.numerator) / Number(x.denominator);

  if (Number.isNaN(d)) return 0x7ff8;
  if (d === 0) return 0; // also -0, which is = to 0
  if (Number.isSafeInteger(d)) {
    return ((d % HASH_BOUND) + HASH_BOUND) % HASH_BOUND;
  }
  FLOAT_BITS.setFloat64(0, d);
  const h = Math.imul(FLOAT_BITS.getUint32(0), 0x01000193) ^ FLOAT_BITS.getUint32(4);
  return (h >>> 0) % HASH_BOUND;
}

/**
 * Whether a value is a Scheme number.
 * @param {*} x - The value.
 * @returns {boolean}
 */
function isNumber(x) {
  return typeof x === 'number' || typeof x === 'bigint'
    || x instanceof Rational || x instanceof Complex;
}

// =============================================================================
// Primitives
// =============================================================================

/**
 * Hash-store primitives and the native hash functions.
 *
 * The `%`-prefixed names are the store, for `hash_table.scm` alone. The hash
 * functions carry their SRFI names because the libraries export them as they
 * are; each accepts and ignores a second argument, which SRFI 125 requires of
 * its deprecated versions and SRFI 128's do not forbid, so one procedure serves
 * both.
 */
export const hashTablePrimitives = {
  /**
   * Creates an empty store.
   * @param {Symbol} kind - `eq`, `eqv`, `string` or `string-ci`.
   * @returns {HashStore}
   */
  '%make-hash-store': (kind) => {
    const name = kind instanceof Symbol ? kind.name : kind;
    if (!KINDS.has(name)) {
      throw new SchemeTypeError('%make-hash-store', 1, 'store kind', kind);
    }
    return new HashStore(name);
  },

  /**
   * Returns the value stored under a key, or a default.
   * @param {HashStore} store - The store.
   * @param {*} key - The key.
   * @param {*} dflt - Returned when the key is absent.
   * @returns {*}
   */
  '%hash-store-ref': (store, key, dflt) => {
    const c = assertStore('%hash-store-ref', store).canonicalize(key, 'hash-table-ref');
    if (c === null) {
      const value = store.direct.get(key);
      return value !== undefined || store.direct.has(key) ? value : dflt;
    }
    const entry = store.canonical.get(c);
    return entry === undefined ? dflt : entry[1];
  },

  /**
   * Stores a value under a key, replacing any previous value. A key already
   * present keeps the form it was first stored in.
   * @param {HashStore} store - The store.
   * @param {*} key - The key.
   * @param {*} value - The value.
   * @returns {boolean} True.
   */
  '%hash-store-set!': (store, key, value) => {
    const c = assertStore('%hash-store-set!', store).canonicalize(key, 'hash-table-set!');
    if (c === null) {
      store.direct.set(key, value);
    } else {
      const entry = store.canonical.get(c);
      if (entry === undefined) store.canonical.set(c, [key, value]);
      else entry[1] = value;
    }
    return true;
  },

  /**
   * Removes a key.
   * @param {HashStore} store - The store.
   * @param {*} key - The key.
   * @returns {boolean} Whether the key was present.
   */
  '%hash-store-delete!': (store, key) => {
    const c = assertStore('%hash-store-delete!', store).canonicalize(key, 'hash-table-delete!');
    return c === null ? store.direct.delete(key) : store.canonical.delete(c);
  },

  /**
   * Whether a key is present.
   * @param {HashStore} store - The store.
   * @param {*} key - The key.
   * @returns {boolean}
   */
  '%hash-store-contains?': (store, key) => {
    const c = assertStore('%hash-store-contains?', store).canonicalize(key, 'hash-table-contains?');
    return c === null ? store.direct.has(key) : store.canonical.has(c);
  },

  /**
   * The number of associations.
   * @param {HashStore} store - The store.
   * @returns {bigint}
   */
  '%hash-store-size': (store) => BigInt(assertStore('%hash-store-size', store).size),

  /**
   * Removes every association.
   * @param {HashStore} store - The store.
   * @returns {boolean} True.
   */
  '%hash-store-clear!': (store) => {
    assertStore('%hash-store-clear!', store);
    store.direct.clear();
    store.canonical.clear();
    return true;
  },

  /**
   * The keys, as stored.
   * @param {HashStore} store - The store.
   * @returns {Cons|null} A newly allocated list.
   */
  '%hash-store-keys': (store) => {
    assertStore('%hash-store-keys', store);
    let result = null;
    for (const key of store.direct.keys()) result = cons(key, result);
    for (const entry of store.canonical.values()) result = cons(entry[0], result);
    return result;
  },

  /**
   * The values, in the same order as `%hash-store-keys` gives the keys.
   * @param {HashStore} store - The store.
   * @returns {Cons|null} A newly allocated list.
   */
  '%hash-store-values': (store) => {
    assertStore('%hash-store-values', store);
    let result = null;
    for (const value of store.direct.values()) result = cons(value, result);
    for (const entry of store.canonical.values()) result = cons(entry[1], result);
    return result;
  },

  /**
   * One key, when the store is not empty.
   * @param {HashStore} store - The store.
   * @param {*} dflt - Returned when the store is empty.
   * @returns {*}
   */
  '%hash-store-some-key': (store, dflt) => {
    assertStore('%hash-store-some-key', store);
    for (const key of store.direct.keys()) return key;
    for (const entry of store.canonical.values()) return entry[0];
    return dflt;
  },

  /**
   * A new store with the same kind and associations.
   * @param {HashStore} store - The store.
   * @returns {HashStore}
   */
  '%hash-store-copy': (store) => {
    assertStore('%hash-store-copy', store);
    const copy = new HashStore(store.kind);
    copy.direct = new Map(store.direct);
    for (const [c, entry] of store.canonical) copy.canonical.set(c, [entry[0], entry[1]]);
    return copy;
  },

  /**
   * SRFI 128 `string-hash`.
   * @param {string} str - The string.
   * @returns {bigint}
   */
  'string-hash': (str, _bound) => {
    assertString('string-hash', 1, str);
    return BigInt(hashString(str));
  },

  /**
   * SRFI 128 `string-ci-hash`: equal for strings equal under `string-ci=?`.
   * @param {string} str - The string.
   * @returns {bigint}
   */
  'string-ci-hash': (str, _bound) => {
    assertString('string-ci-hash', 1, str);
    return BigInt(hashString(str.toLowerCase()));
  },

  /**
   * SRFI 128 `number-hash`: equal for numbers equal under `=`.
   * @param {*} x - The number.
   * @returns {bigint}
   */
  'number-hash': (x, _bound) => {
    if (!isNumber(x)) throw new SchemeTypeError('number-hash', 1, 'number', x);
    if (x instanceof Complex) {
      const imag = hashReal(x.imag);
      // A complex number with a zero imaginary part is = to its real part.
      return BigInt(imag === 0
        ? hashReal(x.real)
        : ((Math.imul(hashReal(x.real), 31) + imag) >>> 0) % HASH_BOUND);
    }
    return BigInt(hashReal(x));
  },

  /**
   * A hash consistent with `eqv?` for values `eqv?` compares by identity --
   * records, procedures, JavaScript objects -- which have no content to hash.
   * Each object is numbered the first time it is asked about; the numbering is
   * weak, so it never keeps an object alive.
   * @param {*} x - The value.
   * @returns {bigint}
   */
  '%identity-hash': (x) => {
    if ((typeof x !== 'object' && typeof x !== 'function') || x === null) {
      return BigInt(hashString(typeof x));
    }
    let id = IDENTITIES.get(x);
    if (id === undefined) {
      id = BigInt(nextIdentity);
      nextIdentity = (nextIdentity + 1) % HASH_BOUND;
      IDENTITIES.set(x, id);
    }
    return id;
  },

  /**
   * The bound on the hashes of this library's hash functions.
   * @returns {bigint}
   */
  '%hash-bound': () => BigInt(HASH_BOUND),

  /**
   * A salt chosen afresh on each run, below the bound.
   * @returns {bigint}
   */
  '%hash-salt': () => SALT
};

/** The numbers handed out by `%identity-hash`. */
const IDENTITIES = new WeakMap();
let nextIdentity = 1;

/** The salt for this run; see `%hash-salt`. */
const SALT = BigInt(Math.floor(Math.random() * HASH_BOUND));
