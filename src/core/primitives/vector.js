/**
 * Vector Primitives for Scheme.
 * 
 * Provides vector (array) operations per R7RS §6.8.
 */

import { list, toArray } from '../interpreter/cons.js';
import {
    assertVector,
    assertInteger,
    assertIndex,
    assertArity,
    assertString,
    assertChar,
    isChar
} from '../interpreter/type_check.js';
import { SchemeRangeError, SchemeError } from '../interpreter/errors.js';

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Validates and returns start/end range for vector operations.
 * @param {string} procName - Procedure name
 * @param {Array} vec - The vector
 * @param {number|undefined} start - Start index (default 0)
 * @param {number|undefined} end - End index (default vec.length)
 * @returns {[number, number]} Validated [start, end]
 */
function validateRange(procName, vec, start, end) {
    const s = start === undefined ? 0 : start;
    const e = end === undefined ? vec.length : end;

    if (!Number.isInteger(s) || s < 0 || s > vec.length) {
        throw new SchemeRangeError(procName, 'start', 0, vec.length, s);
    }
    if (!Number.isInteger(e) || e < s || e > vec.length) {
        throw new SchemeRangeError(procName, 'end', s, vec.length, e);
    }
    return [s, e];
}

// =============================================================================
// Vector Primitives
// =============================================================================

/**
 * Vector primitives exported to Scheme.
 */
export const vectorPrimitives = {
    // -------------------------------------------------------------------------
    // Constructors
    // -------------------------------------------------------------------------

    /**
     * Creates a vector from arguments.
     * @param {...*} args - Elements of the vector.
     * @returns {Array} New vector.
     */
    'vector': (...args) => args,

    /**
     * Creates a vector of specified size.
     * @param {number} k - Size of vector.
     * @param {*} [fill] - Fill value.
     * @returns {Array} New vector.
     */
    'make-vector': (k, fill) => {
        assertInteger('make-vector', 1, k);
        if (k < 0) {
            throw new SchemeRangeError('make-vector', 'size', 0, Infinity, k);
        }
        return new Array(k).fill(fill);
    },

    // -------------------------------------------------------------------------
    // Type Predicate
    // -------------------------------------------------------------------------

    /**
     * Vector type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a vector.
     */
    'vector?': (obj) => Array.isArray(obj),

    // -------------------------------------------------------------------------
    // Accessors
    // -------------------------------------------------------------------------

    /**
     * Returns the length of a vector.
     * @param {Array} vec - A vector.
     * @returns {number} Length.
     */
    'vector-length': (vec) => {
        assertVector('vector-length', 1, vec);
        return vec.length;
    },

    /**
     * Returns element at index.
     * @param {Array} vec - A vector.
     * @param {number} k - Index.
     * @returns {*} Element at index.
     */
    'vector-ref': (vec, k) => {
        assertVector('vector-ref', 1, vec);
        assertIndex('vector-ref', k, vec.length);
        return vec[k];
    },

    /**
     * Sets element at index.
     * @param {Array} vec - A vector.
     * @param {number} k - Index.
     * @param {*} obj - New value.
     * @returns {null} Unspecified.
     */
    'vector-set!': (vec, k, obj) => {
        assertVector('vector-set!', 1, vec);
        assertIndex('vector-set!', k, vec.length);
        vec[k] = obj;
        return null;
    },

    // -------------------------------------------------------------------------
    // Mutation
    // -------------------------------------------------------------------------

    /**
     * Fills vector with a value (in-place).
     * @param {Array} vec - Vector to fill
     * @param {*} fill - Fill value
     * @param {number} [start] - Start index (default 0)
     * @param {number} [end] - End index (default length)
     * @returns {null} Unspecified
     */
    'vector-fill!': (vec, fill, start, end) => {
        assertVector('vector-fill!', 1, vec);
        const [s, e] = validateRange('vector-fill!', vec, start, end);
        for (let i = s; i < e; i++) {
            vec[i] = fill;
        }
        return null;
    },

    // -------------------------------------------------------------------------
    // Copying
    // -------------------------------------------------------------------------

    /**
     * Returns a copy of a vector.
     * @param {Array} vec - Source vector
     * @param {number} [start] - Start index (default 0)
     * @param {number} [end] - End index (default length)
     * @returns {Array} New vector
     */
    'vector-copy': (vec, start, end) => {
        assertVector('vector-copy', 1, vec);
        const [s, e] = validateRange('vector-copy', vec, start, end);
        return vec.slice(s, e);
    },

    /**
     * Copies elements from one vector to another (in-place).
     * @param {Array} to - Destination vector
     * @param {number} at - Destination start index
     * @param {Array} from - Source vector
     * @param {number} [start] - Source start index (default 0)
     * @param {number} [end] - Source end index (default length)
     * @returns {null} Unspecified
     */
    'vector-copy!': (to, at, from, start, end) => {
        assertVector('vector-copy!', 1, to);
        assertInteger('vector-copy!', 2, at);
        assertVector('vector-copy!', 3, from);

        if (at < 0 || at > to.length) {
            throw new SchemeRangeError('vector-copy!', 'at', 0, to.length, at);
        }

        const [s, e] = validateRange('vector-copy!', from, start, end);
        const count = e - s;

        if (at + count > to.length) {
            throw new SchemeRangeError('vector-copy!', 'range', 0, to.length - at, count);
        }

        // Handle overlapping copies correctly
        if (to === from && at > s && at < e) {
            // Copy backwards to avoid overwriting
            for (let i = count - 1; i >= 0; i--) {
                to[at + i] = from[s + i];
            }
        } else {
            for (let i = 0; i < count; i++) {
                to[at + i] = from[s + i];
            }
        }
        return null;
    },

    /**
     * Concatenates vectors.
     * @param {...Array} vecs - Vectors to concatenate
     * @returns {Array} New vector
     */
    'vector-append': (...vecs) => {
        vecs.forEach((v, i) => assertVector('vector-append', i + 1, v));
        return [].concat(...vecs);
    },

    // -------------------------------------------------------------------------
    // Conversion
    // -------------------------------------------------------------------------

    /**
     * Converts vector to list.
     * @param {Array} vec - A vector.
     * @param {number} [start] - Start index
     * @param {number} [end] - End index
     * @returns {Cons|null} A list.
     */
    'vector->list': (vec, start, end) => {
        assertVector('vector->list', 1, vec);
        const [s, e] = validateRange('vector->list', vec, start, end);
        return list(...vec.slice(s, e));
    },

    /**
     * Converts list to vector.
     * @param {Cons|null} lst - A list.
     * @returns {Array} A vector.
     */
    'list->vector': (lst) => {
        const arr = toArray(lst);
        return arr;
    },

    /**
     * Converts vector of characters to string.
     * @param {Array} vec - Vector of characters
     * @param {number} [start] - Start index
     * @param {number} [end] - End index
     * @returns {string} String
     */
    'vector->string': (vec, start, end) => {
        assertVector('vector->string', 1, vec);
        const [s, e] = validateRange('vector->string', vec, start, end);
        const slice = vec.slice(s, e);
        slice.forEach((c, i) => {
            if (!isChar(c)) {
                throw new SchemeError(
                    `vector->string: element ${s + i} is not a character`,
                    [c],
                    'vector->string'
                );
            }
        });
        return slice.join('');
    },

    /**
     * Converts string to vector of characters.
     * @param {string} str - String to convert
     * @param {number} [start] - Start index
     * @param {number} [end] - End index
     * @returns {Array} Vector of characters
     */
    'string->vector': (str, start, end) => {
        assertString('string->vector', 1, str);
        const s = start === undefined ? 0 : start;
        const e = end === undefined ? str.length : end;

        if (!Number.isInteger(s) || s < 0 || s > str.length) {
            throw new SchemeRangeError('string->vector', 'start', 0, str.length, s);
        }
        if (!Number.isInteger(e) || e < s || e > str.length) {
            throw new SchemeRangeError('string->vector', 'end', s, str.length, e);
        }

        return str.slice(s, e).split('');
    }
};

// Mark primitives that should receive raw Scheme objects (no JS bridge wrapping)
vectorPrimitives['vector'].skipBridge = true;
vectorPrimitives['make-vector'].skipBridge = true;
vectorPrimitives['vector-set!'].skipBridge = true;
vectorPrimitives['vector-ref'].skipBridge = true; // Returns raw object
vectorPrimitives['vector-fill!'].skipBridge = true;
