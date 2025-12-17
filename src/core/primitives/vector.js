/**
 * Vector Primitives for Scheme.
 * 
 * Provides vector (array) operations for the Scheme runtime.
 */

import { list, toArray } from '../interpreter/cons.js';
import { assertVector, assertInteger, assertIndex, assertArity } from '../interpreter/type_check.js';
import { SchemeRangeError } from '../interpreter/errors.js';

/**
 * Vector primitives exported to Scheme.
 */
export const vectorPrimitives = {
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

    /**
     * Vector type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a vector.
     */
    'vector?': (obj) => Array.isArray(obj),

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

    /**
     * Converts vector to list.
     * @param {Array} vec - A vector.
     * @returns {Cons|null} A list.
     */
    'vector->list': (vec) => {
        assertVector('vector->list', 1, vec);
        return list(...vec);
    },

    /**
     * Converts list to vector.
     * @param {Cons|null} lst - A list.
     * @returns {Array} A vector.
     */
    'list->vector': (lst) => {
        const arr = toArray(lst);
        return arr;
    }
};
