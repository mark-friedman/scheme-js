
import { Cons, list, toArray } from '../interpreter/cons.js';

export const vectorPrimitives = {
    'vector': (...args) => args,

    'make-vector': (k, fill) => {
        if (typeof k !== 'number' || k < 0) {
            throw new Error("make-vector: expected non-negative integer size");
        }
        return new Array(k).fill(fill);
    },

    'vector?': (obj) => Array.isArray(obj),

    'vector-length': (vec) => {
        if (!Array.isArray(vec)) throw new Error("vector-length: expected vector");
        return vec.length;
    },

    'vector-ref': (vec, k) => {
        if (!Array.isArray(vec)) throw new Error("vector-ref: expected vector");
        if (typeof k !== 'number' || k < 0 || k >= vec.length) {
            throw new Error(`vector-ref: index ${k} out of bounds`);
        }
        return vec[k];
    },

    'vector-set!': (vec, k, obj) => {
        if (!Array.isArray(vec)) throw new Error("vector-set!: expected vector");
        if (typeof k !== 'number' || k < 0 || k >= vec.length) {
            throw new Error(`vector-set!: index ${k} out of bounds`);
        }
        vec[k] = obj;
        return null; // or undefined, Scheme set! returns unspecified
    },

    'vector->list': (vec) => {
        if (!Array.isArray(vec)) throw new Error("vector->list: expected vector");
        return list(...vec);
    },

    'list->vector': (lst) => {
        // lst is a Cons chain or null
        const arr = toArray(lst); // toArray handles proper lists
        return arr;
    }
};
