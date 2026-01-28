/**
 * Cons Cell and List Utilities for Scheme.
 * 
 * This module provides the fundamental Cons pair type and list operations.
 */

import { Symbol } from './symbol.js';
import { SchemeTypeError } from './errors.js';

/**
 * Represents a Scheme pair (cons cell).
 */
export class Cons {
    /**
     * @param {*} car - The first element.
     * @param {*} cdr - The second element.
     */
    constructor(car, cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    toString() {
        return `(${this.toArray().join(' ')})`;
    }

    /**
     * Converts a proper list to a JS array.
     * For improper lists, includes a '.' Symbol and the final cdr.
     * @returns {Array} Array representation of the list.
     */
    toArray() {
        const arr = [];
        let current = this;
        while (current instanceof Cons) {
            arr.push(current.car);
            current = current.cdr;
        }
        if (current !== null) {
            // Improper list: include dot notation
            arr.push(new Symbol('.'));
            arr.push(current);
        }
        return arr;
    }
}

// =============================================================================
// Constructors
// =============================================================================

/**
 * Creates a cons cell.
 * @param {*} car - The first element.
 * @param {*} cdr - The second element.
 * @returns {Cons}
 */
export function cons(car, cdr) {
    return new Cons(car, cdr);
}

/**
 * Creates a proper list from arguments.
 * @param {...*} args - Elements of the list.
 * @returns {Cons|null} A proper list, or null for empty.
 */
export function list(...args) {
    let head = null;
    for (let i = args.length - 1; i >= 0; i--) {
        head = new Cons(args[i], head);
    }
    return head;
}

// =============================================================================
// Basic Accessors
// =============================================================================

/**
 * Returns the car of a pair, or null if given null.
 * @param {Cons|null} pair
 * @returns {*}
 */
export function car(pair) {
    return pair === null ? null : pair.car;
}

/**
 * Returns the cdr of a pair, or null if given null.
 * @param {Cons|null} pair
 * @returns {*}
 */
export function cdr(pair) {
    return pair === null ? null : pair.cdr;
}

// =============================================================================
// Compound Accessors (cadr, cddr, etc.)
// =============================================================================

/** @param {Cons} cons - (car (cdr cons)) */
export function cadr(cons) { return cons.cdr.car; }

/** @param {Cons} cons - (cdr (cdr cons)) */
export function cddr(cons) { return cons.cdr.cdr; }

/** @param {Cons} cons - (car (cdr (cdr cons))) */
export function caddr(cons) { return cons.cdr.cdr.car; }

/** @param {Cons} cons - (cdr (cdr (cdr cons))) */
export function cdddr(cons) { return cons.cdr.cdr.cdr; }

/** @param {Cons} cons - (car (cdr (cdr (cdr cons)))) */
export function cadddr(cons) { return cons.cdr.cdr.cdr.car; }

// =============================================================================
// List Utilities
// =============================================================================

/**
 * Converts a Scheme list to a JavaScript array.
 * @param {Cons|null} list - A proper Scheme list.
 * @returns {Array} JavaScript array of the list elements.
 * @throws {Error} If input is not a proper list.
 */
export function toArray(list) {
    if (list === null) return [];
    if (!(list instanceof Cons)) {
        throw new SchemeTypeError('toArray', 1, 'list', list);
    }
    return list.toArray();
}

/**
 * Maps a function over a Scheme list, returning a JavaScript array.
 * @param {Cons|null} list - A proper Scheme list.
 * @param {Function} fn - Function to apply to each element.
 * @returns {Array} JavaScript array of mapped results.
 */
export function mapCons(list, fn) {
    const result = [];
    let curr = list;
    while (curr instanceof Cons) {
        result.push(fn(curr.car));
        curr = curr.cdr;
    }
    return result;
}
