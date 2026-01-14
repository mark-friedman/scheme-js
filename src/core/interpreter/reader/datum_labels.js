/**
 * @fileoverview Datum label handling for circular structure support.
 * Implements R7RS #n= and #n# syntax for shared structure.
 */

import { Cons } from '../cons.js';
import { SchemeReadError } from '../errors.js';

/**
 * Placeholder for forward references in datum labels.
 * Used when #n# references a label that hasn't been fully read yet.
 */
export class Placeholder {
    constructor(id) {
        this.id = id;
        this.value = null; // Will be set when the labelled datum is fully read
        this.resolved = false;
    }
}

/**
 * Traverses the object graph replacing Placeholders with their resolved values.
 * Handles recursion and cycle detection using a visited Set.
 * @param {*} obj
 * @param {Set} visited
 * @returns {*} The fixed-up object
 */
export function fixup(obj, visited = new Set()) {
    if (obj === null || typeof obj !== 'object') {
        return obj;
    }

    if (visited.has(obj)) {
        return obj;
    }
    visited.add(obj);

    // Handle Cons pairs
    if (obj instanceof Cons) {
        if (obj.car instanceof Placeholder) {
            if (!obj.car.resolved) throw new SchemeReadError(`reference to undefined label #${obj.car.id}#`, 'datum label');
            obj.car = obj.car.value;
        } else {
            fixup(obj.car, visited);
        }

        if (obj.cdr instanceof Placeholder) {
            if (!obj.cdr.resolved) throw new SchemeReadError(`reference to undefined label #${obj.cdr.id}#`, 'datum label');
            obj.cdr = obj.cdr.value;
        } else {
            fixup(obj.cdr, visited);
        }
        return obj;
    }

    // Handle Vectors (Arrays)
    if (Array.isArray(obj)) {
        for (let i = 0; i < obj.length; i++) {
            if (obj[i] instanceof Placeholder) {
                if (!obj[i].resolved) throw new SchemeReadError(`reference to undefined label #${obj[i].id}#`, 'datum label');
                obj[i] = obj[i].value;
            } else {
                fixup(obj[i], visited);
            }
        }
        return obj;
    }

    return obj;
}
