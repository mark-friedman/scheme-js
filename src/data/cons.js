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
     * Handles improper lists by pushing the Symbol '.' and final cdr as the last elements?
     * Or just throws/stops?
     * For debugging, let's try to be nice.
     */
    toArray() {
        const arr = [];
        let current = this;
        while (current instanceof Cons) {
            arr.push(current.car);
            current = current.cdr;
        }
        if (current !== null) {
            // Improper list end
            arr.push(new Symbol('.'));
            arr.push(current);
        }
        return arr;
    }
}

/**
 * Helper to create a cons cell.
 */
export function cons(car, cdr) {
    return new Cons(car, cdr);
}

/**
 * Helper to create a proper list from arguments.
 */
export function list(...args) {
    let head = null;
    for (let i = args.length - 1; i >= 0; i--) {
        head = new Cons(args[i], head);
    }
    return head;
}

// Circular dependency note: Symbol is needed for toString of improper lists if we want (a . b)
// But we can just use a string "." for now or import Symbol if available.
import { Symbol } from './symbol.js';
