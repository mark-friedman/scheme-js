/**
 * @fileoverview What the printer asks of JavaScript: what only JavaScript can
 * say about a value the printer of `(scheme core)` (src/core/scheme/printer.scm)
 * is given -- whether it is an object written by its fields and what they are,
 * a procedure's name, whether it is a continuation, several values, and a host
 * value's own text. Apart from the printer's doors (printer.js), which start
 * the library system, so that compiled code that prints needs nothing of it.
 */

import { Cons, list } from '../../interpreter/cons.js';
import { Symbol } from '../../interpreter/symbol.js';
import { Port, EOF_OBJECT } from './ports.js';
import { Rational } from '../rational.js';
import { Complex } from '../complex.js';
import { Char } from '../char_class.js';
import { SchemeString } from '../string_class.js';
import { Flonum } from '../../interpreter/number_representation.js';
import { Values, isSchemeContinuation } from '../../interpreter/values.js';

// ============================================================================
// What the printer asks of JavaScript
// ============================================================================

/**
 * Whether a value is written as an object, `#{(key value) ...}`: a record,
 * a class's instance or another JavaScript object that is not one of the
 * values Scheme has a syntax for. An error object is not: its message is not
 * one of its fields, and it is written as its text, as the REPLs show it.
 * @param {*} val
 * @returns {boolean}
 */
function isHostObject(val) {
    return val !== null && typeof val === 'object'
        && !Array.isArray(val) && !(val instanceof Uint8Array) && !(val instanceof Cons)
        && !(val instanceof Port) && !(val instanceof Symbol) && !(val instanceof SchemeString)
        && val !== EOF_OBJECT && !(val instanceof Char) && !(val instanceof Rational)
        && !(val instanceof Complex) && !(val instanceof Flonum) && !(val instanceof Values)
        && !(val instanceof Error);
}

export const printerPrimitives = {
    /**
     * Whether a value is written as an object, by its fields.
     * @param {*} val
     * @returns {boolean}
     */
    '%host-object?': isHostObject,

    /**
     * An object's fields, as `#{...}` writes them: its own enumerable
     * properties, but a record's 'type' and 'typeDescriptor'.
     * @param {Object} obj
     * @returns {Cons|null} A list of (key . value), each key a string.
     */
    '%host-object-fields': (obj) => list(...Object.entries(obj)
        .filter(([key]) => key !== 'type' && key !== 'typeDescriptor')
        .map(([key, value]) => new Cons(key, value))),

    /**
     * A procedure's name: the one Scheme gave it, a primitive's or a class's,
     * or a JavaScript function's own.
     * @param {Function} proc
     * @returns {string|boolean} The name, or false where it has none.
     */
    '%procedure-name': (proc) => {
        const name = 'schemeName' in proc ? proc.schemeName : proc.name;
        return typeof name === 'string' && name !== '' && name !== 'anonymous' ? name : false;
    },

    /**
     * Whether a procedure is a continuation.
     * @param {Function} proc
     * @returns {boolean}
     */
    '%continuation?': isSchemeContinuation,

    /**
     * The values of several, as a list, or false for any other value.
     * @param {*} val
     * @returns {Cons|null|boolean}
     */
    '%values-list': (val) => val instanceof Values ? list(...val.values) : false,

    /**
     * The text of a value Scheme has no syntax for, as it gives it: a port's,
     * an error object's, or `undefined`.
     * @param {*} val
     * @returns {string}
     */
    '%host-text': (val) => String(val)
};
