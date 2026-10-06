/**
 * @fileoverview The printer's door. How `write`, `display`, `write-shared`
 * and `write-simple` write a datum is Scheme, the printer of `(scheme core)`
 * (src/core/scheme/printer.scm); here are the text it gives JavaScript, and
 * what only JavaScript can say about a value the printer is given: whether it
 * is an object written by its fields and what they are, a procedure's name,
 * whether it is a continuation, several values, and a host value's own text.
 */

import { Cons, list } from '../../interpreter/cons.js';
import { Symbol, intern } from '../../interpreter/symbol.js';
import { Port, EOF_OBJECT } from './ports.js';
import { Rational } from '../rational.js';
import { Complex } from '../complex.js';
import { Char } from '../char_class.js';
import { SchemeString, stringValue } from '../string_class.js';
import { Flonum } from '../../interpreter/number_representation.js';
import { Values, isSchemeContinuation, callSchemeProcedure } from '../../interpreter/values.js';
import { systemLibrary } from '../../interpreter/library_seed.js';

// ============================================================================
// The text the printer gives JavaScript
// ============================================================================

/** `(scheme core)`'s exports, once found. @type {Map<string, Function>|null} */
let core = null;

/**
 * The text the printer writes a datum as.
 * @param {*} val - The datum.
 * @param {boolean} display - Whether it is displayed rather than written.
 * @param {string} labelling - 'cycles', 'shared' or 'none' (printer.scm).
 * @returns {string}
 */
function datumText(val, display, labelling) {
    if (core === null) core = systemLibrary(['scheme', 'core']);
    return stringValue(callSchemeProcedure(core.get('datum->string'), [val, display, intern(labelling)]));
}

/**
 * A value as `display` writes it.
 * @param {*} val
 * @returns {string}
 */
export function displayString(val) {
    return datumText(val, true, 'cycles');
}

/**
 * A value as `write` writes it.
 * @param {*} val
 * @returns {string}
 */
export function writeString(val) {
    return datumText(val, false, 'cycles');
}

/**
 * A value as `write-shared` writes it, with a datum label on every object
 * written more than once.
 * @param {*} val
 * @returns {string}
 */
export function writeStringShared(val) {
    return datumText(val, false, 'shared');
}

/**
 * A value as `write-simple` writes it, with no datum labels: it does not end
 * on circular structure.
 * @param {*} val
 * @returns {string}
 */
export function writeStringSimple(val) {
    return datumText(val, false, 'none');
}

/**
 * The text the REPLs show for a value: as `write` writes it, several values
 * one to a line.
 * @param {*} val
 * @returns {string}
 */
export function replText(val) {
    if (core === null) core = systemLibrary(['scheme', 'core']);
    return stringValue(callSchemeProcedure(core.get('repl-text'), [val]));
}

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
