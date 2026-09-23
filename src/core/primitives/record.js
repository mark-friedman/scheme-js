/**
 * Record Primitives for Scheme.
 * 
 * Provides record type operations for R7RS define-record-type.
 */

import { toArray } from '../interpreter/cons.js';
import { assertString, assertList, assertSymbol } from '../interpreter/type_check.js';
import { SchemeError, SchemeTypeError, SchemeArityError } from '../interpreter/errors.js';
import { noteSchemeStore, storedToScheme } from '../interpreter/js_interop.js';
import { SCHEME_PRIMITIVE } from '../interpreter/values.js';

// ============================================================================
// Record type helpers
// ============================================================================

// The field names of a record type made by make-record-type, in field order.
const RECORD_FIELDS = Symbol('record-fields');

/**
 * Notes each constructor argument as a Scheme store into its field, so that
 * integer-valued flonums keep their exactness (see `noteSchemeStore` in
 * `js_interop.js`).
 * @param {Object} record - The record just constructed.
 * @param {string[]} fieldNames - The field each argument initialised.
 * @param {Array} args - The constructor's arguments.
 */
function noteConstructorStores(record, fieldNames, args) {
    for (let i = 0; i < args.length; i++) {
        noteSchemeStore(record, fieldNames[i], args[i]);
    }
}

/**
 * Checks a define-record-type constructor spec against the record's fields.
 * @param {Function} rtd - Record type descriptor.
 * @param {string[]} fieldNames - The record type's field names.
 * @param {Cons|null} tags - The constructor's field tags.
 * @returns {string[]} The tags' names, in argument order.
 */
function constructorFieldNames(rtd, fieldNames, tags) {
    assertList('record-constructor', 2, tags);
    const names = toArray(tags).map(tag => assertSymbol('record-constructor', 2, tag).name);
    const typeName = rtd.schemeName ?? rtd.name;
    names.forEach((name, i) => {
        if (!fieldNames.includes(name)) {
            throw new SchemeError(
                `define-record-type: constructor tag ${name} is not a field of ${typeName}`,
                [tags], 'define-record-type');
        }
        if (names.indexOf(name) !== i) {
            throw new SchemeError(
                `define-record-type: constructor tag ${name} appears more than once in ${typeName}`,
                [tags], 'define-record-type');
        }
    });
    return names;
}

// ============================================================================
// Record primitives
// ============================================================================

/**
 * Record primitives exported to Scheme.
 */
export const recordPrimitives = {
    /**
     * Creates a new record type.
     * @param {string} name - Name of the record type.
     * @param {Cons} fields - List of field symbols.
     * @returns {Function} The record type constructor class.
     */
    'make-record-type': (name, fields) => {
        // name may be a Symbol object from quoted symbol 'type
        const typeName = typeof name === 'string' ? name : name.name;
        const fieldNames = toArray(fields).map(s => s.name);

        // Sanitize name for use as JS class name (replace <> and other invalid chars)
        const jsClassName = typeName.replace(/[^a-zA-Z0-9_$]/g, '_');

        // Fields are set by name rather than written into generated source:
        // Scheme field names such as `type-test` or `ordered?` are not
        // JavaScript identifiers. A computed key names the class, so it shows
        // up under the record type's name in a debugger, without `new
        // Function` -- which a strict Content-Security-Policy forbids.
        const ClassConstructor = {
            [jsClassName]: class {
                constructor(...args) {
                    for (let i = 0; i < fieldNames.length; i++) {
                        this[fieldNames[i]] = args[i];
                    }
                }
            }
        }[jsClassName];
        Object.defineProperty(ClassConstructor, 'schemeName', { get: () => typeName });
        ClassConstructor[RECORD_FIELDS] = fieldNames;
        return ClassConstructor;
    },

    /**
     * Creates a constructor for a record type.
     *
     * The constructor takes exactly one argument per field tag, as
     * define-record-type passes them, and initialises the named fields; any
     * other field is left undefined, which R7RS leaves unspecified. Without
     * tags, a record type's constructor takes every field in field order.
     * A class built by make-class has no field list here: it maps its own
     * constructor parameters to fields, so its constructor passes its
     * arguments through unchanged, constructing the class with `new` as a
     * JavaScript caller would. define-class binds its constructor to the
     * class itself instead.
     *
     * @param {Function} rtd - Record type descriptor.
     * @param {Cons|null} [tags] - The fields the arguments initialise, in order.
     * @param {Symbol} [name] - The constructor's name, for error messages.
     * @returns {Function} Constructor function.
     */
    'record-constructor': (rtd, tags, name) => {
        const fieldNames = rtd[RECORD_FIELDS];
        let ctor;
        if (fieldNames === undefined) {
            if (tags !== undefined) {
                throw new SchemeTypeError('record-constructor', 1, 'record type', rtd);
            }
            ctor = function (...args) {
                return new rtd(...args);
            };
        } else {
            const tagNames = tags === undefined
                ? fieldNames
                : constructorFieldNames(rtd, fieldNames, tags);
            const arity = tagNames.length;
            const procName = name ? name.name : `${rtd.schemeName} constructor`;
            const inFieldOrder = arity === fieldNames.length &&
                tagNames.every((tag, i) => tag === fieldNames[i]);
            if (inFieldOrder) {
                ctor = function (...args) {
                    if (args.length !== arity) {
                        throw new SchemeArityError(procName, arity, arity, args.length);
                    }
                    const record = new rtd(...args);
                    noteConstructorStores(record, tagNames, args);
                    return record;
                };
            } else {
                ctor = function (...args) {
                    if (args.length !== arity) {
                        throw new SchemeArityError(procName, arity, arity, args.length);
                    }
                    // Constructing with no arguments first defines every field,
                    // in field order, so all records of the type share one shape.
                    const record = new rtd();
                    for (let i = 0; i < arity; i++) {
                        record[tagNames[i]] = args[i];
                    }
                    noteConstructorStores(record, tagNames, args);
                    return record;
                };
            }
        }
        ctor[SCHEME_PRIMITIVE] = true;
        // Ensure instanceof works in JS if rtd is a class/constructor
        if (rtd.prototype) {
            ctor.prototype = rtd.prototype;
        }
        return ctor;
    },

    /**
     * Creates a predicate for a record type.
     * @param {Function} rtd - Record type descriptor.
     * @returns {Function} Predicate function.
     */
    'record-predicate': (rtd) => {
        const pred = (obj) => obj instanceof rtd;
        pred[SCHEME_PRIMITIVE] = true;
        return pred;
    },

    /**
     * Creates an accessor for a record field.
     * @param {Function} rtd - Record type descriptor.
     * @param {Symbol} field - Field symbol.
     * @returns {Function} Accessor function.
     */
    'record-accessor': (rtd, field) => {
        const fieldName = field.name;
        const acc = (obj) => {
            if (!(obj instanceof rtd)) {
                throw new SchemeTypeError(`${fieldName} accessor`, 1, rtd.name, obj);
            }
            // An integer JavaScript wrote reads as exact; a flonum Scheme
            // stored reads as itself.
            return storedToScheme(obj, fieldName, obj[fieldName]);
        };
        acc[SCHEME_PRIMITIVE] = true;
        return acc;
    },

    /**
     * Creates a modifier for a record field.
     * @param {Function} rtd - Record type descriptor.
     * @param {Symbol} field - Field symbol.
     * @returns {Function} Modifier function.
     */
    'record-modifier': (rtd, field) => {
        const fieldName = field.name;
        const mod = (obj, val) => {
            if (!(obj instanceof rtd)) {
                throw new SchemeTypeError(`${fieldName} modifier`, 1, rtd.name, obj);
            }
            obj[fieldName] = val;
            noteSchemeStore(obj, fieldName, val);
        };
        mod[SCHEME_PRIMITIVE] = true;
        return mod;
    }
};
