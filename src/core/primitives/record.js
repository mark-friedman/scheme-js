/**
 * Record Primitives for Scheme.
 * 
 * Provides record type operations for R7RS define-record-type.
 */

import { isString, stringValue, SchemeString } from './string_class.js';
import { toArray, Cons, list } from '../interpreter/cons.js';
import { intern } from '../interpreter/symbol.js';
import { assertString, assertList, assertSymbol } from '../interpreter/type_check.js';
import { SchemeError, SchemeTypeError, SchemeArityError } from '../interpreter/errors.js';
import { noteSchemeStore, storedToScheme } from '../interpreter/js_interop.js';
import { Flonum } from '../interpreter/number_representation.js';
import { SCHEME_PRIMITIVE } from '../interpreter/values.js';

// ============================================================================
// Record type helpers
// ============================================================================

// The field names of a record type made by make-record-type, in field order.
export const RECORD_FIELDS = Symbol('record-fields');

// What a record's procedures carry, for compiled code that makes, tests,
// reads or writes the record itself where it calls one ("Records" in
// emit.scm): an accessor its field's name, a modifier its field's name under
// another key, a predicate `true`, a constructor a key naming its type's
// fields and the fields it takes (`constructorKey`); and each its record type.
export const RECORD_READS = Symbol('record-reads');
export const RECORD_WRITES = Symbol('record-writes');
export const RECORD_TESTS = Symbol('record-tests');
export const RECORD_MAKES = Symbol('record-makes');
export const RECORD_TYPE = Symbol('record-type');

/**
 * Notes each constructor argument as a Scheme store into its field, so that
 * integer-valued flonums keep their exactness (see `noteSchemeStore` in
 * `js_interop.js`). A record just made has no notes, so only an inexact
 * number needs one: an exact integer's store would clear a note there is not.
 * @param {Object} record - The record just constructed.
 * @param {string[]} fieldNames - The field each argument initialised.
 * @param {Array} args - The constructor's arguments.
 */
function noteConstructorStores(record, fieldNames, args) {
    for (let i = 0; i < args.length; i++) {
        if (args[i] instanceof Flonum) noteSchemeStore(record, fieldNames[i], args[i]);
    }
}

/**
 * The key a record constructor carries for compiled code, naming its record
 * type's fields, in field order, and the fields its arguments initialise, in
 * argument order: two constructors with one key make the same record from the
 * same arguments, but for its type. JSON, so that no two different pairs of
 * lists, whatever their names, have one key.
 * @param {string[]} fieldNames - The record type's fields.
 * @param {string[]} tagNames - The constructor's.
 * @returns {string}
 */
function constructorKey(fieldNames, tagNames) {
    return JSON.stringify([fieldNames, tagNames]);
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
        const typeName = isString(name) ? stringValue(name) : name.name;
        const fieldNames = toArray(fields).map(s => s.name);

        // Sanitize name for use as JS class name (replace <> and other invalid chars)
        const jsClassName = typeName.replace(/[^a-zA-Z0-9_$]/g, '_');

        // Fields are set by name rather than written into generated source:
        // Scheme field names such as `type-test` or `ordered?` are not
        // JavaScript identifiers. A computed key names the class, so it shows
        // up under the record type's name in a debugger, without `new
        // Function` -- which a strict Content-Security-Policy forbids.
        //
        // It sets as many fields as it is given arguments, in field order,
        // and with none sets none: compiled code makes a record so and sets
        // every field itself, by name, a site of its own (`record-make`
        // in emit.scm), where this loop, one site for every record type, cost
        // a record of four fields 37 ns more. Every constructor sets every
        // field, in field order, so that all records of a type share one
        // shape.
        const ClassConstructor = {
            [jsClassName]: class {
                constructor(...args) {
                    const count = Math.min(args.length, fieldNames.length);
                    for (let i = 0; i < count; i++) {
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
                // Each field's argument, in field order, or -1 for a field
                // the constructor leaves out, which is set undefined: every
                // field set, in field order, so all records of the type share
                // one shape.
                const argumentOf = fieldNames.map(field => tagNames.indexOf(field));
                ctor = function (...args) {
                    if (args.length !== arity) {
                        throw new SchemeArityError(procName, arity, arity, args.length);
                    }
                    const record = new rtd();
                    for (let i = 0; i < fieldNames.length; i++) {
                        record[fieldNames[i]] = argumentOf[i] < 0 ? undefined : args[argumentOf[i]];
                    }
                    noteConstructorStores(record, tagNames, args);
                    return record;
                };
            }
            ctor[RECORD_MAKES] = constructorKey(fieldNames, tagNames);
            ctor[RECORD_TYPE] = rtd;
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
        pred[RECORD_TESTS] = true;
        pred[RECORD_TYPE] = rtd;
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
            // stored reads as itself. Anything but a number reads as it is.
            const value = obj[fieldName];
            return typeof value === 'number' ? storedToScheme(obj, fieldName, value) : value;
        };
        acc[SCHEME_PRIMITIVE] = true;
        acc[RECORD_READS] = fieldName;
        acc[RECORD_TYPE] = rtd;
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
        mod[RECORD_WRITES] = fieldName;
        mod[RECORD_TYPE] = rtd;
        return mod;
    },

    /**
     * What a procedure does with a record, for the compiler: an accessor's
     * field as `(accessor . field)`, a modifier's as `(modifier . field)`, a
     * predicate as `(predicate)`, and a constructor as `(constructor key
     * fields tags)` -- the key it carries, its type's fields and the fields
     * its arguments initialise; #f for anything else.
     * @param {*} proc - The value.
     * @returns {Cons|boolean}
     */
    '%record-procedure-kind': (proc) => {
        if (typeof proc !== 'function') return false;
        if (proc[RECORD_READS] !== undefined) return new Cons(intern('accessor'), intern(proc[RECORD_READS]));
        if (proc[RECORD_WRITES] !== undefined) return new Cons(intern('modifier'), intern(proc[RECORD_WRITES]));
        if (proc[RECORD_TESTS] === true) return new Cons(intern('predicate'), null);
        if (proc[RECORD_MAKES] !== undefined) {
            const [fieldNames, tagNames] = JSON.parse(proc[RECORD_MAKES]);
            const symbols = (names) => list(...names.map(name => intern(name)));
            return list(intern('constructor'), new SchemeString(proc[RECORD_MAKES]),
                symbols(fieldNames), symbols(tagNames));
        }
        return false;
    }
};
