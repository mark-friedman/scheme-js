/**
 * Record Primitives for Scheme.
 * 
 * Provides record type operations for R7RS define-record-type.
 */

import { toArray } from '../interpreter/cons.js';
import { assertString, assertList, assertSymbol } from '../interpreter/type_check.js';
import { SchemeTypeError } from '../interpreter/errors.js';

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

        // Dynamically create a named class
        const classSrc = `
            return class ${jsClassName} {
                static get schemeName() { return ${JSON.stringify(typeName)}; }
                constructor(${fieldNames.join(', ')}) {
                    ${fieldNames.map(f => `this.${f} = ${f};`).join('\n')}
                }
            }
        `;

        const ClassConstructor = new Function(classSrc)();
        return ClassConstructor;
    },

    /**
     * Creates a constructor for a record type.
     * @param {Function} rtd - Record type descriptor.
     * @returns {Function} Constructor function.
     */
    'record-constructor': (rtd) => {
        return (...args) => new rtd(...args);
    },

    /**
     * Creates a predicate for a record type.
     * @param {Function} rtd - Record type descriptor.
     * @returns {Function} Predicate function.
     */
    'record-predicate': (rtd) => {
        return (obj) => obj instanceof rtd;
    },

    /**
     * Creates an accessor for a record field.
     * @param {Function} rtd - Record type descriptor.
     * @param {Symbol} field - Field symbol.
     * @returns {Function} Accessor function.
     */
    'record-accessor': (rtd, field) => {
        const fieldName = field.name;
        return (obj) => {
            if (!(obj instanceof rtd)) {
                throw new SchemeTypeError(`${fieldName} accessor`, 1, rtd.name, obj);
            }
            return obj[fieldName];
        };
    },

    /**
     * Creates a modifier for a record field.
     * @param {Function} rtd - Record type descriptor.
     * @param {Symbol} field - Field symbol.
     * @returns {Function} Modifier function.
     */
    'record-modifier': (rtd, field) => {
        const fieldName = field.name;
        return (obj, val) => {
            if (!(obj instanceof rtd)) {
                throw new SchemeTypeError(`${fieldName} modifier`, 1, rtd.name, obj);
            }
            obj[fieldName] = val;
        };
    }
};
