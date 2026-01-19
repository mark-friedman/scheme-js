/**
 * JavaScript Interop Primitives for Scheme.
 * 
 * Provides JavaScript evaluation and property access capabilities.
 */

import { assertString } from '../../core/interpreter/type_check.js';
import { SchemeTypeError, SchemeError } from '../../core/interpreter/errors.js';

/**
 * Interop primitives exported to Scheme.
 */
export const interopPrimitives = {
    /**
     * Evaluates a string as JavaScript code.
     * @param {string} code - JavaScript code to evaluate.
     * @returns {*} Result of evaluation.
     */
    'js-eval': (code) => {
        assertString('js-eval', 1, code);
        return (1, eval)(code);
    },

    /**
     * Accesses a property on a JavaScript object.
     * Used by the reader's dot notation: obj.prop -> (js-ref obj "prop")
     * @param {Object} obj - The object to access.
     * @param {string} prop - The property name.
     * @returns {*} The property value.
     */
    'js-ref': (obj, prop) => {
        assertString('js-ref', 2, prop);
        if (obj === null || obj === undefined) {
            throw new SchemeError(`js-ref: cannot access property "${prop}" on ${obj}`, [obj, prop], 'js-ref');
        }
        return obj[prop];
    },

    /**
     * Sets a property on a JavaScript object.
     * Used by set! with dot notation: (set! obj.prop val) -> (js-set! obj "prop" val)
     * @param {Object} obj - The object to modify.
     * @param {string} prop - The property name.
     * @param {*} value - The value to set.
     * @returns {undefined}
     */
    'js-set!': (obj, prop, value) => {
        assertString('js-set!', 2, prop);
        if (obj === null || obj === undefined) {
            throw new SchemeError(`js-set!: cannot set property "${prop}" on ${obj}`, [obj, prop], 'js-set!');
        }
        obj[prop] = value;
        return undefined;
    },

    /**
     * Invokes a method on a JavaScript object.
     * Used by expanded dot notation: obj.method(args...) -> (js-invoke obj "method" args...)
     * @param {Object} obj - The object.
     * @param {string} method - The method name.
     * @param {...*} args - Arguments to the method.
     * @returns {*} Result of the method call.
     */
    'js-invoke': (obj, method, ...args) => {
        assertString('js-invoke', 2, method);
        if (obj === null || obj === undefined) {
            throw new SchemeError(`js-invoke: cannot call method "${method}" on ${obj}`, [obj, method], 'js-invoke');
        }
        const func = obj[method];
        if (typeof func !== 'function') {
            throw new SchemeTypeError('js-invoke', 2, 'function', func);
        }
        return func.apply(obj, args);
    },

    /**
     * Creates a plain JavaScript object from key-value pairs.
     * Keys are converted to strings following JS semantics:
     * - Scheme Symbol: uses the symbol's name property
     * - String: used verbatim
     * - Number: converted to string
     * - Other: String() conversion
     * @param {...*} args - Alternating keys and values.
     * @returns {Object} The new JavaScript object.
     */
    'js-obj': (...args) => {
        const obj = {};
        for (let i = 0; i < args.length; i += 2) {
            const key = args[i];
            const val = args[i + 1];
            // Convert key to string:
            // - Scheme Symbol (has .name property) -> use the symbol name
            // - String -> use verbatim
            // - Other -> String() conversion
            let keyStr;
            if (key && typeof key === 'object' && typeof key.name === 'string') {
                // Scheme Symbol object
                keyStr = key.name;
            } else if (typeof key === 'string') {
                keyStr = key;
            } else {
                keyStr = String(key);
            }
            obj[keyStr] = val;
        }
        return obj;
    },

    /**
     * Merges multiple objects/js-obj results into one.
     * Used for spread syntax in #{...} literals.
     * @param {...Object} objects - Objects to merge (left to right, later overrides earlier).
     * @returns {Object} The merged JavaScript object.
     */
    'js-obj-merge': (...objects) => {
        const result = {};
        for (const obj of objects) {
            if (obj && typeof obj === 'object') {
                Object.assign(result, obj);
            } else if (obj !== null && obj !== undefined) {
                throw new SchemeTypeError('js-obj-merge', 0, 'object', obj);
            }
        }
        return result;
    },

    /**
     * Returns the type of a JavaScript value.
     * @param {*} val - The value to check.
     * @returns {string} The result of `typeof val`.
     */
    'js-typeof': (val) => {
        return typeof val;
    },

    /**
     * The JavaScript `undefined` value.
     */
    'js-undefined': undefined,

    /**
     * Checks if a value is JavaScript undefined or null.
     * @param {*} val - The value to check.
     * @returns {boolean} True if val is undefined or null (roughly equivalent to void).
     */
    'js-undefined?': (val) => {
        return val === undefined;
    },

    /**
     * The JavaScript `null` value.
     */
    'js-null': null,

    /**
     * Checks if a value is JavaScript null.
     * @param {*} val - The value to check.
     * @returns {boolean} True if val is null.
     */
    'js-null?': (val) => {
        return val === null;
    },

    /**
     * Creates a new instance of a JavaScript class using the `new` operator.
     * @param {Function} constructor - The constructor function or class.
     * @param {...*} args - Arguments to pass to the constructor.
     * @returns {Object} The new instance.
     */
    'js-new': (constructor, ...args) => {
        if (typeof constructor !== 'function') {
            throw new SchemeTypeError('js-new', 1, 'function', constructor);
        }
        return new constructor(...args);
    }
};
