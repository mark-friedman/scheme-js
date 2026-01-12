/**
 * JavaScript Interop Primitives for Scheme.
 * 
 * Provides JavaScript evaluation and property access capabilities.
 */

import { assertString } from '../../core/interpreter/type_check.js';

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
            throw new Error(`js-ref: cannot access property "${prop}" on ${obj}`);
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
            throw new Error(`js-set!: cannot set property "${prop}" on ${obj}`);
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
            throw new Error(`js-invoke: cannot call method "${method}" on ${obj}`);
        }
        const func = obj[method];
        if (typeof func !== 'function') {
            throw new Error(`js-invoke: property "${method}" is not a function on ${obj}`);
        }
        return func.apply(obj, args);
    },

    /**
     * Creates a plain JavaScript object from key-value pairs.
     * @param {...*} args - Alternating keys and values.
     * @returns {Object} The new JavaScript object.
     */
    'js-obj': (...args) => {
        const obj = {};
        for (let i = 0; i < args.length; i += 2) {
            const key = args[i];
            const val = args[i + 1];
            const keyStr = (typeof key === 'string') ? key : (key && typeof key.name === 'string') ? key.name : String(key);
            obj[keyStr] = val;
        }
        return obj;
    }
};
