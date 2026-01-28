/**
 * JavaScript Promise Interop Primitives for Scheme.
 * 
 * Provides transparent interoperability between Scheme and JavaScript Promises.
 * These are thin wrappers around the native Promise API, allowing Scheme
 * procedures to be used as callbacks.
 */

import { toArray } from '../../core/interpreter/cons.js';
import { Closure } from '../../core/interpreter/values.js';
import { assertProcedure, isList } from '../../core/interpreter/type_check.js';
import { SchemeTypeError } from '../../core/interpreter/errors.js';

/**
 * Creates a JavaScript-callable wrapper for a Scheme procedure.
 * Uses the interpreter's JS bridge mechanism to invoke closures.
 * 
 * @param {Closure|Function} proc - A Scheme closure or JS function
 * @param {Object} interpreter - The interpreter instance
 * @returns {Function} A JavaScript function that can be called by Promise.then()
 */
function wrapSchemeCallback(proc, interpreter) {
    // If it's a function (plain JS function, new-style Closure, or new-style Continuation)
    // we can use it directly.
    if (typeof proc === 'function') {
        return proc;
    }

    throw new SchemeTypeError('wrapSchemeCallback', 1, 'procedure', proc);
}

/**
 * Promise primitives that require interpreter access.
 * 
 * @param {Object} interpreter - The interpreter instance
 * @returns {Object} Promise primitive bindings
 */
export function getPromisePrimitives(interpreter) {
    return {
        /**
         * Tests if a value is a JavaScript Promise.
         * @param {*} obj - Value to test
         * @returns {boolean} True if obj is a Promise
         */
        'js-promise?': (obj) => {
            return obj instanceof Promise;
        },

        /**
         * Creates a resolved Promise with the given value.
         * @param {*} value - Value to resolve with
         * @returns {Promise} A resolved Promise
         */
        'js-promise-resolve': (value) => {
            return Promise.resolve(value);
        },

        /**
         * Creates a rejected Promise with the given reason.
         * @param {*} reason - Rejection reason
         * @returns {Promise} A rejected Promise
         */
        'js-promise-reject': (reason) => {
            return Promise.reject(reason);
        },

        /**
         * Creates a new Promise with a resolver procedure.
         * The procedure is called with two arguments: resolve and reject,
         * which are procedures that can be called to settle the promise.
         * 
         * Usage: (make-js-promise (lambda (resolve reject) ...))
         * 
         * @param {Function|Closure} executor - (resolve, reject) => void
         * @returns {Promise} A new Promise
         */
        'make-js-promise': (executor) => {
            assertProcedure('make-js-promise', 1, executor);

            const wrappedExecutor = wrapSchemeCallback(executor, interpreter);

            return new Promise((resolve, reject) => {
                // Call the Scheme executor with resolve and reject
                // These are already JS functions so they can be passed directly
                wrappedExecutor(resolve, reject);
            });
        },

        /**
         * Attaches fulfillment and/or rejection handlers to a Promise.
         * 
         * @param {Promise} promise - The promise to attach handlers to
         * @param {Function|Closure} onFulfilled - Success handler
         * @param {Function|Closure} [onRejected] - Optional rejection handler
         * @returns {Promise} A new Promise for chaining
         */
        'js-promise-then': (promise, onFulfilled, onRejected) => {
            if (!(promise instanceof Promise)) {
                throw new SchemeTypeError('js-promise-then', 1, 'Promise', promise);
            }
            assertProcedure('js-promise-then', 2, onFulfilled);

            const wrappedFulfilled = wrapSchemeCallback(onFulfilled, interpreter);

            if (onRejected !== undefined) {
                assertProcedure('js-promise-then', 3, onRejected);
                const wrappedRejected = wrapSchemeCallback(onRejected, interpreter);
                return promise.then(wrappedFulfilled, wrappedRejected);
            }

            return promise.then(wrappedFulfilled);
        },

        /**
         * Attaches a rejection handler to a Promise.
         * 
         * @param {Promise} promise - The promise to attach handler to
         * @param {Function|Closure} onRejected - Rejection handler
         * @returns {Promise} A new Promise for chaining
         */
        'js-promise-catch': (promise, onRejected) => {
            if (!(promise instanceof Promise)) {
                throw new SchemeTypeError('js-promise-catch', 1, 'Promise', promise);
            }
            assertProcedure('js-promise-catch', 2, onRejected);

            const wrappedRejected = wrapSchemeCallback(onRejected, interpreter);
            return promise.catch(wrappedRejected);
        },

        /**
         * Attaches a handler that runs regardless of fulfillment or rejection.
         * 
         * @param {Promise} promise - The promise to attach handler to
         * @param {Function|Closure} onFinally - Handler to run
         * @returns {Promise} A new Promise for chaining
         */
        'js-promise-finally': (promise, onFinally) => {
            if (!(promise instanceof Promise)) {
                throw new SchemeTypeError('js-promise-finally', 1, 'Promise', promise);
            }
            assertProcedure('js-promise-finally', 2, onFinally);

            const wrappedFinally = wrapSchemeCallback(onFinally, interpreter);
            return promise.finally(wrappedFinally);
        },

        /**
         * Waits for all promises in a list to resolve.
         * 
         * @param {Cons|Array} promises - List of promises
         * @returns {Promise<Array>} Promise that resolves to array of results
         */
        'js-promise-all': (promises) => {
            let arr;
            if (isList(promises)) {
                arr = toArray(promises);
            } else if (Array.isArray(promises)) {
                arr = promises;
            } else {
                throw new SchemeTypeError('js-promise-all', 1, 'list of promises', promises);
            }

            return Promise.all(arr);
        },

        /**
         * Waits for the first promise to settle (resolve or reject).
         * 
         * @param {Cons|Array} promises - List of promises  
         * @returns {Promise} Promise that settles with first result
         */
        'js-promise-race': (promises) => {
            let arr;
            if (isList(promises)) {
                arr = toArray(promises);
            } else if (Array.isArray(promises)) {
                arr = promises;
            } else {
                throw new SchemeTypeError('js-promise-race', 1, 'list of promises', promises);
            }

            return Promise.race(arr);
        },

        /**
         * Waits for all promises to settle (resolve or reject).
         * Unlike promise-all, does not short-circuit on rejection.
         * 
         * @param {Cons|Array} promises - List of promises
         * @returns {Promise<Array>} Promise that resolves to array of outcome objects
         */
        'js-promise-all-settled': (promises) => {
            let arr;
            if (isList(promises)) {
                arr = toArray(promises);
            } else if (Array.isArray(promises)) {
                arr = promises;
            } else {
                throw new SchemeTypeError('js-promise-all-settled', 1, 'list of promises', promises);
            }

            return Promise.allSettled(arr);
        }
    };
}
