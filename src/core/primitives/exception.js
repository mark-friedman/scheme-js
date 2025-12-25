/**
 * Exception Primitives for Scheme.
 * 
 * Provides R7RS exception handling procedures:
 * - raise: Raise a non-continuable exception
 * - raise-continuable: Raise a continuable exception
 * - with-exception-handler: Install an exception handler
 * - error: Raise a SchemeError with message and irritants
 * - error-object?, error-object-message, error-object-irritants
 */

import { TailCall } from '../interpreter/values.js';
import { WithExceptionHandlerInit, RaiseNode, LiteralNode } from '../interpreter/ast.js';
import { SchemeError } from '../interpreter/errors.js';
import { list } from '../interpreter/cons.js';

/**
 * Returns exception primitives.
 * @param {Interpreter} interpreter - The interpreter instance
 * @returns {Object} Map of primitive names to functions
 */
export function getExceptionPrimitives(interpreter) {
    /**
     * raise: Raise a non-continuable exception.
     * If no handler is found, the exception becomes a JS error.
     */
    const raisePrimitive = (exception) => {
        return new TailCall(new RaiseNode(exception, false), null);
    };

    /**
     * raise-continuable: Raise a continuable exception.
     * The handler can return a value that becomes the result of raise-continuable.
     */
    const raiseContinuablePrimitive = (exception) => {
        return new TailCall(new RaiseNode(exception, true), null);
    };

    /**
     * with-exception-handler: Install an exception handler.
     * (with-exception-handler handler thunk)
     */
    const withExceptionHandlerPrimitive = (handler, thunk) => {
        return new TailCall(
            new WithExceptionHandlerInit(handler, thunk),
            null
        );
    };
    // Tell AppFrame not to wrap handler/thunk - we need raw Closures
    withExceptionHandlerPrimitive.skipBridge = true;

    /**
     * error: Create and raise a SchemeError.
     * (error message irritant ...)
     */
    const errorPrimitive = (message, ...irritants) => {
        const msg = typeof message === 'string' ? message : String(message);
        const err = new SchemeError(msg, irritants);
        return new TailCall(new RaiseNode(err, false), null);
    };

    return {
        'raise': raisePrimitive,
        'raise-continuable': raiseContinuablePrimitive,
        'with-exception-handler': withExceptionHandlerPrimitive,
        'error': errorPrimitive,

        /**
         * error-object?: Check if value is a SchemeError.
         */
        'error-object?': (obj) => obj instanceof SchemeError,

        /**
         * error-object-message: Get the error message.
         */
        'error-object-message': (obj) => {
            if (!(obj instanceof SchemeError)) {
                throw new SchemeError('error-object-message: expected error object', [obj]);
            }
            return obj.message;
        },

        /**
         * error-object-irritants: Get the error irritants as a list.
         */
        'error-object-irritants': (obj) => {
            if (!(obj instanceof SchemeError)) {
                throw new SchemeError('error-object-irritants: expected error object', [obj]);
            }
            return list(...obj.irritants);
        }
    };
}
