/**
 * Exception Primitives for Scheme.
 * 
 * Provides R7RS exception handling procedures:
 * - raise: Raise a non-continuable exception
 * - raise-continuable: Raise a continuable exception
 * - with-exception-handler: Install an exception handler
 * - error: Raise a SchemeError with message and irritants
 *
 * What error objects are -- error-object? and the rest -- is in
 * error_object.js, which compiled code needs without the interpreter.
 */

import { TailCall } from '../interpreter/values.js';
import { WithExceptionHandlerInit } from '../interpreter/ast.js';
import { pendingRaise } from '../interpreter/ast_nodes.js';
import { SchemeError, SchemeSyntaxError } from '../interpreter/errors.js';
import { unwrapSyntax } from '../interpreter/syntax_object.js';

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
    const raisePrimitive = (exception) => pendingRaise(exception, false);

    /**
     * raise-continuable: Raise a continuable exception.
     * The handler can return a value that becomes the result of raise-continuable.
     */
    const raiseContinuablePrimitive = (exception) => pendingRaise(exception, true);

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
        return pendingRaise(new SchemeError(msg, irritants), false);
    };

    return {
        'raise': raisePrimitive,
        'raise-continuable': raiseContinuablePrimitive,
        'with-exception-handler': withExceptionHandlerPrimitive,
        'error': errorPrimitive,

        /**
         * %raise-syntax-error: Raises a syntax error with a message and
         * irritants, as `syntax-error` does when it is expanded (macros.scm).
         * A syntax error reaches whoever analyzed the form as it was raised,
         * where what `error` raises in a transformer is reported as the
         * transformer's failure; and its irritants are data, though a macro's
         * template gives them as syntax.
         */
        '%raise-syntax-error': (message, ...irritants) => {
            const error = new SchemeSyntaxError(typeof message === 'string' ? message : String(message));
            error.irritants = irritants.map(unwrapSyntax);
            return pendingRaise(error, false);
        },
    };
}
