/**
 * Exception Primitives for Scheme.
 *
 * Provides R7RS exception handling procedures:
 * - with-exception-handler: Install an exception handler
 * - raise, raise-continuable and error, which are in raise.js, since compiled
 *   code needs them without the interpreter
 *
 * What error objects are -- error-object? and the rest -- is in
 * error_object.js, which compiled code needs without the interpreter too.
 */

import { TailCall } from '../interpreter/values.js';
import { WithExceptionHandlerInit } from '../interpreter/ast.js';
import { pendingRaise } from '../interpreter/ast_nodes.js';
import { SchemeSyntaxError } from '../interpreter/errors.js';
import { unwrapSyntax } from '../interpreter/syntax_object.js';
import { raisePrimitives } from './raise.js';

/**
 * Returns exception primitives.
 * @param {Interpreter} interpreter - The interpreter instance
 * @returns {Object} Map of primitive names to functions
 */
export function getExceptionPrimitives(interpreter) {
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

    return {
        ...raisePrimitives,
        'with-exception-handler': withExceptionHandlerPrimitive,

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
