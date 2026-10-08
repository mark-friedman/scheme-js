/**
 * @fileoverview `raise`, `raise-continuable` and `error`: what raises,
 * apart from the handlers (exception.js), which only the interpreter can
 * establish, so that compiled code running with no interpreter has them.
 *
 * Each returns a pending raise rather than raising itself (`pendingRaise` in
 * src/core/interpreter/ast_nodes.js): the interpreter performs it by looking
 * for a handler on its frame stack; compiled code continues it through its
 * raw entry, which throws, to the run beneath it or, where there is none, to
 * whoever called the program.
 */

import { pendingRaise, unhandled } from '../interpreter/ast_nodes.js';
import { SchemeError } from '../interpreter/errors.js';

/**
 * The primitives that raise.
 * @type {Object<string, Function>}
 */
export const raisePrimitives = {
    /**
     * raise: Raise a non-continuable exception.
     * If no handler is found, the exception becomes a JS error.
     */
    'raise': (exception) => pendingRaise(exception, false),

    /**
     * raise-continuable: Raise a continuable exception.
     * The handler can return a value that becomes the result of raise-continuable.
     */
    'raise-continuable': (exception) => pendingRaise(exception, true),

    /**
     * error: Create and raise a SchemeError.
     * (error message irritant ...)
     */
    'error': (message, ...irritants) => {
        const msg = typeof message === 'string' ? message : String(message);
        return pendingRaise(new SchemeError(msg, irritants), false);
    },

    /**
     * What a raise with no handler in force does where the handlers are
     * Scheme's ((scheme-js handlers), for a program compiled ahead of time):
     * throws to whoever called the program what a raise nobody handles throws
     * under the interpreter.
     * @param {*} exception - What was raised.
     * @returns {never}
     */
    '%raise-unhandled': (exception) => {
        throw unhandled(exception);
    }
};
