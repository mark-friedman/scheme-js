/**
 * @fileoverview The handlers in force, as `with-exception-handler` and the
 * raises written in Scheme read and set them ((scheme-js handlers),
 * src/core/scheme/handlers.scm): the list the runtime keeps for a program
 * compiled ahead of time, and the procedure a driver with no interpreter
 * hands an error JavaScript threw to (`handlerList` and `errorRaiser` in
 * src/core/interpreter/unwind.js).
 */

import { handlerList, errorRaiser } from '../interpreter/unwind.js';
import { Cons } from '../interpreter/cons.js';
import { SchemeTypeError } from '../interpreter/errors.js';

/**
 * The handlers primitives.
 * @type {Object<string, Function>}
 */
export const handlerPrimitives = {
    /**
     * The handlers in force: a list of `(handler . winds)`, innermost first.
     * @returns {Cons|null}
     */
    '%handlers': () => handlerList.v,

    /**
     * Makes a list of handlers the ones in force.
     * @param {Cons|null} handlers - The list.
     * @returns {undefined}
     */
    '%set-handlers!': (handlers) => {
        if (handlers !== null && !(handlers instanceof Cons)) {
            throw new SchemeTypeError('%set-handlers!', 1, 'list', handlers);
        }
        handlerList.v = handlers;
        return undefined;
    },

    /**
     * Makes a procedure the one a driver with no interpreter hands an error
     * JavaScript threw to while a handler is in force.
     * @param {Function} raise - The procedure.
     * @returns {undefined}
     */
    '%set-error-raiser!': (raise) => {
        if (typeof raise !== 'function') {
            throw new SchemeTypeError('%set-error-raiser!', 1, 'procedure', raise);
        }
        errorRaiser.v = raise;
        return undefined;
    }
};
