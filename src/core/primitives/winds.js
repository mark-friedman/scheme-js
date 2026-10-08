/**
 * @fileoverview The winds in force, as `dynamic-wind` written in Scheme reads
 * and sets them ((scheme-js winds), src/core/scheme/winds.scm): the list the
 * runtime keeps for a program compiled ahead of time, which each continuation
 * records and invoking one goes back to (`windList` and `travelTo` in
 * src/core/interpreter/unwind.js).
 */

import { windList } from '../interpreter/unwind.js';
import { Cons } from '../interpreter/cons.js';
import { SchemeTypeError } from '../interpreter/errors.js';

/**
 * The winds primitives.
 * @type {Object<string, Function>}
 */
export const windPrimitives = {
    /**
     * The winds in force: a list of `(before . after)`, innermost first.
     * @returns {Cons|null}
     */
    '%winds': () => windList.v,

    /**
     * Makes a list of winds the ones in force.
     * @param {Cons|null} winds - The list.
     * @returns {undefined}
     */
    '%set-winds!': (winds) => {
        if (winds !== null && !(winds instanceof Cons)) {
            throw new SchemeTypeError('%set-winds!', 1, 'list', winds);
        }
        windList.v = winds;
        return undefined;
    }
};
