/**
 * @fileoverview What the reader, `(scheme-js reader)` (src/core/scheme/reader.scm),
 * needs of the representations: its errors, the strings it reads, and the note
 * that a datum label has been referred to.
 */

import { SchemeReadError } from '../interpreter/errors.js';
import { pendingRaise } from '../interpreter/ast_nodes.js';
import { noteLabelReference } from '../interpreter/reader/datum_labels.js';
import { stringValue } from './string_class.js';

/**
 * A number from Scheme as JavaScript's, or null for #f.
 * @param {*} value - An exact or inexact number, or #f.
 * @returns {number|null}
 */
function numberOrNull(value) {
    return value === false ? null : Number(value);
}

/**
 * The reader's primitives.
 */
export const readerPrimitives = {
    /**
     * Raises a read error.
     * @param {string} message - What is wrong.
     * @param {string|boolean} context - What was being read, or #f.
     * @param {boolean} incomplete - Whether the text ended inside a datum,
     *   which more text could complete (`SchemeReadError.endOfInput`).
     * @param {number|boolean} line - Where, or #f.
     * @param {number|boolean} column - Where, or #f.
     * @param {number|boolean} offset - Where in the text the unfinished token
     *   began, or #f.
     * @returns {*} The raise.
     */
    '%read-error': (message, context, incomplete, line, column, offset) => {
        const what = context === false ? null : stringValue(context);
        const error = incomplete
            ? SchemeReadError.endOfInput(stringValue(message), what, numberOrNull(line), numberOrNull(column),
                numberOrNull(offset))
            : new SchemeReadError(stringValue(message), what, numberOrNull(line), numberOrNull(column));
        return pendingRaise(error, false);
    },

    /**
     * A string as a literal is: the characters it holds, as a string that
     * cannot be changed.
     * @param {string} str - The string.
     * @returns {string}
     */
    '%literal-string': (str) => stringValue(str),

    /**
     * Notes that a datum label has been referred to, after which the data
     * the evaluator copies may share structure and be circular.
     */
    '%note-label-reference!': () => {
        noteLabelReference();
    }
};
