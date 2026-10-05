/**
 * @fileoverview What the reader, `(scheme-js reader)` (src/core/scheme/reader.scm),
 * needs of the representations: its errors, the strings it reads, and the note
 * that a datum label has been referred to.
 */

import { SchemeReadError } from '../interpreter/errors.js';
import { pendingRaise } from '../interpreter/ast_nodes.js';
import { noteLabelReference } from '../interpreter/syntax_object.js';
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
 *
 * Besides what it needs of the representations, three scans of a whole text,
 * which in Scheme a character at a time would cost the reader most of its
 * time: the reader skips whitespace and comments, and finds where an atom
 * ends, with the first two, and works out a datum's line and column from where
 * it is with the third, rather than counting lines as it goes.
 */
export const readerPrimitives = {
    /**
     * Where the first of some characters is in a text, from a place.
     * @param {string} text - The text.
     * @param {string} chars - The characters.
     * @param {bigint} start - Where to begin.
     * @returns {bigint} Its index, or the text's length if there is none.
     */
    '%string-find-any': (text, chars, start) => {
        const s = stringValue(text);
        const set = stringValue(chars);
        let i = Number(start);
        while (i < s.length && !set.includes(s[i])) i++;
        return i;
    },

    /**
     * Where the first character not among some is in a text, from a place.
     * @param {string} text - The text.
     * @param {string} chars - The characters.
     * @param {bigint} start - Where to begin.
     * @returns {bigint} Its index, or the text's length if there is none.
     */
    '%string-skip-any': (text, chars, start) => {
        const s = stringValue(text);
        const set = stringValue(chars);
        let i = Number(start);
        while (i < s.length && set.includes(s[i])) i++;
        return i;
    },

    /**
     * Where each line of a text begins: 0, and the index after each line
     * ending, CR LF being one.
     * @param {string} text - The text.
     * @returns {Array<bigint>} The indices, as a vector.
     */
    '%line-starts': (text) => {
        const s = stringValue(text);
        const starts = [0];
        for (let i = 0; i < s.length; i++) {
            const c = s.charCodeAt(i);
            if (c === 10) starts.push(i + 1);
            else if (c === 13 && s.charCodeAt(i + 1) !== 10) starts.push(i + 1);
        }
        return starts;
    },

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
