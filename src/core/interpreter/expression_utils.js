/**
 * @fileoverview What a REPL asks of the text typed into it, answered by the
 * reader, `(scheme-js reader)` (src/core/scheme/reader.scm): whether it is
 * complete, and which parentheses delimit its lists and vectors. These are
 * the doors the REPLs call, converting the answers for JavaScript.
 */

import { systemLibrary } from './library_seed.js';
import { callSchemeProcedure } from './values.js';
import { toArray } from './cons.js';

/**
 * Calls a procedure of the reader.
 * @param {string} name - The procedure's name.
 * @param {...*} args - Its arguments.
 * @returns {*}
 */
function readerCall(name, ...args) {
    return callSchemeProcedure(systemLibrary(['scheme-js', 'reader']).get(name), args);
}

/**
 * Whether input is complete: it holds something, and reads, or fails to read
 * other than by ending inside a datum, as an error to report.
 * @param {string} input - The input.
 * @returns {boolean}
 */
export function isCompleteExpression(input) {
    return Boolean(input) && readerCall('complete-text?', input);
}

/**
 * The parentheses that delimit lists and vectors in a text, in order: the
 * `(` of `#(` and `#u8(` among them, those in strings, characters, |symbols|
 * and comments left out, and those before an unfinished token only.
 * @param {string} text - The text.
 * @returns {Array<{position: number, open: boolean}>}
 */
export function delimiterParens(text) {
    return toArray(readerCall('delimiter-parens', text))
        .map((paren) => ({ position: Number(paren.car), open: paren.cdr }));
}

/**
 * Where the parenthesis matching the one at a position is.
 * @param {string} text - The text.
 * @param {number} position - The position.
 * @returns {number|null} Its position, or null if none is there or none
 *   matches it.
 */
export function findMatchingDelimiter(text, position) {
    const found = readerCall('matching-delimiter', text, BigInt(position));
    return found === false ? null : Number(found);
}
