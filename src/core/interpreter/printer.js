/**
 * @fileoverview The REPLs' door into the printer: the text a REPL shows for
 * the value of an expression is `repl-text`'s, in `(scheme core)`
 * (src/core/scheme/printer.scm).
 */

import { replText } from '../primitives/io/printer.js';

/**
 * The text a REPL shows for a value: as `write` writes it, several values one
 * to a line.
 * @param {*} val - The value from the interpreter.
 * @returns {string}
 */
export function prettyPrint(val) {
    return replText(val);
}
