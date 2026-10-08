/**
 * @fileoverview The printer's door. How `write`, `display`, `write-shared`
 * and `write-simple` write a datum is Scheme, the printer of `(scheme core)`
 * (src/core/scheme/printer.scm); here is the text it gives JavaScript. What
 * only JavaScript can say about a value the printer is given is in
 * printer_primitives.js.
 */

import { intern } from '../../interpreter/symbol.js';
import { stringValue } from '../string_class.js';
import { callSchemeProcedure } from '../../interpreter/values.js';
import { systemLibrary } from '../../interpreter/library_seed.js';

// ============================================================================
// The text the printer gives JavaScript
// ============================================================================

/** `(scheme core)`'s exports, once found. @type {Map<string, Function>|null} */
let core = null;

/**
 * The text the printer writes a datum as.
 * @param {*} val - The datum.
 * @param {boolean} display - Whether it is displayed rather than written.
 * @param {string} labelling - 'cycles', 'shared' or 'none' (printer.scm).
 * @returns {string}
 */
function datumText(val, display, labelling) {
    if (core === null) core = systemLibrary(['scheme', 'core']);
    return stringValue(callSchemeProcedure(core.get('datum->string'), [val, display, intern(labelling)]));
}

/**
 * A value as `display` writes it.
 * @param {*} val
 * @returns {string}
 */
export function displayString(val) {
    return datumText(val, true, 'cycles');
}

/**
 * A value as `write` writes it.
 * @param {*} val
 * @returns {string}
 */
export function writeString(val) {
    return datumText(val, false, 'cycles');
}

/**
 * A value as `write-shared` writes it, with a datum label on every object
 * written more than once.
 * @param {*} val
 * @returns {string}
 */
export function writeStringShared(val) {
    return datumText(val, false, 'shared');
}

/**
 * A value as `write-simple` writes it, with no datum labels: it does not end
 * on circular structure.
 * @param {*} val
 * @returns {string}
 */
export function writeStringSimple(val) {
    return datumText(val, false, 'none');
}

/**
 * The text the REPLs show for a value: as `write` writes it, several values
 * one to a line.
 * @param {*} val
 * @returns {string}
 */
export function replText(val) {
    if (core === null) core = systemLibrary(['scheme', 'core']);
    return stringValue(callSchemeProcedure(core.get('repl-text'), [val]));
}

// What the printer asks of JavaScript is printer_primitives.js, which compiled
// code needs without the library system this module's doors start.
export { printerPrimitives } from './printer_primitives.js';
