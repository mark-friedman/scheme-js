/**
 * Identifier Utilities for Macro System
 * 
 * Shared utilities for handling identifiers (Symbols and SyntaxObjects)
 * across the macro expansion system.
 */

import { Symbol } from './symbol.js';
import { SyntaxObject, identifierEquals } from './syntax_object.js';

// =============================================================================
// Identifier Name Extraction
// =============================================================================

/**
 * Gets the name from an identifier, handling both Symbol and SyntaxObject.
 * @param {Symbol|SyntaxObject|*} id - The identifier to extract from
 * @returns {string|null} The identifier name, or null if not an identifier
 */
export function getIdentifierName(id) {
    if (id instanceof SyntaxObject) return id.name;
    if (id instanceof Symbol) return id.name;
    return null;
}

/**
 * Gets the identifier name from the car of a Cons cell.
 * Handles nested SyntaxObjects where name might itself be a Symbol.
 * @param {Cons} consCell - The cons cell to extract from
 * @returns {string|null} The identifier name, or null if car is not an identifier
 */
export function getCarName(consCell) {
    if (!consCell || !consCell.car) return null;
    const car = consCell.car;

    if (car instanceof Symbol) {
        return car.name;
    }
    if (car instanceof SyntaxObject) {
        // Handle case where SyntaxObject.name might be a Symbol
        return (car.name instanceof Symbol) ? car.name.name : car.name;
    }
    return null;
}

// =============================================================================
// Ellipsis Detection
// =============================================================================

/**
 * Checks if an identifier represents an ellipsis.
 * An identifier is an ellipsis if:
 * - Its name matches the ellipsis name (usually '...')
 * - It's not listed as a literal identifier
 * 
 * @param {Symbol|SyntaxObject|*} id - The identifier to check
 * @param {string} ellipsisName - The ellipsis identifier name (default '...')
 * @param {Array<Symbol|SyntaxObject>} literals - List of literal identifiers
 * @returns {boolean} True if the identifier is an ellipsis
 */
export function isEllipsisIdentifier(id, ellipsisName = '...', literals = []) {
    const name = getIdentifierName(id);
    if (name !== ellipsisName) return false;

    // Check if it's a literal (then not treated as ellipsis)
    return !literals.some(lit => identifierEquals(lit, id));
}

/**
 * Checks if the next element in a pattern/template is an ellipsis.
 * Used for lookahead during pattern matching and transcription.
 * 
 * @param {Cons} cons - The current cons cell (looks at cons.cdr.car)
 * @param {string} ellipsisName - The ellipsis identifier name
 * @param {Array<Symbol|SyntaxObject>} literals - List of literal identifiers (optional)
 * @returns {boolean} True if the next car is an ellipsis
 */
export function nextIsEllipsis(cons, ellipsisName = '...', literals = []) {
    if (!cons || !cons.cdr) return false;
    const cdr = cons.cdr;
    if (!cdr.car) return false;

    return isEllipsisIdentifier(cdr.car, ellipsisName, literals);
}
