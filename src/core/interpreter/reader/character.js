/**
 * @fileoverview Character literal parsing for Scheme reader.
 */

import { SchemeReadError } from '../errors.js';
import { Char } from '../../primitives/char_class.js';

/**
 * Named character constants per R7RS §6.6.
 */
export const NAMED_CHARACTERS = {
    'alarm': '\x07',
    'backspace': '\x08',
    'delete': '\x7F',
    'escape': '\x1B',
    'newline': '\n',
    'null': '\x00',
    'return': '\r',
    'space': ' ',
    'tab': '\t'
};

/**
 * Parses a character literal (after the #\ prefix).
 * @param {string} name - The character name or literal
 * @returns {Char} Scheme character object
 */
export function readCharacter(name) {
    // Hex escape: #\x41 -> 'A'
    if (name.startsWith('x') && name.length > 1) {
        const codePoint = parseInt(name.slice(1), 16);
        if (isNaN(codePoint)) {
            throw new SchemeReadError(`invalid character hex escape: #\\${name}`, 'character');
        }
        return new Char(codePoint);
    }

    // Named character: #\newline -> '\n'
    const lower = name.toLowerCase();
    if (NAMED_CHARACTERS.hasOwnProperty(lower)) {
        return new Char(NAMED_CHARACTERS[lower].codePointAt(0));
    }

    // Single character: #\a -> 'a'
    if (name.length === 1) {
        return new Char(name.codePointAt(0));
    }

    throw new SchemeReadError(`unknown character name: #\\${name}`, 'character');
}
