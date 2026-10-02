/**
 * Utility functions for expression completeness detection and delimiter matching.
 * Used by the browser REPL for multiline expression support and paren matching.
 *
 * Both ask the reader rather than scan the text themselves, so they see
 * Scheme's lexical syntax as evaluating the text will: a parenthesis, a double
 * quote or a `#|` inside a string, a |symbol|, a character or a comment means
 * nothing there.
 * @module expression_utils
 */

import { parse, tokenize } from './reader/index.js';
import { SchemeReadError } from './errors.js';

/**
 * Checks if a string contains one or more complete S-expressions.
 * Returns true if the input is complete, false if it ends inside a datum and
 * needs more input: a list or vector not closed, a string, |symbol| or block
 * comment not ended, a quote, `#;` or `#\` with nothing after it.
 *
 * Input with an error more input cannot mend, such as an unbalanced `)`, is
 * complete, so that evaluating it reports the error.
 *
 * @param {string} input - Source code to check
 * @returns {boolean} True if input contains complete expression(s)
 */
export function isCompleteExpression(input) {
    if (!input || input.trim() === '') {
        return false;
    }
    try {
        parse(input, { suppressLog: true });
        return true;
    } catch (e) {
        return !(e instanceof SchemeReadError && e.incomplete);
    }
}

/**
 * The parentheses in the text that are delimiters, opening or closing a
 * list, a vector or a bytevector, in order. If the text ends inside a string,
 * a |symbol|, a character or a block comment, those before it.
 * @param {string} text - Source code
 * @returns {Array<{position: number, open: boolean}>} Each one's index in
 *   the text and whether it opens
 */
export function delimiterParens(text) {
    let tokens;
    try {
        tokens = tokenize(text);
    } catch (e) {
        if (!(e instanceof SchemeReadError && e.offset !== null)) throw e;
        // What is unfinished starts where a token could, so the text before
        // it reads as the same tokens
        tokens = tokenize(text.slice(0, e.offset));
    }
    const parens = [];
    for (const token of tokens) {
        if (token.value === '(' || token.value === '#(' || token.value === '#u8(') {
            // `#(` and `#u8(` end in the parenthesis they open
            parens.push({ position: token.offset + token.value.length - 1, open: true });
        } else if (token.value === ')') {
            parens.push({ position: token.offset, open: false });
        }
    }
    return parens;
}

/**
 * Finds the position of the matching delimiter for the one at the given position.
 *
 * A parenthesis in a string, a |symbol|, a character or a comment is no
 * delimiter, and has no match.
 *
 * @param {string} text - Source code
 * @param {number} position - Position of the delimiter to match
 * @returns {number|null} Position of matching delimiter, or null if not found
 */
export function findMatchingDelimiter(text, position) {
    if (position < 0 || position >= text.length) {
        return null;
    }
    const parens = delimiterParens(text);
    const start = parens.findIndex((paren) => paren.position === position);
    if (start < 0) {
        return null;
    }

    // Walk forward from an opening parenthesis or back from a closing one,
    // counting those that face the same way as one deeper, to the one that
    // brings the depth back to zero
    const { open } = parens[start];
    const step = open ? 1 : -1;
    let depth = 0;
    for (let i = start; i >= 0 && i < parens.length; i += step) {
        depth += parens[i].open === open ? 1 : -1;
        if (depth === 0) {
            return parens[i].position;
        }
    }
    return null;
}
