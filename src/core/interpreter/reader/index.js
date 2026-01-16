/**
 * @fileoverview Reader module barrel export.
 * Provides the main parse() entry point and re-exports submodules.
 */

import { tokenize, stripBlockComments } from './tokenizer.js';
import { readFromTokens } from './parser.js';
import { fixup } from './datum_labels.js';

// Re-export for consumers who need specific functions
export { tokenize, stripBlockComments } from './tokenizer.js';
export { readFromTokens, readList, readVector, readAtom, readBytevector, readJSObjectLiteral } from './parser.js';
export { parseNumber, parsePrefixedNumber } from './number_parser.js';
export { handleDotAccess, buildPropertyAccessForm } from './dot_access.js';
export { processStringEscapes, processSymbolEscapes } from './string_utils.js';
export { readCharacter, NAMED_CHARACTERS } from './character.js';
export { Placeholder, fixup } from './datum_labels.js';

/**
 * Parses a string of Scheme code into a list of S-expressions.
 * @param {string} input - Source code to parse
 * @param {Object} [options] - Parsing options
 * @param {boolean} [options.caseFold=false] - If true, fold symbol names to lowercase (for include-ci)
 * @returns {Array} Array of S-expressions (Cons, Symbol, number, etc.)
 */
export function parse(input, options = {}) {
    // State object for parsing context
    // Use provided state or create new one
    const state = options.state || {
        caseFold: options.caseFold || false,
        labels: new Map()
    };

    // Strip block comments before tokenizing
    const preprocessed = stripBlockComments(input);
    const tokens = tokenize(preprocessed);
    const expressions = [];

    try {
        while (tokens.length > 0) {
            const expr = readFromTokens(tokens, state);
            // If the expression is a Placeholder, it means top-level #n# (unlikely but possible)
            // or #n=... which returns the value. 
            // We need to run fixup on the result to resolve internal cycles.
            expressions.push(fixup(expr));
        }
    } catch (e) {
        if (!options.suppressLog) {
            console.error(`Parse error in input: "${input.substring(0, 100)}${input.length > 100 ? '...' : ''}"`);
        }
        throw e;
    }
    return expressions;
}
