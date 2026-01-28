/**
 * @fileoverview Core parser for Scheme S-expressions.
 * Handles lists, vectors, atoms, quotes, and special syntaxes.
 */

import { Cons, cons, list } from '../cons.js';
import { Symbol, intern } from '../symbol.js';
import { SchemeReadError } from '../errors.js';

import { handleDotAccess, buildPropertyAccessForm } from './dot_access.js';
import { processSymbolEscapes, processStringEscapes } from './string_utils.js';
import { parseNumber } from './number_parser.js';
import { readCharacter } from './character.js';
import { Placeholder } from './datum_labels.js';

/**
 * Main parser dispatch. Reads one S-expression from tokens.
 * @param {Array} tokens - Token array
 * @param {Object} state - Parser state (caseFold, labels)
 * @returns {*} Parsed S-expression
 */
export function readFromTokens(tokens, state) {
    if (tokens.length === 0) {
        throw new SchemeReadError('unexpected end of input', state.current || 'expression');
    }

    const tokenObj = tokens.shift();
    const token = tokenObj.value;

    // Handle fold-case directives
    if (token === '#!fold-case') {
        state.caseFold = true;
        if (tokens.length === 0) return undefined;
        return readFromTokens(tokens, state); // Continue to next datum
    }
    if (token === '#!no-fold-case') {
        state.caseFold = false;
        if (tokens.length === 0) return undefined;
        return readFromTokens(tokens, state); // Continue to next datum
    }

    // Handle datum comments: #; skips the next datum
    if (token === '#;') {
        if (tokens.length === 0) {
            throw new SchemeReadError('unexpected end of input after #;', 'datum comment');
        }
        readFromTokens(tokens, state); // Read and discard next datum
        if (tokens.length === 0) return undefined;
        return readFromTokens(tokens, state); // Return the datum after that
    }

    // Handle fused tokens like #1=100
    if (token.includes('=') && /^#\d+=/.test(token)) {
        const match = token.match(/^(#\d+=)(.*)/);
        if (match) {
            const labelToken = match[1];
            let remainder = match[2];

            const id = parseInt(labelToken.slice(1, -1), 10);

            // Create placeholder and register it
            const placeholder = new Placeholder(id);
            state.labels.set(id, placeholder);

            let val;
            if (remainder) {
                // Check if remainder was split from a delimiter (e.g. #0=#( -> remainder=#, next=( )
                if (remainder === '#' && tokens.length > 0 && tokens[0].value === '(') {
                    remainder = '#(';
                    tokens.shift();
                } else if (remainder === '#u8' && tokens.length > 0 && tokens[0].value === '(') {
                    remainder = '#u8(';
                    tokens.shift();
                }

                tokens.unshift({ value: remainder, hasPrecedingSpace: false });
                val = readFromTokens(tokens, state);
            } else {
                // No remainder, read next token
                val = readFromTokens(tokens, state);
            }

            placeholder.value = val;
            placeholder.resolved = true;
            return handleDotAccess(val, tokens);
        }
    }

    // Handle #n= standalone token
    if (/^#\d+=$/.test(token)) {
        const id = parseInt(token.slice(1, -1), 10);
        const placeholder = new Placeholder(id);
        state.labels.set(id, placeholder);

        const val = readFromTokens(tokens, state);
        placeholder.value = val;
        placeholder.resolved = true;
        return handleDotAccess(val, tokens);
    }

    // Handle #n# reference
    if (/^#\d+#$/.test(token)) {
        const id = parseInt(token.slice(1, -1), 10);
        const placeholder = state.labels.get(id);
        if (!placeholder) {
            throw new SchemeReadError(`reference to undefined label #${id}#`, 'datum label');
        }
        return handleDotAccess(placeholder, tokens);
    }

    let result;
    if (token === '(') {
        result = readList(tokens, state);
    } else if (token === '#(') {
        result = readVector(tokens, state);
    } else if (token === '#u8(') {
        result = readBytevector(tokens);
    } else if (token === '#{') {
        result = readJSObjectLiteral(tokens, state);
    } else if (token === ')') {
        throw new SchemeReadError("unexpected ')' - unbalanced parentheses", 'list');
    } else if (token === '}') {
        throw new SchemeReadError("unexpected '}' - unbalanced braces", 'object literal');
    } else if (token === "'") {
        // Quotes
        result = list(intern('quote'), readFromTokens(tokens, state));
    } else if (token === '`') {
        result = list(intern('quasiquote'), readFromTokens(tokens, state));
    } else if (token === ',') {
        result = list(intern('unquote'), readFromTokens(tokens, state));
    } else if (token === ',@') {
        result = list(intern('unquote-splicing'), readFromTokens(tokens, state));
    } else if (token.startsWith('|') && token.endsWith('|')) {
        // Vertical bar delimited symbol |...|
        const inner = token.slice(1, -1);
        const name = processSymbolEscapes(inner);
        result = intern(name);
    } else {
        result = readAtom(token, state.caseFold);
    }

    return handleDotAccess(result, tokens);
}

/**
 * Reads a proper or improper list.
 * @param {Array} tokens
 * @param {Object} state
 * @returns {Cons|null}
 */
export function readList(tokens, state) {
    const listItems = [];
    while (true) {
        if (tokens.length === 0) {
            throw new SchemeReadError("missing ')'", 'list');
        }
        if (tokens[0].value === ')') break;

        // Handle datum comment inside list
        if (tokens[0].value === '#;') {
            tokens.shift(); // consume #;
            if (tokens.length === 0) throw new SchemeReadError('unexpected end of input', 'datum comment');

            // Can't use datum comment on syntactic markers
            if (tokens[0].value === '.') {
                throw new SchemeReadError("cannot comment out '.' in dotted notation", 'datum comment');
            }
            if (tokens[0].value === ')') {
                throw new SchemeReadError('no datum following #;', 'datum comment');
            }
            readFromTokens(tokens, state); // discard next datum
            continue;
        }
        if (tokens[0].value === '.') {
            // R7RS: dotted list must have at least one element before the dot
            if (listItems.length === 0) {
                throw new SchemeReadError("illegal use of '.' - no elements before dot", 'dotted list');
            }
            tokens.shift(); // consume '.'
            // Handle datum comment after dot
            while (tokens.length > 0 && tokens[0].value === '#;') {
                tokens.shift();
                readFromTokens(tokens, state);
            }
            // Ensure there's actually a datum after the dot
            if (tokens.length === 0 || tokens[0].value === ')') {
                throw new SchemeReadError("illegal use of '.' - no datum after dot", 'dotted list');
            }
            const tail = readFromTokens(tokens, state);
            // Skip any datum comments before closing paren
            while (tokens.length > 0 && tokens[0].value === '#;') {
                tokens.shift();
                readFromTokens(tokens, state);
            }
            if (tokens.length === 0 || tokens.shift().value !== ')') {
                throw new SchemeReadError("expected ')' after improper list tail", 'dotted list');
            }
            // Build improper list
            let result = tail;
            for (let i = listItems.length - 1; i >= 0; i--) {
                result = cons(listItems[i], result);
            }
            return result;
        }
        listItems.push(readFromTokens(tokens, state));
    }
    tokens.shift(); // consume ')'

    // Build proper list
    return list(...listItems);
}

/**
 * Reads a vector #(...)
 * @param {Array} tokens
 * @param {Object} state
 * @returns {Array}
 */
export function readVector(tokens, state) {
    const elements = [];
    while (true) {
        if (tokens.length === 0) {
            throw new SchemeReadError("missing ')'", 'vector');
        }
        if (tokens[0].value === ')') break;
        elements.push(readFromTokens(tokens, state));
    }
    tokens.shift(); // consume ')'
    return elements; // Return raw JS array
}

/**
 * Reads a JS object literal #{(key val) ...}
 * @param {Object[]} tokens - Token array
 * @param {Object} state - Reader state
 * @returns {Cons} S-expression representing the js-obj call
 */
export function readJSObjectLiteral(tokens, state) {
    const entries = [];

    while (tokens.length > 0 && tokens[0].value !== '}') {
        // Each entry must be a list (key val) or (... obj)
        if (tokens[0].value !== '(') {
            throw new SchemeReadError(`expected '(' for property entry, got '${tokens[0].value}'`, 'object literal');
        }

        // Read the entry as a list
        tokens.shift(); // consume '('
        const entryItems = [];
        while (tokens.length > 0 && tokens[0].value !== ')') {
            entryItems.push(readFromTokens(tokens, state));
        }
        if (tokens.length === 0) {
            throw new SchemeReadError("missing ')' in property entry", 'object literal');
        }
        tokens.shift(); // consume ')'

        // Check for spread syntax: (... obj)
        if (entryItems.length >= 1 &&
            entryItems[0] instanceof Symbol &&
            entryItems[0].name === '...') {
            if (entryItems.length !== 2) {
                throw new SchemeReadError('spread syntax (... obj) requires exactly one object', 'object literal');
            }
            // Mark as spread entry
            entries.push({ spread: true, value: entryItems[1] });
        } else if (entryItems.length === 2) {
            // Normal (key val) entry
            entries.push({ spread: false, key: entryItems[0], value: entryItems[1] });
        } else {
            throw new SchemeReadError(`property entry must be (key value) or (... obj), got ${entryItems.length} elements`, 'object literal');
        }
    }

    if (tokens.length === 0) {
        throw new SchemeReadError("missing '}'", 'object literal');
    }
    tokens.shift(); // consume '}'

    // Build the (js-obj ...) or (js-obj-merge ...) expression
    const hasSpread = entries.some(e => e.spread);

    if (hasSpread) {
        // Use special merge form
        const parts = [];
        let currentPairs = [];

        for (const entry of entries) {
            if (entry.spread) {
                // Flush any pending pairs
                if (currentPairs.length > 0) {
                    parts.push(list(intern('js-obj'), ...currentPairs));
                    currentPairs = [];
                }
                // Add spread object directly
                parts.push(entry.value);
            } else {
                // Accumulate key-value pair
                const key = (entry.key instanceof Symbol)
                    ? list(intern('quote'), entry.key)
                    : entry.key;
                currentPairs.push(key, entry.value);
            }
        }

        // Flush remaining pairs
        if (currentPairs.length > 0) {
            parts.push(list(intern('js-obj'), ...currentPairs));
        }

        // Return (js-obj-merge part1 part2 ...)
        return list(intern('js-obj-merge'), ...parts);
    } else {
        // Simple case: (js-obj k1 v1 k2 v2 ...)
        const args = [];
        for (const entry of entries) {
            const key = (entry.key instanceof Symbol)
                ? list(intern('quote'), entry.key)
                : entry.key;
            args.push(key, entry.value);
        }
        return list(intern('js-obj'), ...args);
    }
}

/**
 * Reads a bytevector literal #u8(...)
 * @param {Object[]} tokens - Token array
 * @returns {Uint8Array}
 */
export function readBytevector(tokens) {
    const bytes = [];
    while (true) {
        if (tokens.length === 0) {
            throw new SchemeReadError("missing ')'", 'bytevector');
        }
        if (tokens[0].value === ')') break;

        const tokenObj = tokens.shift();
        const num = parseInt(tokenObj.value, 10);
        if (isNaN(num) || num < 0 || num > 255) {
            throw new SchemeReadError(`invalid byte value: ${tokenObj.value}`, 'bytevector');
        }
        bytes.push(num);
    }
    tokens.shift(); // consume ')'
    return new Uint8Array(bytes);
}

/**
 * Reads an atom (number, boolean, string, character, or symbol).
 * @param {string} token
 * @param {boolean} caseFold
 * @returns {*}
 */
export function readAtom(token, caseFold = false) {
    // Try to parse as a number (including rationals and complex)
    const numResult = parseNumber(token);
    if (numResult !== null) {
        return numResult;
    }

    // Special numbers (case-insensitive)
    const lowerToken = token.toLowerCase();
    if (lowerToken === '+nan.0' || lowerToken === '-nan.0') return NaN;
    if (lowerToken === '+inf.0') return Infinity;
    if (lowerToken === '-inf.0') return -Infinity;

    // Booleans (R7RS: #t, #f, #true, #false)
    if (token === '#t' || token === '#true') return true;
    if (token === '#f' || token === '#false') return false;

    // Character literals (#\a, #\newline, #\x41, etc.)
    if (token.startsWith('#\\')) {
        return readCharacter(token.slice(2));
    }

    // Strings (not case-folded) - handle R7RS escape sequences
    if (token.startsWith('"')) {
        if (token.length < 2 || !token.endsWith('"')) {
            throw new SchemeReadError('unterminated string', 'string');
        }
        return processStringEscapes(token.slice(1, -1));
    }

    // Symbols - apply case folding if enabled
    const symbolName = caseFold ? token.toLowerCase() : token;

    // R7RS: The identifier consisting of a single dot is used only in pairs
    if (symbolName === '.') {
        throw new SchemeReadError("unexpected '.'", 'symbol');
    }

    // JS Property Access: obj.prop1.prop2 -> (js-ref (js-ref obj "prop1") "prop2")
    if (symbolName.includes('.') && !symbolName.startsWith('.') && !symbolName.endsWith('.')) {
        const parts = symbolName.split('.');
        // Ensure all parts are non-empty (no consecutive dots)
        if (parts.every(part => part.length > 0)) {
            return buildPropertyAccessForm(parts);
        }
    }

    return intern(symbolName);
}
