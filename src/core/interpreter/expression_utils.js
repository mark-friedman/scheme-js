/**
 * Utility functions for expression completeness detection and delimiter matching.
 * Used by the browser REPL for multiline expression support and paren matching.
 * @module expression_utils
 */

/**
 * Checks if a string contains one or more complete S-expressions.
 * Returns true if the input is complete (can be parsed), false if incomplete
 * (unclosed parentheses, strings, or quotes needing more input).
 * 
 * @param {string} input - Source code to check
 * @returns {boolean} True if input contains complete expression(s)
 */
export function isCompleteExpression(input) {
    if (!input || input.trim() === '') {
        return false;
    }

    // Track nesting depth and string/comment state
    let parenDepth = 0;
    let bracketDepth = 0; // for #( vectors
    let inString = false;
    let inLineComment = false;
    let blockCommentDepth = 0;
    let i = 0;

    while (i < input.length) {
        const char = input[i];
        const next = i + 1 < input.length ? input[i + 1] : '';

        // Handle line comments
        if (inLineComment) {
            if (char === '\n') {
                inLineComment = false;
            }
            i++;
            continue;
        }

        // Handle block comments
        if (blockCommentDepth > 0) {
            if (char === '|' && next === '#') {
                blockCommentDepth--;
                i += 2;
            } else if (char === '#' && next === '|') {
                blockCommentDepth++;
                i += 2;
            } else {
                i++;
            }
            continue;
        }

        // Handle strings
        if (inString) {
            if (char === '\\' && i + 1 < input.length) {
                // Escape sequence - skip next char
                i += 2;
            } else if (char === '"') {
                inString = false;
                i++;
            } else {
                i++;
            }
            continue;
        }

        // Start of line comment
        if (char === ';') {
            inLineComment = true;
            i++;
            continue;
        }

        // Start of block comment
        if (char === '#' && next === '|') {
            blockCommentDepth++;
            i += 2;
            continue;
        }

        // Start of string
        if (char === '"') {
            inString = true;
            i++;
            continue;
        }

        // Parentheses
        if (char === '(') {
            parenDepth++;
            i++;
            continue;
        }

        if (char === ')') {
            parenDepth--;
            // Mismatched closing paren - invalid but "complete" in the sense of not needing more input
            if (parenDepth < 0) {
                return true; // Let the parser report the error
            }
            i++;
            continue;
        }

        // Vector start #(
        if (char === '#' && next === '(') {
            bracketDepth++;
            i += 2;
            continue;
        }

        // Bytevector start #u8(
        if (char === '#' && next === 'u' && i + 3 < input.length &&
            input[i + 2] === '8' && input[i + 3] === '(') {
            bracketDepth++;
            i += 4;
            continue;
        }

        // Quote-like prefixes (' ` , ,@) - these require a following datum
        if (char === "'" || char === '`') {
            // Need to check if there's a complete datum after this
            const remaining = input.slice(i + 1).trim();
            if (remaining === '') {
                return false; // Quote with nothing after it
            }
            i++;
            continue;
        }

        if (char === ',') {
            if (next === '@') {
                i += 2;
            } else {
                i++;
            }
            // Need to check if there's a complete datum after this
            const remaining = input.slice(i).trim();
            if (remaining === '') {
                return false; // Unquote with nothing after it
            }
            continue;
        }

        i++;
    }

    // Check final state
    if (inString) {
        return false; // Unclosed string
    }
    if (blockCommentDepth > 0) {
        return false; // Unclosed block comment
    }
    if (parenDepth > 0 || bracketDepth > 0) {
        return false; // Unclosed parentheses
    }

    return true;
}

/**
 * Analyzes the balance of delimiters in the input.
 * Returns information about unclosed delimiters for UI feedback.
 * 
 * @param {string} input - Source code to analyze
 * @returns {{parenDepth: number, inString: boolean, positions: number[]}} 
 *          Delimiter analysis with positions of unmatched opening parens
 */
export function analyzeDelimiters(input) {
    const openParens = []; // Stack of positions of unmatched opening parens
    let inString = false;
    let inLineComment = false;
    let blockCommentDepth = 0;
    let i = 0;

    while (i < input.length) {
        const char = input[i];
        const next = i + 1 < input.length ? input[i + 1] : '';

        // Handle line comments
        if (inLineComment) {
            if (char === '\n') {
                inLineComment = false;
            }
            i++;
            continue;
        }

        // Handle block comments
        if (blockCommentDepth > 0) {
            if (char === '|' && next === '#') {
                blockCommentDepth--;
                i += 2;
            } else if (char === '#' && next === '|') {
                blockCommentDepth++;
                i += 2;
            } else {
                i++;
            }
            continue;
        }

        // Handle strings
        if (inString) {
            if (char === '\\' && i + 1 < input.length) {
                i += 2;
            } else if (char === '"') {
                inString = false;
                i++;
            } else {
                i++;
            }
            continue;
        }

        if (char === ';') {
            inLineComment = true;
            i++;
            continue;
        }

        if (char === '#' && next === '|') {
            blockCommentDepth++;
            i += 2;
            continue;
        }

        if (char === '"') {
            inString = true;
            i++;
            continue;
        }

        if (char === '(' || (char === '#' && next === '(')) {
            openParens.push(i);
            if (char === '#') {
                i += 2;
            } else {
                i++;
            }
            continue;
        }

        if (char === ')') {
            openParens.pop();
            i++;
            continue;
        }

        i++;
    }

    return {
        parenDepth: openParens.length,
        inString,
        positions: openParens
    };
}

/**
 * Finds the position of the matching delimiter for the one at the given position.
 * 
 * @param {string} text - Source code
 * @param {number} position - Position of the delimiter to match
 * @returns {number|null} Position of matching delimiter, or null if not found
 */
export function findMatchingDelimiter(text, position) {
    if (position < 0 || position >= text.length) {
        return null;
    }

    const char = text[position];

    // Opening delimiter - search forward
    if (char === '(') {
        return findForward(text, position, '(', ')');
    }

    // Closing delimiter - search backward
    if (char === ')') {
        return findBackward(text, position, '(', ')');
    }

    return null;
}

/**
 * Search forward for matching closing delimiter.
 */
function findForward(text, startPos, open, close) {
    let depth = 0;
    let inString = false;
    let inLineComment = false;
    let blockCommentDepth = 0;

    for (let i = startPos; i < text.length; i++) {
        const char = text[i];
        const next = i + 1 < text.length ? text[i + 1] : '';

        // Skip comments and strings
        if (inLineComment) {
            if (char === '\n') inLineComment = false;
            continue;
        }
        if (blockCommentDepth > 0) {
            if (char === '|' && next === '#') { blockCommentDepth--; i++; }
            else if (char === '#' && next === '|') { blockCommentDepth++; i++; }
            continue;
        }
        if (inString) {
            if (char === '\\') { i++; continue; }
            if (char === '"') inString = false;
            continue;
        }
        if (char === ';') { inLineComment = true; continue; }
        if (char === '#' && next === '|') { blockCommentDepth++; i++; continue; }
        if (char === '"') { inString = true; continue; }

        // Track parentheses
        if (char === open) {
            depth++;
        } else if (char === close) {
            depth--;
            if (depth === 0) {
                return i;
            }
        }
    }

    return null;
}

/**
 * Search backward for matching opening delimiter.
 */
function findBackward(text, startPos, open, close) {
    let depth = 0;
    // Simple backward search - doesn't handle all comment cases perfectly
    // but good enough for UI highlighting
    let inString = false;

    for (let i = startPos; i >= 0; i--) {
        const char = text[i];
        const prev = i > 0 ? text[i - 1] : '';

        // Basic string handling (imperfect but functional)
        if (char === '"' && prev !== '\\') {
            inString = !inString;
            continue;
        }
        if (inString) continue;

        // Track parentheses
        if (char === close) {
            depth++;
        } else if (char === open) {
            depth--;
            if (depth === 0) {
                return i;
            }
        }
    }

    return null;
}
