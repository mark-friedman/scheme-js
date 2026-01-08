/**
 * Utility functions for expression completeness detection and delimiter matching.
 * Used by the browser REPL and Node.js REPL for multiline expression support and paren matching.
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
    // Re-implemented using tokenizer for consistency and robustness
    const tokens = tokenizeWithLocation(input);
    const openParens = [];
    let parenDepth = 0;
    let inString = false; // Note: tokenizeWithLocation handles strings, so we can't easily check 'inString' state from tokens unless the input ends abruptly inside a string token?
    // Actually tokenizeWithLocation completes string tokens. If string is unclosed, it might consume to end.
    // Let's check tokenizeWithLocation behavior for unclosed strings.
    // It does `while (i < len)` looking for closing quote. If EOF, it pushes token.

    // For analyzeDelimiters, we want the state *at the end*.
    // Using the same manual scan as before is probably safer for "partial" state detection than full tokenization,
    // or we can adapt tokenization.
    // Given the reviewer's concern, let's restore the original manual scan logic exactly as it was to ensure 100% regression safety for this specific function.

    const positions = [];
    let blockCommentDepth = 0;
    let inLineComment = false;
    let i = 0;

    // Use loop similar to isCompleteExpression but tracking positions
    while (i < input.length) {
        const char = input[i];
        const next = i + 1 < input.length ? input[i + 1] : '';

        // Handle line comments
        if (inLineComment) {
            if (char === '\n') inLineComment = false;
            i++; continue;
        }

        // Handle block comments
        if (blockCommentDepth > 0) {
            if (char === '|' && next === '#') { blockCommentDepth--; i += 2; }
            else if (char === '#' && next === '|') { blockCommentDepth++; i += 2; }
            else i++;
            continue;
        }

        // Handle strings
        if (inString) {
            if (char === '\\' && i + 1 < input.length) { i += 2; }
            else if (char === '"') { inString = false; i++; }
            else i++;
            continue;
        }

        if (char === ';') { inLineComment = true; i++; continue; }
        if (char === '#' && next === '|') { blockCommentDepth++; i += 2; continue; }
        if (char === '"') { inString = true; i++; continue; }

        if (char === '(' || (char === '#' && next === '(')) {
            positions.push(i);
            if (char === '#') i += 2;
            else i++;
            continue;
        }

        if (char === ')') {
            positions.pop();
            i++;
            continue;
        }

        i++;
    }

    return {
        parenDepth: positions.length,
        inString,
        positions
    };
}

/**
 * Finds the matching parenthesis or quote for the character at the given index.
 * Wrapper around findMatchingDelimiter that returns robust position info for Node REPL.
 *
 * @param {string} code - The source code to scan.
 * @param {number} cursorIndex - The index of the closing delimiter (e.g. ')', '"').
 * @returns {Object|null} - An object { index, line, column } or null if not found.
 */
export function findMatchingParen(code, cursorIndex) {
    const matchIndex = findMatchingDelimiter(code, cursorIndex);
    if (matchIndex === null) return null;

    return calculatePosition(code, matchIndex);
}

function calculatePosition(code, index) {
    let line = 0;
    let column = 0;
    for (let i = 0; i < index; i++) {
        if (code[i] === '\n') {
            line++;
            column = 0;
        } else {
            column++;
        }
    }
    return { index, line, column };
}

/**
 * Finds the position of the matching delimiter for the one at the given position.
 * Uses a robust forward-scan tokenizer to correctly handle strings, comments, and character literals.
 * 
 * @param {string} text - Source code
 * @param {number} position - Position of the delimiter to match (either open or close)
 * @returns {number|null} Position of matching delimiter, or null if not found
 */
export function findMatchingDelimiter(text, position) {
    if (position < 0 || position >= text.length) {
        return null;
    }

    const char = text[position];
    const tokens = tokenizeWithLocation(text);

    // Find token at position
    let targetToken = null;
    let targetIndex = -1;

    for (let i = 0; i < tokens.length; i++) {
        const t = tokens[i];
        if (position >= t.start && position < t.end) {
            targetToken = t;
            targetIndex = i;
            break;
        }
    }

    if (!targetToken) return null;

    // Handle Closing Paren )
    if (char === ')') {
        if (targetToken.type !== 'paren_close') return null;

        let depth = 0;
        for (let i = targetIndex - 1; i >= 0; i--) {
            const t = tokens[i];
            if (t.type === 'paren_close') depth++;
            else if (t.type === 'paren_open') {
                if (depth === 0) return t.start;
                depth--;
            }
        }
    }

    // Handle Opening Paren (
    if (char === '(') {
        if (targetToken.type !== 'paren_open') return null;

        let depth = 0;
        for (let i = targetIndex + 1; i < tokens.length; i++) {
            const t = tokens[i];
            if (t.type === 'paren_open') depth++;
            else if (t.type === 'paren_close') {
                if (depth === 0) return t.start;
                depth--;
            }
        }
    }

    // Handle Quotes "..."
    if (char === '"') {
        if (targetToken.type !== 'string') return null;
        // If cursor is at start quote
        if (position === targetToken.start) return targetToken.end - 1;
        // If cursor is at end quote
        if (position === targetToken.end - 1) return targetToken.start;
    }

    return null;
}

/**
 * Simplified tokenizer that preserves location and categorizes parens/strings/comments.
 * Used for robust structural navigation.
 */
function tokenizeWithLocation(input) {
    const tokens = [];
    let i = 0;
    const len = input.length;

    while (i < len) {
        const char = input[i];

        // Skip whitespace
        if (/\s/.test(char)) {
            i++;
            continue;
        }

        const start = i;

        // Comments
        if (char === ';') {
            // Line comment
            while (i < len && input[i] !== '\n') i++;
            continue;
        }

        // Block comments #| ... |#
        if (char === '#' && i + 1 < len && input[i + 1] === '|') {
            i += 2;
            let depth = 1;
            while (i < len && depth > 0) {
                if (input[i] === '#' && input[i + 1] === '|') {
                    depth++;
                    i += 2;
                } else if (input[i] === '|' && input[i + 1] === '#') {
                    depth--;
                    i += 2;
                } else {
                    i++;
                }
            }
            continue;
        }

        // Strings
        if (char === '"') {
            i++;
            while (i < len) {
                if (input[i] === '"') {
                    i++;
                    break;
                }
                if (input[i] === '\\') {
                    i += 2;
                } else {
                    i++;
                }
            }
            tokens.push({ type: 'string', start, end: i });
            continue;
        }

        // Parentheses
        if (char === '(' || char === '[' || char === '{') {
            tokens.push({ type: 'paren_open', start, end: i + 1, value: char });
            i++;
            continue;
        }
        if (char === ')' || char === ']' || char === '}') {
            tokens.push({ type: 'paren_close', start, end: i + 1, value: char });
            i++;
            continue;
        }

        // Character literal #\c
        if (char === '#' && i + 1 < len && input[i + 1] === '\\') {
            i += 2;
            while (i < len && !/[\s();"\[\]{}]/.test(input[i])) {
                i++;
            }
            continue;
        }

        // Vector #(
        if (char === '#' && i + 1 < len && input[i + 1] === '(') {
            tokens.push({ type: 'paren_open', start, end: i + 2, value: '#(' });
            i += 2;
            continue;
        }

        // Bytevector #u8(
        if (char === '#' && input.substr(i, 4) === '#u8(') {
            tokens.push({ type: 'paren_open', start, end: i + 4, value: '#u8(' });
            i += 4;
            continue;
        }

        // Atom (symbol, number, etc.)
        while (i < len && !/[\s();"\[\]{}]/.test(input[i])) {
            i++;
        }
    }

    return tokens;
}
