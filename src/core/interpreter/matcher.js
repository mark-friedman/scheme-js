
/**
 * Finds the matching parenthesis or quote for the character at the given index.
 *
 * @param {string} code - The source code to scan.
 * @param {number} cursorIndex - The index of the closing delimiter (e.g. ')', '"').
 * @returns {Object|null} - An object { index, line, column } or null if not found.
 */
export function findMatchingParen(code, cursorIndex) {
    if (cursorIndex < 0 || cursorIndex >= code.length) return null;

    const char = code[cursorIndex];

    // Reverse scan
    let balance = 0;
    let inString = false;
    let inChar = false; // #\c

    // We can't easily reverse scan complex tokens like comments and strings without context.
    // So we forward scan up to cursorIndex to map the structure, then pick the match.
    // This is safer and reuses logic.

    const tokens = tokenizeWithLocation(code);

    // Find the token at cursorIndex
    // If it's a closing paren, find its match.
    // If it's a quote, find its start.

    // Flattened structure: array of tokens with { type, start, end, value }

    let targetToken = null;
    let targetTokenIndex = -1;

    for (let i = 0; i < tokens.length; i++) {
        const t = tokens[i];
        if (t.start <= cursorIndex && t.end > cursorIndex) {
            targetToken = t;
            targetTokenIndex = i;
            break;
        }
    }

    if (!targetToken) return null;

    // Logic for parenthesis
    if (char === ')') {
        if (targetToken.type !== 'paren_close') return null; // Should match

        // Scan backwards in token stream for matching open
        let depth = 0;
        for (let i = targetTokenIndex - 1; i >= 0; i--) {
            const t = tokens[i];
            if (t.type === 'paren_close') {
                depth++;
            } else if (t.type === 'paren_open') {
                if (depth === 0) {
                    return calculatePosition(code, t.start);
                }
                depth--;
            }
        }
    }

    // Logic for quotes
    if (char === '"') {
        if (targetToken.type !== 'string') return null;
        // The token is the whole string "..."
        // If cursor is at the end quote (last char), we return the start quote (first char).
        // targetToken.start is the index of opening "
        // targetToken.end is index after closing "

        // Ensure cursor is actually on the closing quote
        if (cursorIndex === targetToken.end - 1) {
            return calculatePosition(code, targetToken.start);
        }
    }

    return null;
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
 * Simplified tokenizer that preserves location and categorizes parens/strings/comments.
 * Doesn't need to be full Scheme parser, just structure aware.
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
            // Don't emit comment tokens for matching purposes
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

        // Datum comment #; -> Skip next datum?
        // For visual matching, we probably still want to match parens inside datum comments?
        // Emacs usually matches parens even if commented out in block comments sometimes, but strictly speaking Scheme parser ignores them.
        // The user request is "REPL no longer does parenthesis matching".
        // Typically REPLs match structural parens regardless of datum comments, but NOT inside string literals or line comments.
        // Let's treat #; as just a token prefix for now (ignoring its semantic meaning of skipping).

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

        // Character literal #\c (might contain parens or quotes that shouldn't be matched)
        if (char === '#' && i + 1 < len && input[i + 1] === '\\') {
            // Read character
            i += 2;
            // Read next char or named char
            // Heuristic: read until delimiter
            while (i < len && !/[\s();"\[\]{}]/.test(input[i])) {
                i++;
            }
             // If stopped at delimiter, it's fine.
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
        // Consume until delimiter
        while (i < len && !/[\s();"\[\]{}]/.test(input[i])) {
            i++;
        }
        // tokens.push({ type: 'atom', start, end: i });
    }

    return tokens;
}
