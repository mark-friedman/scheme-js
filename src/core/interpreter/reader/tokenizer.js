/**
 * @fileoverview Tokenizer for Scheme S-expressions.
 * Handles lexical analysis including block comments and whitespace tracking.
 */

/**
 * Strips block comments #|...|# from input, including nested ones.
 * @param {string} input - Source code
 * @returns {string} Input with block comments removed
 */
export function stripBlockComments(input) {
    let result = '';
    let i = 0;

    while (i < input.length) {
        if (i + 1 < input.length && input[i] === '#' && input[i + 1] === '|') {
            // Start of block comment - find matching end
            let depth = 1;
            i += 2;
            while (i < input.length && depth > 0) {
                if (i + 1 < input.length && input[i] === '#' && input[i + 1] === '|') {
                    depth++;
                    i += 2;
                } else if (i + 1 < input.length && input[i] === '|' && input[i + 1] === '#') {
                    depth--;
                    i += 2;
                } else {
                    i++;
                }
            }
            // Replace with space to preserve token boundaries
            result += ' ';
        } else {
            result += input[i];
            i++;
        }
    }
    return result;
}

/**
 * Tokenizes Scheme source code into an array of token objects.
 * Each token has a `value` string and `hasPrecedingSpace` boolean.
 * 
 * @param {string} input - Source code (after block comment stripping)
 * @returns {Array<{value: string, hasPrecedingSpace: boolean}>} Token array
 */
export function tokenize(input) {
    // Regex based tokenizer
    // Matches (order matters - longer/more specific matches first):
    // 1. #\ followed by character name or single char (character literals)
    // 2. #u8( - Bytevector start
    // 3. #{ - JS object literal start
    // 4. #( - Vector start
    // 5. #; - Datum comment
    // 6. #!fold-case, #!no-fold-case - Case folding directives
    // 7. ( or ) or } - List/object delimiters
    // 8. ' ` ,@ , - Quote/Quasiquote
    // 9. Strings
    // 10. |...| - Vertical bar delimited symbols
    // 11. Line comments (;...)
    // 12. Complex numbers with inf/nan (e.g., +inf.0+inf.0i, -nan.0+inf.0i)
    // 13. Single special numbers (+nan.0, +inf.0, etc.)
    // 14. Atoms (anything else, stops at whitespace, parens, or semicolon)
    // Note: Updated to capture leading whitespace in group 1
    const regex = /^(\s*)(#\\(?:x[0-9a-fA-F]+|[a-zA-Z]+|.)|#u8\(|#\{|#\(|#;|#!fold-case|#!no-fold-case|'|`|,@|,|[(){}]|"(?:\\.|[^"])*"|\|(?:[^|\\]|\\.)*\||;[^\n]*|[+-]?(?:nan|inf)\.0[+-](?:nan|inf)\.0i|[+-]?(?:nan|inf)\.0|[^\s(){};]+)(.*)/si;

    const tokens = [];
    let current = input;

    // Track if we are at the very beginning of the input
    let isStart = true;

    while (current.length > 0) {
        const match = current.match(regex);
        if (!match) {
            // If we can't match a token but have content, it might be just whitespace at EOF or invalid input
            // Skip one char and try again (fallback, though regex should cover whitespace)
            if (current.trim() === '') break;
            current = current.slice(1);
            continue;
        }

        const whitespace = match[1];
        const tokenStr = match[2];
        current = match[3]; // Rest of string

        if (tokenStr.startsWith(';')) {
            // Comments act as whitespace separator effectively, but we don't count them as "space" for adjacency.
            // Actually, standard behavior: `(a) ;comment\n.prop` -> separate line, effectively separate.
            // We will count comments as breaking adjacency.
            isStart = false;
            continue;
        }

        // hasPrecedingSpace is true if there was whitespace OR we are at start of input
        const hasSpace = (whitespace.length > 0) || isStart;

        tokens.push({ value: tokenStr, hasPrecedingSpace: hasSpace });
        isStart = false; // After first token, not start anymore
    }
    return tokens;
}
