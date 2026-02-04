/**
 * @fileoverview Tokenizer for Scheme S-expressions with source location tracking.
 * Handles lexical analysis including block comments, whitespace tracking, and
 * source position information for debugger support.
 */

/**
 * Source location information for a token or S-expression.
 * @typedef {Object} SourceInfo
 * @property {string} filename - Source file name
 * @property {number} line - 1-indexed line number
 * @property {number} column - 1-indexed column number
 * @property {number} [endLine] - 1-indexed end line number
 * @property {number} [endColumn] - 1-indexed end column number (exclusive)
 */

/**
 * Creates a SourceInfo object.
 * @param {string} filename - Source file name
 * @param {number} line - Start line (1-indexed)
 * @param {number} column - Start column (1-indexed)
 * @param {number} [endLine] - End line
 * @param {number} [endColumn] - End column (exclusive)
 * @returns {SourceInfo}
 */
export function createSourceInfo(filename, line, column, endLine = null, endColumn = null) {
    return {
        filename,
        line,
        column,
        endLine: endLine ?? line,
        endColumn: endColumn ?? column
    };
}

/**
 * Strips block comments #|...|# from input, including nested ones.
 * Returns both the stripped string and a mapping for position adjustment.
 * @param {string} input - Source code
 * @returns {string} Input with block comments removed (replaced with spaces)
 */
export function stripBlockComments(input) {
    let result = '';
    let i = 0;

    while (i < input.length) {
        if (i + 1 < input.length && input[i] === '#' && input[i + 1] === '|') {
            // Start of block comment - find matching end
            let depth = 1;
            i += 2;
            let commentContent = '';
            while (i < input.length && depth > 0) {
                if (i + 1 < input.length && input[i] === '#' && input[i + 1] === '|') {
                    depth++;
                    commentContent += '#|';
                    i += 2;
                } else if (i + 1 < input.length && input[i] === '|' && input[i + 1] === '#') {
                    depth--;
                    if (depth > 0) commentContent += '|#';
                    i += 2;
                } else {
                    commentContent += input[i];
                    i++;
                }
            }
            // Replace comment with spaces/newlines to preserve line numbering
            for (const ch of commentContent) {
                result += (ch === '\n' || ch === '\r') ? ch : ' ';
            }
            // Add space for the |# closing
            result += '  ';
        } else {
            result += input[i];
            i++;
        }
    }
    return result;
}

/**
 * Tokenizes Scheme source code into an array of token objects with source locations.
 * Each token has a `value` string, `hasPrecedingSpace` boolean, and `source` info.
 * 
 * @param {string} input - Source code (after block comment stripping)
 * @param {string} [filename='<unknown>'] - Source file name for error messages
 * @returns {Array<{value: string, hasPrecedingSpace: boolean, source: SourceInfo}>} Token array
 */
export function tokenize(input, filename = '<unknown>') {
    const tokens = [];
    let pos = 0;
    let line = 1;
    let column = 1;
    let isStart = true;

    /**
     * Advance position and update line/column tracking.
     * @param {number} count - Number of characters to advance
     */
    function advance(count = 1) {
        for (let i = 0; i < count && pos < input.length; i++) {
            if (input[pos] === '\n') {
                line++;
                column = 1;
            } else if (input[pos] === '\r') {
                // Handle \r\n as single newline
                if (pos + 1 < input.length && input[pos + 1] === '\n') {
                    pos++;
                    i++;
                }
                line++;
                column = 1;
            } else {
                column++;
            }
            pos++;
        }
    }

    /**
     * Peek at current character.
     */
    function peek() {
        return pos < input.length ? input[pos] : null;
    }

    /**
     * Check if current position starts with a string.
     */
    function startsWith(str) {
        return input.slice(pos, pos + str.length) === str;
    }

    /**
     * Skip whitespace and track if any was found.
     * @returns {boolean} True if whitespace was skipped
     */
    function skipWhitespace() {
        let skipped = false;
        while (pos < input.length) {
            const ch = input[pos];
            if (ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r') {
                advance();
                skipped = true;
            } else {
                break;
            }
        }
        return skipped;
    }

    /**
     * Skip a line comment (starts with ;).
     * @returns {boolean} True if a comment was skipped
     */
    function skipLineComment() {
        if (peek() === ';') {
            while (pos < input.length && input[pos] !== '\n') {
                advance();
            }
            return true;
        }
        return false;
    }

    /**
     * Read a string token (including quotes).
     * @returns {string}
     */
    function readString() {
        let str = '"';
        advance(); // Skip opening quote

        while (pos < input.length) {
            const ch = input[pos];
            if (ch === '\\') {
                str += ch;
                advance();
                if (pos < input.length) {
                    str += input[pos];
                    advance();
                }
            } else if (ch === '"') {
                str += ch;
                advance();
                break;
            } else {
                str += ch;
                advance();
            }
        }
        return str;
    }

    /**
     * Read a vertical-bar symbol |...|.
     * @returns {string}
     */
    function readBarSymbol() {
        let str = '|';
        advance(); // Skip opening |

        while (pos < input.length) {
            const ch = input[pos];
            if (ch === '\\') {
                str += ch;
                advance();
                if (pos < input.length) {
                    str += input[pos];
                    advance();
                }
            } else if (ch === '|') {
                str += ch;
                advance();
                break;
            } else {
                str += ch;
                advance();
            }
        }
        return str;
    }

    /**
     * Read an atom (identifier, number, etc.) until a delimiter.
     * @returns {string}
     */
    function readAtom() {
        let atom = '';
        while (pos < input.length) {
            const ch = input[pos];
            // Delimiters: whitespace, parens, braces, semicolon
            if (' \t\n\r(){}[];'.includes(ch)) {
                break;
            }
            atom += ch;
            advance();
        }
        return atom;
    }

    /**
     * Read a character literal #\...
     * @returns {string}
     */
    function readCharLiteral() {
        let str = '#\\';
        advance(); // Skip #
        advance(); // Skip \

        // Check for hex escape #\xNN
        if (pos < input.length && input[pos].toLowerCase() === 'x') {
            str += input[pos];
            advance();
            while (pos < input.length && /[0-9a-fA-F]/.test(input[pos])) {
                str += input[pos];
                advance();
            }
            return str;
        }

        // Check for named character (e.g., #\newline, #\space)
        if (pos < input.length && /[a-zA-Z]/.test(input[pos])) {
            // Read the full name
            while (pos < input.length && /[a-zA-Z]/.test(input[pos])) {
                str += input[pos];
                advance();
            }
            return str;
        }

        // Single character
        if (pos < input.length) {
            str += input[pos];
            advance();
        }
        return str;
    }

    // Main tokenization loop
    while (pos < input.length) {
        // Skip whitespace and comments, tracking if space was found
        let hasSpace = isStart;

        while (true) {
            const skippedWs = skipWhitespace();
            const skippedComment = skipLineComment();
            if (skippedWs || skippedComment) {
                hasSpace = true;
            } else {
                break;
            }
        }

        if (pos >= input.length) break;

        // Record token start position
        const startLine = line;
        const startColumn = column;
        let tokenValue = '';

        const ch = input[pos];
        const next = pos + 1 < input.length ? input[pos + 1] : null;

        // Handle different token types
        if (ch === '(' || ch === ')' || ch === '{' || ch === '}') {
            tokenValue = ch;
            advance();
        } else if (ch === "'" || ch === '`') {
            tokenValue = ch;
            advance();
        } else if (ch === ',' && next === '@') {
            tokenValue = ',@';
            advance();
            advance();
        } else if (ch === ',') {
            tokenValue = ch;
            advance();
        } else if (ch === '"') {
            tokenValue = readString();
        } else if (ch === '|') {
            tokenValue = readBarSymbol();
        } else if (ch === '#') {
            // Handle various # tokens
            if (next === '(') {
                tokenValue = '#(';
                advance();
                advance();
            } else if (next === '{') {
                tokenValue = '#{';
                advance();
                advance();
            } else if (next === ';') {
                tokenValue = '#;';
                advance();
                advance();
            } else if (startsWith('#u8(')) {
                tokenValue = '#u8(';
                advance();
                advance();
                advance();
                advance();
            } else if (next === '\\') {
                tokenValue = readCharLiteral();
            } else if (startsWith('#!fold-case')) {
                tokenValue = '#!fold-case';
                for (let i = 0; i < 11; i++) advance();
            } else if (startsWith('#!no-fold-case')) {
                tokenValue = '#!no-fold-case';
                for (let i = 0; i < 14; i++) advance();
            } else {
                // Generic # token - read as atom
                tokenValue = readAtom();
            }
        } else {
            // Read atom (identifier, number, etc.)
            tokenValue = readAtom();
        }

        if (tokenValue) {
            tokens.push({
                value: tokenValue,
                hasPrecedingSpace: hasSpace,
                source: createSourceInfo(filename, startLine, startColumn, line, column)
            });
        }

        isStart = false;
    }

    return tokens;
}
