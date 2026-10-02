/**
 * @fileoverview Tokenizer for Scheme S-expressions with source location tracking.
 * Handles lexical analysis including block comments, whitespace tracking, and
 * source position information for debugger support.
 */

import { SchemeReadError } from '../errors.js';

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
 * Tokenizes Scheme source code into an array of token objects with source locations.
 * Each token has a `value` string, `hasPrecedingSpace` boolean, `source` info,
 * and the `offset` in the input where it starts.
 *
 * Line and block comments are skipped where a token could start, and only
 * there (R7RS 2.2): a `#|` inside a string, a |symbol| or a character is part
 * of that token, and one inside a line comment is part of the comment. A datum
 * comment, `#;`, is a token, for the parser to skip the datum after it.
 *
 * Input that ends inside a string, a |symbol|, a block comment or a `#\` is
 * an error marked `incomplete` (`SchemeReadError.endOfInput`), whose `offset`
 * is where the unfinished one begins.
 *
 * @param {string} input - Source code
 * @param {string} [filename='<unknown>'] - Source file name for error messages
 * @returns {Array<{value: string, hasPrecedingSpace: boolean, source: SourceInfo, offset: number}>} Token array
 * @throws {SchemeReadError} If the input ends inside a token or a block comment
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
            // Up to the line ending, which R7RS allows to be CR LF or CR as
            // well as LF, and not over it: `advance` steps over CR LF as one,
            // so a loop watching only for LF would never see it.
            while (pos < input.length && input[pos] !== '\n' && input[pos] !== '\r') {
                advance();
            }
            return true;
        }
        return false;
    }

    /**
     * Whether a block comment starts at the current position.
     * @returns {boolean}
     */
    function atBlockComment() {
        return input[pos] === '#' && input[pos + 1] === '|';
    }

    /**
     * Skip a block comment, `#| ... |#`, and the comments nested in it.
     * Inside one only `#|` and `|#` mean anything: R7RS 2.2 defines its text
     * as any characters but those two, so a string or a character in it is
     * not read as one.
     * @returns {boolean} True if a comment was skipped
     * @throws {SchemeReadError} If the input ends inside the comment
     */
    function skipBlockComment() {
        if (!atBlockComment()) return false;
        const startLine = line;
        const startColumn = column;
        const startOffset = pos;
        advance(2);
        let depth = 1;
        while (depth > 0) {
            if (pos >= input.length) {
                throw SchemeReadError.endOfInput('unterminated block comment', 'block comment', startLine, startColumn, startOffset);
            }
            if (atBlockComment()) {
                depth++;
                advance(2);
            } else if (input[pos] === '|' && input[pos + 1] === '#') {
                depth--;
                advance(2);
            } else {
                advance();
            }
        }
        return true;
    }

    /**
     * Read a string token (including quotes).
     * @returns {string}
     * @throws {SchemeReadError} If the input ends before the closing quote
     */
    function readString() {
        const startLine = line;
        const startColumn = column;
        const startOffset = pos;
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
                return str;
            } else {
                str += ch;
                advance();
            }
        }
        // The input ended first, perhaps just after an escaped quote, which
        // closes nothing
        throw SchemeReadError.endOfInput('unterminated string', 'string', startLine, startColumn, startOffset);
    }

    /**
     * Read a vertical-bar symbol |...|.
     * @returns {string}
     * @throws {SchemeReadError} If the input ends before the closing bar
     */
    function readBarSymbol() {
        const startLine = line;
        const startColumn = column;
        const startOffset = pos;
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
                return str;
            } else {
                str += ch;
                advance();
            }
        }
        throw SchemeReadError.endOfInput('unterminated |symbol|', 'symbol', startLine, startColumn, startOffset);
    }

    /**
     * Read an atom (identifier, number, etc.) until a delimiter.
     * @returns {string}
     */
    function readAtom() {
        let atom = '';
        while (pos < input.length) {
            const ch = input[pos];
            // Delimiters (R7RS 7.1.1): whitespace, a vertical line, parens,
            // a double quote, a semicolon; braces and brackets; and the start
            // of a block comment, which ends an identifier as whitespace would
            if (' \t\n\r(){}[];"|'.includes(ch) || atBlockComment()) {
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
     * @throws {SchemeReadError} If the input ends just after the `#\`
     */
    function readCharLiteral() {
        if (pos + 2 >= input.length) {
            throw SchemeReadError.endOfInput('unexpected end of input after #\\', 'character', line, column, pos);
        }
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

    // A script header (SRFI 22): the first line of a program written to be
    // run from a shell, `#!/usr/bin/env chibi-scheme` or `#! ` and a path.
    // R7RS has no such syntax, and its own `#!` directives, `#!fold-case` and
    // `#!no-fold-case`, begin with neither; the line is skipped as a comment.
    if (input.startsWith('#!/') || input.startsWith('#! ')) {
        while (pos < input.length && input[pos] !== '\n' && input[pos] !== '\r') advance();
    }

    // Main tokenization loop
    while (pos < input.length) {
        // Skip whitespace and comments, tracking if space was found
        let hasSpace = isStart;

        while (true) {
            const skippedWs = skipWhitespace();
            const skippedComment = skipLineComment() || skipBlockComment();
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
        const startOffset = pos;
        let tokenValue = '';

        const ch = input[pos];
        const next = pos + 1 < input.length ? input[pos + 1] : null;

        // Handle different token types
        // Square brackets are tokens like parentheses, for the parser to
        // report. They delimit an atom, so reading one as an atom would read
        // nothing, and the loop would never pass it.
        if (ch === '(' || ch === ')' || ch === '{' || ch === '}' || ch === '[' || ch === ']') {
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
                source: createSourceInfo(filename, startLine, startColumn, line, column),
                offset: startOffset
            });
        }

        isStart = false;
    }

    return tokens;
}
