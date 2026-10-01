import { Port, EOF_OBJECT } from './ports.js';

/**
 * Where the characters after a position in a string end, counting a character
 * outside the Basic Multilingual Plane, two UTF-16 code units, as one
 * character, as `read-char` and `string->list` do. (`string-length` and
 * `string-ref` count code units, since a string's positions are code units.)
 * @param {string} str - The string.
 * @param {number} start - A position in it, between characters.
 * @param {number} count - How many characters to pass.
 * @returns {{end: number, passed: number}} The position after them, or the
 *   string's end if it has fewer, and how many were passed.
 */
export function passCharacters(str, start, count) {
    let end = start;
    let passed = 0;
    while (passed < count && end < str.length) {
        end += str.codePointAt(end) > 0xFFFF ? 2 : 1;
        passed++;
    }
    return { end, passed };
}

/**
 * String input port - reads from a string.
 */
export class StringInputPort extends Port {
    /**
     * @param {string} str - The string to read from.
     */
    constructor(str) {
        super('input');
        this._string = str;
        this._pos = 0;
    }

    /** @returns {boolean} Whether there's more to read. */
    hasMore() { return this._pos < this._string.length; }

    /**
     * Reads the next character.
     * @returns {string|object} The character, as a string of its one or two
     *   code units, or EOF_OBJECT.
     */
    readChar() {
        if (!this._open) {
            throw new Error('read-char: port is closed');
        }
        const ch = this.peekChar();
        if (ch !== EOF_OBJECT) this._pos += ch.length;
        return ch;
    }

    /**
     * Peeks at the next character without consuming it.
     * @returns {string|object} The character, as a string of its one or two
     *   code units, or EOF_OBJECT.
     */
    peekChar() {
        if (!this._open) {
            throw new Error('peek-char: port is closed');
        }
        if (this._pos >= this._string.length) {
            return EOF_OBJECT;
        }
        return String.fromCodePoint(this._string.codePointAt(this._pos));
    }

    /**
     * Reads a line (up to newline or EOF).
     * @returns {string|object} The line or EOF_OBJECT.
     */
    readLine() {
        if (!this._open) {
            throw new Error('read-line: port is closed');
        }
        if (this._pos >= this._string.length) {
            return EOF_OBJECT;
        }
        let line = '';
        while (this._pos < this._string.length) {
            const ch = this._string[this._pos++];
            if (ch === '\n') {
                return line;
            }
            if (ch === '\r') {
                // Handle \r\n
                if (this._pos < this._string.length && this._string[this._pos] === '\n') {
                    this._pos++;
                }
                return line;
            }
            line += ch;
        }
        return line;
    }

    /**
     * Reads up to k characters.
     * @param {number} k - Maximum characters to read.
     * @returns {string|object} The string or EOF_OBJECT.
     */
    readString(k) {
        if (!this._open) {
            throw new Error('read-string: port is closed');
        }
        if (this._pos >= this._string.length) {
            return EOF_OBJECT;
        }
        const { end } = passCharacters(this._string, this._pos, k);
        const result = this._string.slice(this._pos, end);
        this._pos = end;
        return result;
    }

    /**
     * Checks if a character is ready.
     * @returns {boolean} Always true for string ports.
     */
    charReady() {
        // A string's characters are all there to read, and at its end so is
        // the end of file, which R7RS has `char-ready?` answer #t for too.
        return this._open;
    }

    toString() {
        return `#<string-input-port:${this._open ? 'open' : 'closed'}>`;
    }
}

/**
 * String output port - writes to an accumulator.
 */
export class StringOutputPort extends Port {
    constructor() {
        super('output');
        this._buffer = '';
    }

    /**
     * Writes a character.
     * @param {string} ch - The character to write.
     */
    writeChar(ch) {
        if (!this._open) {
            throw new Error('write-char: port is closed');
        }
        this._buffer += ch;
    }

    /**
     * Writes a string.
     * @param {string} str - The string to write.
     * @param {number} start - Optional start index.
     * @param {number} end - Optional end index.
     */
    writeString(str, start = 0, end = str.length) {
        if (!this._open) {
            throw new Error('write-string: port is closed');
        }
        this._buffer += str.slice(start, end);
    }

    /**
     * Gets the accumulated string.
     * @returns {string} The output string.
     */
    getString() {
        return this._buffer;
    }

    toString() {
        return `#<string-output-port:${this._open ? 'open' : 'closed'}>`;
    }
}
