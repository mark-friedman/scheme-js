import { Port, EOF_OBJECT } from './ports.js';

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
     * @returns {string|object} The character or EOF_OBJECT.
     */
    readChar() {
        if (!this._open) {
            throw new Error('read-char: port is closed');
        }
        if (this._pos >= this._string.length) {
            return EOF_OBJECT;
        }
        return this._string[this._pos++];
    }

    /**
     * Peeks at the next character without consuming it.
     * @returns {string|object} The character or EOF_OBJECT.
     */
    peekChar() {
        if (!this._open) {
            throw new Error('peek-char: port is closed');
        }
        if (this._pos >= this._string.length) {
            return EOF_OBJECT;
        }
        return this._string[this._pos];
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
        const end = Math.min(this._pos + k, this._string.length);
        const result = this._string.slice(this._pos, end);
        this._pos = end;
        return result;
    }

    /**
     * Checks if a character is ready.
     * @returns {boolean} Always true for string ports.
     */
    charReady() {
        return this._open && this._pos < this._string.length;
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
