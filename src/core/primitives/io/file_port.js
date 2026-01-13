import { Port, EOF_OBJECT } from './ports.js';

// ============================================================================
// Environment Detection
// ============================================================================

/**
 * Detect if running in Node.js (vs browser).
 */
const isNode = typeof process !== 'undefined' &&
    process.versions != null &&
    process.versions.node != null;

/**
 * Node.js fs module (loaded dynamically to avoid browser errors).
 * @type {Object|null}
 */
let fs = null;
if (isNode) {
    // Dynamic import for Node.js only
    try {
        fs = await import('node:fs');
    } catch (e) {
        // Fallback for older Node versions
        try {
            fs = await import('fs');
        } catch (e2) {
            console.warn('Failed to load fs module in Node.js environment');
        }
    }
}

// ============================================================================
// File Port Classes (Node.js Only)
// ============================================================================

/**
 * File input port - reads from a file (Node.js only).
 */
export class FileInputPort extends Port {
    /**
     * @param {string} filename - Path to the file.
     */
    constructor(filename) {
        super('input');
        if (!isNode || !fs) {
            throw new Error('open-input-file: file I/O not supported in browser');
        }
        this._filename = filename;
        try {
            this._content = fs.readFileSync(filename, 'utf8');
        } catch (e) {
            throw new Error(`open-input-file: cannot open file ${filename}: ${e.message}`);
        }
        this._pos = 0;
    }

    /** @returns {boolean} Whether there's more to read. */
    hasMore() { return this._pos < this._content.length; }

    readChar() {
        if (!this._open) throw new Error('read-char: port is closed');
        if (this._pos >= this._content.length) return EOF_OBJECT;
        return this._content[this._pos++];
    }

    peekChar() {
        if (!this._open) throw new Error('peek-char: port is closed');
        if (this._pos >= this._content.length) return EOF_OBJECT;
        return this._content[this._pos];
    }

    readLine() {
        if (!this._open) throw new Error('read-line: port is closed');
        if (this._pos >= this._content.length) return EOF_OBJECT;
        let line = '';
        while (this._pos < this._content.length) {
            const ch = this._content[this._pos++];
            if (ch === '\n') return line;
            if (ch === '\r') {
                if (this._pos < this._content.length && this._content[this._pos] === '\n') {
                    this._pos++;
                }
                return line;
            }
            line += ch;
        }
        return line;
    }

    readString(k) {
        if (!this._open) throw new Error('read-string: port is closed');
        if (this._pos >= this._content.length) return EOF_OBJECT;
        const end = Math.min(this._pos + k, this._content.length);
        const result = this._content.slice(this._pos, end);
        this._pos = end;
        return result;
    }

    charReady() {
        return this._open && this._pos < this._content.length;
    }

    toString() {
        return `#<file-input-port:${this._filename}:${this._open ? 'open' : 'closed'}>`;
    }
}

/**
 * File output port - writes to a file (Node.js only).
 */
export class FileOutputPort extends Port {
    /**
     * @param {string} filename - Path to the file.
     */
    constructor(filename) {
        super('output');
        if (!isNode || !fs) {
            throw new Error('open-output-file: file I/O not supported in browser');
        }
        this._filename = filename;
        this._buffer = '';
        // Clear file initially
        try {
            fs.writeFileSync(filename, '');
        } catch (e) {
            throw new Error(`open-output-file: cannot open file ${filename}: ${e.message}`);
        }
    }

    writeChar(ch) {
        if (!this._open) throw new Error('write-char: port is closed');
        this._buffer += ch;
    }

    writeString(str, start = 0, end = str.length) {
        if (!this._open) throw new Error('write-string: port is closed');
        this._buffer += str.slice(start, end);
    }

    /**
     * Flushes buffer to file.
     */
    flush() {
        if (this._buffer.length > 0) {
            try {
                fs.appendFileSync(this._filename, this._buffer);
                this._buffer = '';
            } catch (e) {
                console.error(`Error flushing file port ${this._filename}: ${e.message}`);
            }
        }
    }

    /**
     * Closes the port and writes remaining buffer.
     */
    close() {
        if (this._open) {
            this.flush();
            this._open = false;
        }
    }

    toString() {
        return `#<file-output-port:${this._filename}:${this._open ? 'open' : 'closed'}>`;
    }
}

/**
 * Checks if a file exists.
 * @param {string} filename
 * @returns {boolean}
 */
export function fileExists(filename) {
    if (!isNode || !fs) return false;
    try {
        return fs.existsSync(filename);
    } catch (e) {
        return false;
    }
}

/**
 * Deletes a file.
 * @param {string} filename
 */
export function deleteFile(filename) {
    if (!isNode || !fs) {
        throw new Error('delete-file: file I/O not supported in browser');
    }
    try {
        fs.unlinkSync(filename);
    } catch (e) {
        throw new Error(`delete-file: cannot delete file ${filename}: ${e.message}`);
    }
}

