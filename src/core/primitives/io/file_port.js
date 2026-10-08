import { Port } from './ports.js';
import { StringInputPort } from './string_port.js';
import { BytevectorInputPort, BytevectorOutputPort } from './bytevector_port.js';

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
 * Node.js's `fs`, or null in a browser. Got synchronously, not by a top-level
 * `await`, which a bundler that wraps modules in functions cannot keep.
 * @type {Object|null}
 */
const fs = isNode ? process.getBuiltinModule('node:fs') : null;

// ============================================================================
// File Port Classes (Node.js Only)
// ============================================================================

/**
 * File input port - reads from a file (Node.js only). The file is read whole
 * when the port is opened, and read from as a string port reads its string.
 */
export class FileInputPort extends StringInputPort {
    /**
     * @param {string} filename - Path to the file.
     */
    constructor(filename) {
        if (!isNode || !fs) {
            throw new Error('open-input-file: file I/O not supported in browser');
        }
        let content;
        try {
            content = fs.readFileSync(filename, 'utf8');
        } catch (e) {
            throw new Error(`open-input-file: cannot open file ${filename}: ${e.message}`);
        }
        super(content);
        this._filename = filename;
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
 * Binary file input port - reads a file's bytes (Node.js only). The file is
 * read whole when the port is opened, and read from as a bytevector port
 * reads its bytevector.
 */
export class BinaryFileInputPort extends BytevectorInputPort {
    /**
     * @param {string} filename - Path to the file.
     */
    constructor(filename) {
        if (!isNode || !fs) {
            throw new Error('open-binary-input-file: file I/O not supported in browser');
        }
        let content;
        try {
            content = fs.readFileSync(filename);
        } catch (e) {
            throw new Error(`open-binary-input-file: cannot open file ${filename}: ${e.message}`);
        }
        super(new Uint8Array(content.buffer, content.byteOffset, content.length));
        this._filename = filename;
    }

    toString() {
        return `#<binary-file-input-port:${this._filename}:${this._open ? 'open' : 'closed'}>`;
    }
}

/**
 * Binary file output port - writes bytes to a file (Node.js only), as a
 * bytevector port collects them, and appends them to the file when flushed or
 * closed.
 */
export class BinaryFileOutputPort extends BytevectorOutputPort {
    /**
     * @param {string} filename - Path to the file.
     */
    constructor(filename) {
        super();
        if (!isNode || !fs) {
            throw new Error('open-binary-output-file: file I/O not supported in browser');
        }
        this._filename = filename;
        try {
            fs.writeFileSync(filename, new Uint8Array(0));
        } catch (e) {
            throw new Error(`open-binary-output-file: cannot open file ${filename}: ${e.message}`);
        }
    }

    /**
     * Appends the bytes written since the last flush to the file.
     */
    flush() {
        if (this._buffer.length > 0) {
            fs.appendFileSync(this._filename, Uint8Array.from(this._buffer));
            this._buffer = [];
        }
    }

    /**
     * Closes the port, writing what remains.
     */
    close() {
        if (this._open) {
            this.flush();
            this._open = false;
        }
    }

    toString() {
        return `#<binary-file-output-port:${this._filename}:${this._open ? 'open' : 'closed'}>`;
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

