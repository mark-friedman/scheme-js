import { Port, EOF_OBJECT } from './ports.js';

/**
 * Bytevector input port - reads bytes from a bytevector.
 */
export class BytevectorInputPort extends Port {
    /**
     * @param {Uint8Array} bytevector - The bytevector to read from.
     */
    constructor(bytevector) {
        super('input');
        this._bytevector = bytevector;
        this._pos = 0;
    }

    /** @returns {boolean} Whether this is a binary port. */
    get isBinary() { return true; }

    /** @returns {boolean} Whether this is a textual port. */
    get isTextual() { return false; }

    /** @returns {boolean} Whether there's more to read. */
    hasMore() { return this._pos < this._bytevector.length; }

    /**
     * Reads the next byte.
     * @returns {number|object} The byte (0-255) or EOF_OBJECT.
     */
    readU8() {
        if (!this._open) {
            throw new Error('read-u8: port is closed');
        }
        if (this._pos >= this._bytevector.length) {
            return EOF_OBJECT;
        }
        return this._bytevector[this._pos++];
    }

    /**
     * Peeks at the next byte without consuming it.
     * @returns {number|object} The byte or EOF_OBJECT.
     */
    peekU8() {
        if (!this._open) {
            throw new Error('peek-u8: port is closed');
        }
        if (this._pos >= this._bytevector.length) {
            return EOF_OBJECT;
        }
        return this._bytevector[this._pos];
    }

    /**
     * Checks if a byte is ready.
     * @returns {boolean} Always true for bytevector ports.
     */
    u8Ready() {
        return this._open && this._pos < this._bytevector.length;
    }

    /**
     * Reads up to k bytes into a new bytevector.
     * @param {number} k - Maximum bytes to read.
     * @returns {Uint8Array|object} Bytevector or EOF_OBJECT.
     */
    readBytevector(k) {
        if (!this._open) {
            throw new Error('read-bytevector: port is closed');
        }
        if (this._pos >= this._bytevector.length) {
            return EOF_OBJECT;
        }
        const end = Math.min(this._pos + k, this._bytevector.length);
        const result = this._bytevector.slice(this._pos, end);
        this._pos = end;
        return result;
    }

    toString() {
        return `#<bytevector-input-port:${this._open ? 'open' : 'closed'}>`;
    }
}

/**
 * Bytevector output port - writes bytes to an accumulator.
 */
export class BytevectorOutputPort extends Port {
    constructor() {
        super('output');
        this._buffer = [];
    }

    /** @returns {boolean} Whether this is a binary port. */
    get isBinary() { return true; }

    /** @returns {boolean} Whether this is a textual port. */
    get isTextual() { return false; }

    /**
     * Writes a byte.
     * @param {number} byte - The byte to write (0-255).
     */
    writeU8(byte) {
        if (!this._open) {
            throw new Error('write-u8: port is closed');
        }
        if (!Number.isInteger(byte) || byte < 0 || byte > 255) {
            throw new Error('write-u8: expected byte (0-255)');
        }
        this._buffer.push(byte);
    }

    /**
     * Writes bytes from a bytevector.
     * @param {Uint8Array} bv - Bytevector to write from.
     * @param {number} [start] - Start index.
     * @param {number} [end] - End index.
     */
    writeBytevector(bv, start = 0, end = bv.length) {
        if (!this._open) {
            throw new Error('write-bytevector: port is closed');
        }
        for (let i = start; i < end; i++) {
            this._buffer.push(bv[i]);
        }
    }

    /**
     * Gets the accumulated bytevector.
     * @returns {Uint8Array} The output bytevector.
     */
    getBytevector() {
        return new Uint8Array(this._buffer);
    }

    toString() {
        return `#<bytevector-output-port:${this._open ? 'open' : 'closed'}>`;
    }
}
