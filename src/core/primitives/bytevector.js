/**
 * Bytevector Primitives for Scheme.
 * 
 * Provides bytevector operations per R7RS §6.9.
 * Bytevectors are represented as Uint8Array instances.
 */

import {
    assertArity,
    assertInteger,
    assertString
} from '../interpreter/type_check.js';
import { SchemeTypeError, SchemeRangeError } from '../interpreter/errors.js';

// =============================================================================
// Type Predicates and Assertions
// =============================================================================

/**
 * Checks if value is a bytevector (Uint8Array).
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isBytevector(x) {
    return x instanceof Uint8Array;
}

/**
 * Asserts value is a bytevector.
 * @param {string} procName - Procedure name
 * @param {number} argPos - Argument position (1-indexed)
 * @param {*} value - Value to check
 * @returns {Uint8Array} The bytevector
 * @throws {SchemeTypeError}
 */
function assertBytevector(procName, argPos, value) {
    if (!isBytevector(value)) {
        throw new SchemeTypeError(procName, argPos, 'bytevector', value);
    }
    return value;
}

/**
 * Asserts value is a byte (integer 0-255).
 * @param {string} procName - Procedure name
 * @param {number} argPos - Argument position (1-indexed)
 * @param {*} value - Value to check
 * @returns {number} The byte
 * @throws {SchemeTypeError|SchemeRangeError}
 */
function assertByte(procName, argPos, value) {
    assertInteger(procName, argPos, value);
    if (value < 0 || value > 255) {
        throw new SchemeRangeError(procName, 'byte', 0, 255, value);
    }
    return value;
}

/**
 * Validates and returns start/end range for bytevector operations.
 * @param {string} procName - Procedure name
 * @param {Uint8Array} bv - The bytevector
 * @param {number|undefined} start - Start index (default 0)
 * @param {number|undefined} end - End index (default bv.length)
 * @returns {[number, number]} Validated [start, end]
 */
function validateRange(procName, bv, start, end) {
    const s = start === undefined ? 0 : start;
    const e = end === undefined ? bv.length : end;

    if (!Number.isInteger(s) || s < 0 || s > bv.length) {
        throw new SchemeRangeError(procName, 'start', 0, bv.length, s);
    }
    if (!Number.isInteger(e) || e < s || e > bv.length) {
        throw new SchemeRangeError(procName, 'end', s, bv.length, e);
    }
    return [s, e];
}

// =============================================================================
// Bytevector Primitives
// =============================================================================

/**
 * Bytevector primitives exported to Scheme.
 */
export const bytevectorPrimitives = {
    // -------------------------------------------------------------------------
    // Type Predicates
    // -------------------------------------------------------------------------

    /**
     * Bytevector type predicate.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a bytevector.
     */
    'bytevector?': (obj) => isBytevector(obj),

    // -------------------------------------------------------------------------
    // Constructors
    // -------------------------------------------------------------------------

    /**
     * Creates a bytevector from byte arguments.
     * @param {...number} bytes - Bytes (0-255)
     * @returns {Uint8Array} New bytevector
     */
    'bytevector': (...bytes) => {
        bytes.forEach((b, i) => assertByte('bytevector', i + 1, b));
        return new Uint8Array(bytes);
    },

    /**
     * Creates a bytevector of given length.
     * @param {number} k - Length
     * @param {number} [fill=0] - Fill byte (default 0)
     * @returns {Uint8Array} New bytevector
     */
    'make-bytevector': (k, fill = 0) => {
        assertInteger('make-bytevector', 1, k);
        if (k < 0) {
            throw new SchemeRangeError('make-bytevector', 'length', 0, Infinity, k);
        }
        assertByte('make-bytevector', 2, fill);
        const bv = new Uint8Array(k);
        if (fill !== 0) {
            bv.fill(fill);
        }
        return bv;
    },

    // -------------------------------------------------------------------------
    // Accessors
    // -------------------------------------------------------------------------

    /**
     * Returns the length of a bytevector.
     * @param {Uint8Array} bv - A bytevector.
     * @returns {number} Length.
     */
    'bytevector-length': (bv) => {
        assertBytevector('bytevector-length', 1, bv);
        return bv.length;
    },

    /**
     * Returns byte at index.
     * @param {Uint8Array} bv - A bytevector.
     * @param {number} k - Index.
     * @returns {number} Byte value (0-255).
     */
    'bytevector-u8-ref': (bv, k) => {
        assertBytevector('bytevector-u8-ref', 1, bv);
        assertInteger('bytevector-u8-ref', 2, k);
        if (k < 0 || k >= bv.length) {
            throw new SchemeRangeError('bytevector-u8-ref', 'index', 0, bv.length - 1, k);
        }
        return bv[k];
    },

    /**
     * Sets byte at index.
     * @param {Uint8Array} bv - A bytevector.
     * @param {number} k - Index.
     * @param {number} byte - Byte value (0-255).
     * @returns {undefined}
     */
    'bytevector-u8-set!': (bv, k, byte) => {
        assertBytevector('bytevector-u8-set!', 1, bv);
        assertInteger('bytevector-u8-set!', 2, k);
        if (k < 0 || k >= bv.length) {
            throw new SchemeRangeError('bytevector-u8-set!', 'index', 0, bv.length - 1, k);
        }
        assertByte('bytevector-u8-set!', 3, byte);
        bv[k] = byte;
        return undefined;
    },

    // -------------------------------------------------------------------------
    // Copy Operations
    // -------------------------------------------------------------------------

    /**
     * Returns a copy of a bytevector.
     * @param {Uint8Array} bv - Source bytevector
     * @param {number} [start=0] - Start index
     * @param {number} [end=length] - End index
     * @returns {Uint8Array} New bytevector
     */
    'bytevector-copy': (bv, start, end) => {
        assertBytevector('bytevector-copy', 1, bv);
        const [s, e] = validateRange('bytevector-copy', bv, start, end);
        return bv.slice(s, e);
    },

    /**
     * Copies bytes from source to destination bytevector.
     * @param {Uint8Array} to - Destination bytevector
     * @param {number} at - Destination start index
     * @param {Uint8Array} from - Source bytevector
     * @param {number} [start=0] - Source start index
     * @param {number} [end=length] - Source end index
     * @returns {undefined}
     */
    'bytevector-copy!': (to, at, from, start, end) => {
        assertBytevector('bytevector-copy!', 1, to);
        assertInteger('bytevector-copy!', 2, at);
        assertBytevector('bytevector-copy!', 3, from);

        if (at < 0 || at > to.length) {
            throw new SchemeRangeError('bytevector-copy!', 'at', 0, to.length, at);
        }

        const [s, e] = validateRange('bytevector-copy!', from, start, end);
        const count = e - s;

        if (at + count > to.length) {
            throw new SchemeRangeError('bytevector-copy!', 'destination', 0, to.length - at, count);
        }

        // Handle overlapping regions by copying to temp first if needed
        if (to === from && at > s && at < e) {
            // Overlapping forward copy - need temp buffer
            const temp = from.slice(s, e);
            to.set(temp, at);
        } else {
            // Safe direct copy
            to.set(from.subarray(s, e), at);
        }

        return undefined;
    },

    /**
     * Appends bytevectors.
     * @param {...Uint8Array} bvs - Bytevectors to append
     * @returns {Uint8Array} New concatenated bytevector
     */
    'bytevector-append': (...bvs) => {
        bvs.forEach((bv, i) => assertBytevector('bytevector-append', i + 1, bv));
        const totalLength = bvs.reduce((acc, bv) => acc + bv.length, 0);
        const result = new Uint8Array(totalLength);
        let offset = 0;
        for (const bv of bvs) {
            result.set(bv, offset);
            offset += bv.length;
        }
        return result;
    },

    // -------------------------------------------------------------------------
    // String Conversion
    // -------------------------------------------------------------------------

    /**
     * Decodes a bytevector as UTF-8 to a string.
     * @param {Uint8Array} bv - Source bytevector
     * @param {number} [start=0] - Start index
     * @param {number} [end=length] - End index
     * @returns {string} Decoded string
     */
    'utf8->string': (bv, start, end) => {
        assertBytevector('utf8->string', 1, bv);
        const [s, e] = validateRange('utf8->string', bv, start, end);
        const decoder = new TextDecoder('utf-8');
        return decoder.decode(bv.subarray(s, e));
    },

    /**
     * Encodes a string as UTF-8 to a bytevector.
     * @param {string} str - Source string
     * @param {number} [start=0] - Start index
     * @param {number} [end=length] - End index
     * @returns {Uint8Array} Encoded bytevector
     */
    'string->utf8': (str, start, end) => {
        assertString('string->utf8', 1, str);
        const s = start === undefined ? 0 : start;
        const e = end === undefined ? str.length : end;

        if (!Number.isInteger(s) || s < 0 || s > str.length) {
            throw new SchemeRangeError('string->utf8', 'start', 0, str.length, s);
        }
        if (!Number.isInteger(e) || e < s || e > str.length) {
            throw new SchemeRangeError('string->utf8', 'end', s, str.length, e);
        }

        const encoder = new TextEncoder();
        return encoder.encode(str.slice(s, e));
    }
};

// Mark primitives that should receive raw Scheme objects (no JS bridge wrapping)
bytevectorPrimitives['bytevector'].skipBridge = true;
bytevectorPrimitives['make-bytevector'].skipBridge = true;
bytevectorPrimitives['bytevector-u8-set!'].skipBridge = true;
bytevectorPrimitives['bytevector-u8-ref'].skipBridge = true;
bytevectorPrimitives['bytevector-copy!'].skipBridge = true;
