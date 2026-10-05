import {
    Port, EOF_OBJECT,
    isPort, isInputPort, isOutputPort,
    requireOpenInputPort, requireOpenOutputPort
} from './ports.js';
import { StringInputPort, StringOutputPort } from './string_port.js';
import { BytevectorInputPort, BytevectorOutputPort } from './bytevector_port.js';
import {
    FileInputPort, FileOutputPort, BinaryFileInputPort, BinaryFileOutputPort, fileExists, deleteFile
} from './file_port.js';
import { ConsoleOutputPort } from './console_port.js';
import { standardInputPort } from './stdin_port.js';
import { standardOutputPort, standardErrorPort } from './stdout_port.js';
import { displayString, writeString, writeStringShared, writeStringSimple } from './printer.js';
import { systemLibrary } from '../../interpreter/library_seed.js';
import { callSchemeProcedure } from '../../interpreter/values.js';
import { callLibrarySystem, currentLibraryRegistry } from '../../interpreter/library_registry.js';
import { assertArity } from '../../interpreter/type_check.js';
import { Char } from '../char_class.js';
import { isString, stringValue, freshString } from '../string_class.js';

/**
 * A string argument as a JavaScript string, a mutable Scheme string's
 * characters included.
 * @param {*} value - The argument.
 * @param {string} procName - The procedure, for the error.
 * @returns {string} Its characters.
 * @throws {Error} If it is not a string.
 */
function textOf(value, procName) {
    if (!isString(value)) throw new Error(`${procName}: expected string`);
    return stringValue(value);
}

/**
 * What a read of a string returns, newly allocated and so mutable: a string
 * becomes a `SchemeString`, and the end-of-file object stays as it is.
 * @param {*} result - What the port returned.
 * @returns {*} The result.
 */
function freshRead(result) {
    return typeof result === 'string' ? freshString(result) : result;
}

/**
 * What a read of a character returns: a character becomes a `Char`, and the
 * end-of-file object stays as it is.
 * @param {string|object} result - What the port returned: the character's one
 *   or two code units, or the end-of-file object.
 * @returns {Char|object} The result.
 */
function charRead(result) {
    return typeof result === 'string' ? new Char(result.codePointAt(0)) : result;
}

// ============================================================================
// The Console Ports
// ============================================================================

// What the current ports are to begin with (`current-input-port` and the
// others are parameter objects, in `src/core/scheme/ports.scm`): the console,
// and for input an empty port, since a page has no standard input. One of each
// for the whole process, so that every interpreter's current ports, and every
// library instance's, start on the same console.
const consoleInputPort = new StringInputPort('');
const consoleOutputPort = new ConsoleOutputPort('stdout');
const consoleErrorPort = new ConsoleOutputPort('stderr');

// ============================================================================
// I/O Primitives
// ============================================================================

/**
 * I/O primitives exported to Scheme.
 */
export const ioPrimitives = {
    // --------------------------------------------------------------------------
    // Port Predicates
    // --------------------------------------------------------------------------

    'port?': (obj) => isPort(obj),
    'input-port?': (obj) => isInputPort(obj),
    'output-port?': (obj) => isOutputPort(obj),
    'textual-port?': (obj) => isPort(obj) && obj.isTextual,
    'binary-port?': (obj) => isPort(obj) && obj.isBinary,

    'input-port-open?': (port) => {
        if (!isInputPort(port)) throw new Error('input-port-open?: expected input port');
        return port.isOpen;
    },

    'output-port-open?': (port) => {
        if (!isOutputPort(port)) throw new Error('output-port-open?: expected output port');
        return port.isOpen;
    },

    // --------------------------------------------------------------------------
    // The Console Ports, which the current ports begin as
    // --------------------------------------------------------------------------

    '%console-input-port': () => consoleInputPort,
    '%console-output-port': () => consoleOutputPort,
    '%console-error-port': () => consoleErrorPort,

    // The process's own standard input, output and error (Node.js only), which
    // the CLI makes the current ports of a program it runs.
    'standard-input-port': () => standardInputPort(),
    'standard-output-port': () => standardOutputPort(),
    'standard-error-port': () => standardErrorPort(),

    // --------------------------------------------------------------------------
    // String Ports
    // --------------------------------------------------------------------------

    'open-input-string': (str) => {
        return new StringInputPort(textOf(str, 'open-input-string'));
    },

    'open-output-string': () => new StringOutputPort(),

    'get-output-string': (port) => {
        if (!(port instanceof StringOutputPort)) throw new Error('get-output-string: expected string output port');
        return freshString(port.getString());
    },

    // --------------------------------------------------------------------------
    // Bytevector Ports (Binary I/O)
    // --------------------------------------------------------------------------

    'open-input-bytevector': (bv) => {
        if (!(bv instanceof Uint8Array)) throw new Error('open-input-bytevector: expected bytevector');
        return new BytevectorInputPort(bv);
    },

    'open-output-bytevector': () => new BytevectorOutputPort(),

    'get-output-bytevector': (port) => {
        if (!(port instanceof BytevectorOutputPort)) throw new Error('get-output-bytevector: expected bytevector output port');
        return port.getBytevector();
    },

    // --------------------------------------------------------------------------
    // File Ports
    // --------------------------------------------------------------------------

    'open-input-file': (filename) => {
        filename = textOf(filename, 'open-input-file');
        return new FileInputPort(filename);
    },

    'open-output-file': (filename) => {
        filename = textOf(filename, 'open-output-file');
        return new FileOutputPort(filename);
    },

    'open-binary-input-file': (filename) => {
        filename = textOf(filename, 'open-binary-input-file');
        return new BinaryFileInputPort(filename);
    },

    'open-binary-output-file': (filename) => {
        filename = textOf(filename, 'open-binary-output-file');
        return new BinaryFileOutputPort(filename);
    },

    'file-exists?': (filename) => {
        filename = textOf(filename, 'file-exists?');
        return fileExists(filename);
    },

    'delete-file': (filename) => {
        filename = textOf(filename, 'delete-file');
        deleteFile(filename);
        return undefined;
    },

    // The features are the library system's, in the current registry: what
    // `cond-expand` finds, `node` or `browser` and any a host added included.
    'features': (...args) => {
        assertArity('features', args, 0, 0);
        return callLibrarySystem('registry-feature-list', currentLibraryRegistry());
    },

    // --------------------------------------------------------------------------
    // EOF
    // --------------------------------------------------------------------------

    'eof-object': () => EOF_OBJECT,
    'eof-object?': (obj) => obj === EOF_OBJECT,

    // --------------------------------------------------------------------------
    // Input Operations
    // --------------------------------------------------------------------------

    '%read-char': (port) => {
        requireOpenInputPort(port, 'read-char');
        if (port.readChar) return charRead(port.readChar());
        throw new Error('read-char: unsupported port type');
    },

    '%peek-char': (port) => {
        requireOpenInputPort(port, 'peek-char');
        if (port.peekChar) return charRead(port.peekChar());
        throw new Error('peek-char: unsupported port type');
    },

    '%char-ready?': (port) => {
        if (!isInputPort(port)) throw new Error('char-ready?: expected input port');
        if (!port.isOpen) return false;
        if (port.charReady) return port.charReady();
        return false;
    },

    '%read-line': (port) => {
        requireOpenInputPort(port, 'read-line');
        if (port.readLine) return freshRead(port.readLine());
        throw new Error('read-line: unsupported port type');
    },

    '%read-string': (k, port) => {
        // Handle BigInt k by converting to Number
        if (typeof k === 'bigint') k = Number(k);
        if (typeof k !== 'number' || !Number.isInteger(k) || k < 0) throw new Error('read-string: expected non-negative integer');
        requireOpenInputPort(port, 'read-string');
        if (port.readString) return freshRead(port.readString(k));
        throw new Error('read-string: unsupported port type');
    },

    // --------------------------------------------------------------------------
    // Binary Input
    // --------------------------------------------------------------------------

    '%read-u8': (port) => {
        requireOpenInputPort(port, 'read-u8');
        if (port.readU8) {
            const b = port.readU8();
            return b;
        }
        throw new Error('read-u8: expected binary input port');
    },

    '%peek-u8': (port) => {
        requireOpenInputPort(port, 'peek-u8');
        if (port.peekU8) {
            const b = port.peekU8();
            return b;
        }
        throw new Error('peek-u8: expected binary input port');
    },

    '%u8-ready?': (port) => {
        if (!isInputPort(port)) throw new Error('u8-ready?: expected input port');
        if (!port.isOpen) return false;
        if (port.u8Ready) return port.u8Ready();
        return false;
    },

    '%read-bytevector': (k, port) => {
        // Handle BigInt k by converting to Number
        if (typeof k === 'bigint') k = Number(k);
        if (typeof k !== 'number' || !Number.isInteger(k) || k < 0) throw new Error('read-bytevector: expected non-negative integer');
        requireOpenInputPort(port, 'read-bytevector');
        if (port.readBytevector) return port.readBytevector(k);
        throw new Error('read-bytevector: expected binary input port');
    },

    // --------------------------------------------------------------------------
    // Output Operations
    // --------------------------------------------------------------------------

    '%write-char': (char, port) => {
        // Accept Char objects or single-character strings
        let charStr;
        if (char instanceof Char) {
            charStr = char.toString();
        } else if (typeof char === 'string' && char.length === 1) {
            charStr = char;
        } else {
            throw new Error('write-char: expected character');
        }
        requireOpenOutputPort(port, 'write-char');
        port.writeChar(charStr);
        return undefined;
    },

    '%write-string': (str, port, start, end) => {
        str = textOf(str, 'write-string');
        start = start === undefined ? 0 : Number(start);
        end = end === undefined ? str.length : Number(end);
        requireOpenOutputPort(port, 'write-string');
        port.writeString(str, start, end);
        return undefined;
    },

    // --------------------------------------------------------------------------
    // Binary Output
    // --------------------------------------------------------------------------

    '%write-u8': (byte, port) => {
        // Handle BigInt byte by converting to Number
        if (typeof byte === 'bigint') byte = Number(byte);
        if (!Number.isInteger(byte) || byte < 0 || byte > 255) throw new Error('write-u8: expected byte (0-255)');
        requireOpenOutputPort(port, 'write-u8');
        if (port.writeU8) {
            port.writeU8(byte);
            return undefined;
        }
        throw new Error('write-u8: expected binary output port');
    },

    '%write-bytevector': (bv, port, start, end) => {
        if (!(bv instanceof Uint8Array)) throw new Error('write-bytevector: expected bytevector');
        start = start === undefined ? 0 : start;
        end = end === undefined ? bv.length : end;
        requireOpenOutputPort(port, 'write-bytevector');
        if (port.writeBytevector) {
            port.writeBytevector(bv, start, end);
            return undefined;
        }
        throw new Error('write-bytevector: expected binary output port');
    },

    '%newline': (port) => {
        requireOpenOutputPort(port, 'newline');
        port.writeChar('\n');
        return undefined;
    },

    '%display': (val, port) => {
        requireOpenOutputPort(port, 'display');
        const str = displayString(val);
        port.writeString(str);
        return undefined;
    },

    '%write': (val, port) => {
        requireOpenOutputPort(port, 'write');
        const str = writeString(val);
        port.writeString(str);
        return undefined;
    },

    '%write-simple': (val, port) => {
        requireOpenOutputPort(port, 'write-simple');
        const str = writeStringSimple(val);
        port.writeString(str);
        return undefined;
    },

    '%write-shared': (val, port) => {
        requireOpenOutputPort(port, 'write-shared');
        const str = writeStringShared(val);
        port.writeString(str);
        return undefined;
    },

    // --------------------------------------------------------------------------
    // Port Control
    // --------------------------------------------------------------------------

    'close-port': (port) => {
        if (!isPort(port)) throw new Error('close-port: expected port');
        port.close();
        return undefined;
    },

    'close-input-port': (port) => {
        if (!isInputPort(port)) throw new Error('close-input-port: expected input port');
        port.close();
        return undefined;
    },

    'close-output-port': (port) => {
        if (!isOutputPort(port)) throw new Error('close-output-port: expected output port');
        port.close();
        return undefined;
    },

    '%flush-output-port': (port) => {
        if (!isOutputPort(port)) throw new Error('flush-output-port: expected output port');
        if (port.flush) port.flush();
        return undefined;
    },

    // --------------------------------------------------------------------------
    // Read
    // --------------------------------------------------------------------------

    // The reader's door for `read`: a datum from a port, read by
    // `(scheme-js reader)` (`read-from-port` in reader.scm).
    '%read': (port) => {
        requireOpenInputPort(port, 'read');
        return callSchemeProcedure(systemLibrary(['scheme-js', 'reader']).get('read-from-port'), [port]);
    }
};
