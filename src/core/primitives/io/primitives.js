import {
    Port, EOF_OBJECT,
    isPort, isInputPort, isOutputPort,
    requireOpenInputPort, requireOpenOutputPort
} from './ports.js';
import { StringInputPort, StringOutputPort } from './string_port.js';
import { BytevectorInputPort, BytevectorOutputPort } from './bytevector_port.js';
import { FileInputPort, FileOutputPort, fileExists, deleteFile } from './file_port.js';
import { ConsoleOutputPort } from './console_port.js';
import { displayString, writeString, writeStringShared } from './printer.js';
import { readExpressionFromPort } from './reader_bridge.js';
import { list } from '../../interpreter/cons.js';
import { intern } from '../../interpreter/symbol.js';
import { Char } from '../char_class.js';

// ============================================================================
// Current Ports (Global State)
// ============================================================================

// Default ports - console-based
let currentInputPort = null;  // We'll use a placeholder for input
let currentOutputPort = new ConsoleOutputPort('stdout');
let currentErrorPort = new ConsoleOutputPort('stderr');

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
    // Current Ports
    // --------------------------------------------------------------------------

    'current-input-port': () => {
        if (!currentInputPort) {
            // Create a dummy input port that returns EOF
            currentInputPort = new StringInputPort('');
        }
        return currentInputPort;
    },

    'current-output-port': () => currentOutputPort,
    'current-error-port': () => currentErrorPort,

    // --------------------------------------------------------------------------
    // String Ports
    // --------------------------------------------------------------------------

    'open-input-string': (str) => {
        if (typeof str !== 'string') throw new Error('open-input-string: expected string');
        return new StringInputPort(str);
    },

    'open-output-string': () => new StringOutputPort(),

    'get-output-string': (port) => {
        if (!(port instanceof StringOutputPort)) throw new Error('get-output-string: expected string output port');
        return port.getString();
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
        if (typeof filename !== 'string') throw new Error('open-input-file: expected string');
        return new FileInputPort(filename);
    },

    'open-output-file': (filename) => {
        if (typeof filename !== 'string') throw new Error('open-output-file: expected string');
        return new FileOutputPort(filename);
    },

    'file-exists?': (filename) => {
        if (typeof filename !== 'string') throw new Error('file-exists?: expected string');
        return fileExists(filename);
    },

    'delete-file': (filename) => {
        if (typeof filename !== 'string') throw new Error('delete-file: expected string');
        deleteFile(filename);
        return undefined;
    },

    'call-with-input-file': (filename, proc) => {
        if (typeof filename !== 'string') throw new Error('call-with-input-file: expected string');
        if (typeof proc !== 'function') throw new Error('call-with-input-file: expected procedure');
        const port = new FileInputPort(filename);
        try {
            return proc(port);
        } finally {
            if (port.isOpen) port.close();
        }
    },

    'call-with-output-file': (filename, proc) => {
        if (typeof filename !== 'string') throw new Error('call-with-output-file: expected string');
        if (typeof proc !== 'function') throw new Error('call-with-output-file: expected procedure');
        const port = new FileOutputPort(filename);
        try {
            return proc(port);
        } finally {
            if (port.isOpen) port.close();
        }
    },

    'with-input-from-file': (filename, thunk) => {
        if (typeof filename !== 'string') throw new Error('with-input-from-file: expected string');
        if (typeof thunk !== 'function') throw new Error('with-input-from-file: expected procedure');
        const port = new FileInputPort(filename);
        const old = currentInputPort;
        currentInputPort = port;
        try {
            return thunk();
        } finally {
            currentInputPort = old;
            if (port.isOpen) port.close();
        }
    },

    'with-output-to-file': (filename, thunk) => {
        if (typeof filename !== 'string') throw new Error('with-output-to-file: expected string');
        if (typeof thunk !== 'function') throw new Error('with-output-to-file: expected procedure');
        const port = new FileOutputPort(filename);
        const old = currentOutputPort;
        currentOutputPort = port;
        try {
            return thunk();
        } finally {
            currentOutputPort = old;
            if (port.isOpen) port.close();
        }
    },

    'features': () => {
        return list(
            intern('r7rs'),
            intern('ieee-float'),
            intern('full-unicode'),
            intern('scheme-js')
        );
    },

    // --------------------------------------------------------------------------
    // EOF
    // --------------------------------------------------------------------------

    'eof-object': () => EOF_OBJECT,
    'eof-object?': (obj) => obj === EOF_OBJECT,

    // --------------------------------------------------------------------------
    // Input Operations
    // --------------------------------------------------------------------------

    'read-char': (...args) => {
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        requireOpenInputPort(port, 'read-char');
        if (port.readChar) return port.readChar();
        throw new Error('read-char: unsupported port type');
    },

    'peek-char': (...args) => {
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        requireOpenInputPort(port, 'peek-char');
        if (port.peekChar) return port.peekChar();
        throw new Error('peek-char: unsupported port type');
    },

    'char-ready?': (...args) => {
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        if (!isInputPort(port)) throw new Error('char-ready?: expected input port');
        if (!port.isOpen) return false;
        if (port.charReady) return port.charReady();
        return false;
    },

    'read-line': (...args) => {
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        requireOpenInputPort(port, 'read-line');
        if (port.readLine) return port.readLine();
        throw new Error('read-line: unsupported port type');
    },

    'read-string': (k, ...args) => {
        // Handle BigInt k by converting to Number
        if (typeof k === 'bigint') k = Number(k);
        if (typeof k !== 'number' || !Number.isInteger(k) || k < 0) throw new Error('read-string: expected non-negative integer');
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        requireOpenInputPort(port, 'read-string');
        if (port.readString) return port.readString(k);
        throw new Error('read-string: unsupported port type');
    },

    // --------------------------------------------------------------------------
    // Binary Input
    // --------------------------------------------------------------------------

    'read-u8': (...args) => {
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        requireOpenInputPort(port, 'read-u8');
        if (port.readU8) {
            const b = port.readU8();
            return b === EOF_OBJECT ? b : BigInt(b);
        }
        throw new Error('read-u8: expected binary input port');
    },

    'peek-u8': (...args) => {
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        requireOpenInputPort(port, 'peek-u8');
        if (port.peekU8) {
            const b = port.peekU8();
            return b === EOF_OBJECT ? b : BigInt(b);
        }
        throw new Error('peek-u8: expected binary input port');
    },

    'u8-ready?': (...args) => {
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        if (!isInputPort(port)) throw new Error('u8-ready?: expected input port');
        if (!port.isOpen) return false;
        if (port.u8Ready) return port.u8Ready();
        return false;
    },

    'read-bytevector': (k, ...args) => {
        // Handle BigInt k by converting to Number
        if (typeof k === 'bigint') k = Number(k);
        if (typeof k !== 'number' || !Number.isInteger(k) || k < 0) throw new Error('read-bytevector: expected non-negative integer');
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        requireOpenInputPort(port, 'read-bytevector');
        if (port.readBytevector) return port.readBytevector(k);
        throw new Error('read-bytevector: expected binary input port');
    },

    // --------------------------------------------------------------------------
    // Output Operations
    // --------------------------------------------------------------------------

    'write-char': (char, ...args) => {
        // Accept Char objects or single-character strings
        let charStr;
        if (char instanceof Char) {
            charStr = char.toString();
        } else if (typeof char === 'string' && char.length === 1) {
            charStr = char;
        } else {
            throw new Error('write-char: expected character');
        }
        const port = args.length > 0 ? args[0] : currentOutputPort;
        requireOpenOutputPort(port, 'write-char');
        port.writeChar(charStr);
        return undefined;
    },

    'write-string': (str, ...args) => {
        if (typeof str !== 'string') throw new Error('write-string: expected string');
        let port = currentOutputPort;
        let start = 0;
        let end = str.length;

        if (args.length >= 1 && isOutputPort(args[0])) {
            port = args[0];
            if (args.length >= 2) start = Number(args[1]);
            if (args.length >= 3) end = Number(args[2]);
        } else if (args.length >= 1) {
            start = Number(args[0]);
            if (args.length >= 2) end = Number(args[1]);
        }

        requireOpenOutputPort(port, 'write-string');
        port.writeString(str, start, end);
        return undefined;
    },

    // --------------------------------------------------------------------------
    // Binary Output
    // --------------------------------------------------------------------------

    'write-u8': (byte, ...args) => {
        // Handle BigInt byte by converting to Number
        if (typeof byte === 'bigint') byte = Number(byte);
        if (!Number.isInteger(byte) || byte < 0 || byte > 255) throw new Error('write-u8: expected byte (0-255)');
        const port = args.length > 0 ? args[0] : currentOutputPort;
        requireOpenOutputPort(port, 'write-u8');
        if (port.writeU8) {
            port.writeU8(byte);
            return undefined;
        }
        throw new Error('write-u8: expected binary output port');
    },

    'write-bytevector': (bv, ...args) => {
        if (!(bv instanceof Uint8Array)) throw new Error('write-bytevector: expected bytevector');
        let port = currentOutputPort;
        let start = 0;
        let end = bv.length;

        if (args.length >= 1 && isOutputPort(args[0])) {
            port = args[0];
            if (args.length >= 2) start = args[1];
            if (args.length >= 3) end = args[2];
        } else if (args.length >= 1) {
            start = args[0];
            if (args.length >= 2) end = args[1];
        }

        requireOpenOutputPort(port, 'write-bytevector');
        if (port.writeBytevector) {
            port.writeBytevector(bv, start, end);
            return undefined;
        }
        throw new Error('write-bytevector: expected binary output port');
    },

    'newline': (...args) => {
        const port = args.length > 0 ? args[0] : currentOutputPort;
        requireOpenOutputPort(port, 'newline');
        port.writeChar('\n');
        return undefined;
    },

    'display': (val, ...args) => {
        const port = args.length > 0 ? args[0] : currentOutputPort;
        requireOpenOutputPort(port, 'display');
        const str = displayString(val);
        port.writeString(str);
        return undefined;
    },

    'write': (val, ...args) => {
        const port = args.length > 0 ? args[0] : currentOutputPort;
        requireOpenOutputPort(port, 'write');
        const str = writeString(val);
        port.writeString(str);
        return undefined;
    },

    'write-simple': (val, ...args) => {
        const port = args.length > 0 ? args[0] : currentOutputPort;
        requireOpenOutputPort(port, 'write-simple');
        const str = writeString(val);
        port.writeString(str);
        return undefined;
    },

    'write-shared': (val, ...args) => {
        const port = args.length > 0 ? args[0] : currentOutputPort;
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

    'flush-output-port': (...args) => {
        const port = args.length > 0 ? args[0] : currentOutputPort;
        if (!isOutputPort(port)) throw new Error('flush-output-port: expected output port');
        if (port.flush) port.flush();
        return undefined;
    },

    // --------------------------------------------------------------------------
    // Read
    // --------------------------------------------------------------------------

    'read': (...args) => {
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        requireOpenInputPort(port, 'read');
        return readExpressionFromPort(port);
    }
};
