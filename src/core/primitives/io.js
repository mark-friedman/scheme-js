/**
 * I/O Primitives for Scheme.
 * 
 * Implements R7RS §6.13 port system with textual string ports.
 * Binary ports and file I/O are deferred to later phases.
 */

import { Cons, list } from '../interpreter/cons.js';

// ============================================================================
// EOF Object
// ============================================================================

/**
 * Unique EOF object.
 * In R7RS, (eof-object) returns a unique object that satisfies eof-object?.
 */
const EOF_OBJECT = Object.freeze({ type: 'eof-object', toString: () => '#<eof>' });

// ============================================================================
// Port Classes
// ============================================================================

/**
 * Base class for all ports.
 */
class Port {
    /**
     * @param {string} direction - 'input', 'output', or 'input/output'
     */
    constructor(direction) {
        this.direction = direction;
        this._open = true;
    }

    /** @returns {boolean} Whether this port is open. */
    get isOpen() { return this._open; }

    /** @returns {boolean} Whether this is an input port. */
    get isInput() { return this.direction.includes('input'); }

    /** @returns {boolean} Whether this is an output port. */
    get isOutput() { return this.direction.includes('output'); }

    /** @returns {boolean} Whether this is a textual port. */
    get isTextual() { return true; }

    /** @returns {boolean} Whether this is a binary port. */
    get isBinary() { return false; }

    /** Closes the port. */
    close() { this._open = false; }

    toString() {
        const status = this._open ? 'open' : 'closed';
        return `#<${this.direction}-port:${status}>`;
    }
}

/**
 * String input port - reads from a string.
 */
class StringInputPort extends Port {
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
class StringOutputPort extends Port {
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

/**
 * Console output port - writes to console.log (for default ports).
 */
class ConsoleOutputPort extends Port {
    constructor(name = 'stdout') {
        super('output');
        this._name = name;
        this._lineBuffer = '';
    }

    writeChar(ch) {
        if (!this._open) {
            throw new Error('write-char: port is closed');
        }
        if (ch === '\n') {
            console.log(this._lineBuffer);
            this._lineBuffer = '';
        } else {
            this._lineBuffer += ch;
        }
    }

    writeString(str, start = 0, end = str.length) {
        if (!this._open) {
            throw new Error('write-string: port is closed');
        }
        const slice = str.slice(start, end);
        for (const ch of slice) {
            this.writeChar(ch);
        }
    }

    /**
     * Flushes any remaining buffered output.
     */
    flush() {
        if (this._lineBuffer.length > 0) {
            console.log(this._lineBuffer);
            this._lineBuffer = '';
        }
    }

    toString() {
        return `#<console-${this._name}-port>`;
    }
}

// ============================================================================
// Current Ports (Global State)
// ============================================================================

// Default ports - console-based
let currentInputPort = null;  // We'll use a placeholder for input
let currentOutputPort = new ConsoleOutputPort('stdout');
let currentErrorPort = new ConsoleOutputPort('stderr');

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Checks if a value is a Port.
 * @param {*} val
 * @returns {boolean}
 */
function isPort(val) {
    return val instanceof Port;
}

/**
 * Checks if a value is an input port.
 * @param {*} val
 * @returns {boolean}
 */
function isInputPort(val) {
    return val instanceof Port && val.isInput;
}

/**
 * Checks if a value is an output port.
 * @param {*} val
 * @returns {boolean}
 */
function isOutputPort(val) {
    return val instanceof Port && val.isOutput;
}

/**
 * Validates that a value is an open input port.
 * @param {*} port
 * @param {string} procName
 */
function requireOpenInputPort(port, procName) {
    if (!isInputPort(port)) {
        throw new Error(`${procName}: expected input port, got ${typeof port}`);
    }
    if (!port.isOpen) {
        throw new Error(`${procName}: port is closed`);
    }
}

/**
 * Validates that a value is an open output port.
 * @param {*} port
 * @param {string} procName
 */
function requireOpenOutputPort(port, procName) {
    if (!isOutputPort(port)) {
        throw new Error(`${procName}: expected output port, got ${typeof port}`);
    }
    if (!port.isOpen) {
        throw new Error(`${procName}: port is closed`);
    }
}

/**
 * Converts a Scheme value to its display representation.
 * @param {*} val
 * @returns {string}
 */
function displayString(val) {
    if (val === null) return '()';
    if (val === true) return '#t';
    if (val === false) return '#f';
    if (typeof val === 'string') return val;  // display doesn't quote strings
    if (typeof val === 'number') return String(val);
    if (val instanceof Cons) return consToString(val, displayString);
    if (Array.isArray(val)) return vectorToString(val, displayString);
    if (val && val.name && val.description === undefined) return val.name; // Symbol
    if (val === EOF_OBJECT) return '#<eof>';
    if (val instanceof Port) return val.toString();
    if (typeof val === 'function') return '#<procedure>';
    if (val && val.type === 'record') {
        return `#<${val.typeDescriptor.name}>`;
    }
    return String(val);
}

/**
 * Converts a Scheme value to its write (machine-readable) representation.
 * @param {*} val
 * @returns {string}
 */
function writeString(val) {
    if (val === null) return '()';
    if (val === true) return '#t';
    if (val === false) return '#f';
    if (typeof val === 'string') {
        // Escape and quote strings
        return '"' + val
            .replace(/\\/g, '\\\\')
            .replace(/"/g, '\\"')
            .replace(/\n/g, '\\n')
            .replace(/\r/g, '\\r')
            .replace(/\t/g, '\\t') + '"';
    }
    if (typeof val === 'number') return String(val);
    if (val instanceof Cons) return consToString(val, writeString);
    if (Array.isArray(val)) return vectorToString(val, writeString);
    if (val && val.name && val.description === undefined) return val.name; // Symbol
    if (val === EOF_OBJECT) return '#<eof>';
    if (val instanceof Port) return val.toString();
    if (typeof val === 'function') return '#<procedure>';
    if (val && typeof val === 'object' && val.type === 'char') {
        // Character representation
        const ch = val.value;
        if (ch === ' ') return '#\\space';
        if (ch === '\n') return '#\\newline';
        if (ch === '\t') return '#\\tab';
        if (ch === '\r') return '#\\return';
        return '#\\' + ch;
    }
    // If it's just a single character string representing a char
    if (typeof val === 'string' && val.length === 1) {
        // This is called from write context - but strings should be quoted
        // Character objects would be handled separately
        return '"' + val + '"';
    }
    if (val && val.type === 'record') {
        return `#<${val.typeDescriptor.name}>`;
    }
    return String(val);
}

/**
 * Converts a cons cell to a string.
 * @param {Cons} cons
 * @param {Function} elemFn - Function to convert elements.
 * @returns {string}
 */
function consToString(cons, elemFn) {
    const parts = [];
    let current = cons;
    while (current instanceof Cons) {
        parts.push(elemFn(current.car));
        current = current.cdr;
    }
    if (current === null) {
        return '(' + parts.join(' ') + ')';
    } else {
        // Improper list
        return '(' + parts.join(' ') + ' . ' + elemFn(current) + ')';
    }
}

/**
 * Converts a vector to a string.
 * @param {Array} vec
 * @param {Function} elemFn
 * @returns {string}
 */
function vectorToString(vec, elemFn) {
    return '#(' + vec.map(elemFn).join(' ') + ')';
}

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

    /**
     * Returns #t if obj is a port.
     * @param {*} obj
     * @returns {boolean}
     */
    'port?': (obj) => isPort(obj),

    /**
     * Returns #t if obj is an input port.
     * @param {*} obj
     * @returns {boolean}
     */
    'input-port?': (obj) => isInputPort(obj),

    /**
     * Returns #t if obj is an output port.
     * @param {*} obj
     * @returns {boolean}
     */
    'output-port?': (obj) => isOutputPort(obj),

    /**
     * Returns #t if obj is a textual port.
     * @param {*} obj
     * @returns {boolean}
     */
    'textual-port?': (obj) => isPort(obj) && obj.isTextual,

    /**
     * Returns #t if obj is a binary port.
     * @param {*} obj
     * @returns {boolean}
     */
    'binary-port?': (obj) => isPort(obj) && obj.isBinary,

    /**
     * Returns #t if port is an open input port.
     * @param {Port} port
     * @returns {boolean}
     */
    'input-port-open?': (port) => {
        if (!isInputPort(port)) {
            throw new Error('input-port-open?: expected input port');
        }
        return port.isOpen;
    },

    /**
     * Returns #t if port is an open output port.
     * @param {Port} port
     * @returns {boolean}
     */
    'output-port-open?': (port) => {
        if (!isOutputPort(port)) {
            throw new Error('output-port-open?: expected output port');
        }
        return port.isOpen;
    },

    // --------------------------------------------------------------------------
    // Current Ports
    // --------------------------------------------------------------------------

    /**
     * Returns the current input port.
     * @returns {Port}
     */
    'current-input-port': () => {
        if (!currentInputPort) {
            // Create a dummy input port that returns EOF
            currentInputPort = new StringInputPort('');
        }
        return currentInputPort;
    },

    /**
     * Returns the current output port.
     * @returns {Port}
     */
    'current-output-port': () => currentOutputPort,

    /**
     * Returns the current error port.
     * @returns {Port}
     */
    'current-error-port': () => currentErrorPort,

    // --------------------------------------------------------------------------
    // String Ports
    // --------------------------------------------------------------------------

    /**
     * Opens an input port that reads from string.
     * @param {string} str
     * @returns {StringInputPort}
     */
    'open-input-string': (str) => {
        if (typeof str !== 'string') {
            throw new Error('open-input-string: expected string');
        }
        return new StringInputPort(str);
    },

    /**
     * Opens an output port that accumulates to a string.
     * @returns {StringOutputPort}
     */
    'open-output-string': () => new StringOutputPort(),

    /**
     * Returns the accumulated string from a string output port.
     * @param {StringOutputPort} port
     * @returns {string}
     */
    'get-output-string': (port) => {
        if (!(port instanceof StringOutputPort)) {
            throw new Error('get-output-string: expected string output port');
        }
        return port.getString();
    },

    // --------------------------------------------------------------------------
    // EOF
    // --------------------------------------------------------------------------

    /**
     * Returns the unique EOF object.
     * @returns {object}
     */
    'eof-object': () => EOF_OBJECT,

    /**
     * Returns #t if obj is the EOF object.
     * @param {*} obj
     * @returns {boolean}
     */
    'eof-object?': (obj) => obj === EOF_OBJECT,

    // --------------------------------------------------------------------------
    // Input Operations
    // --------------------------------------------------------------------------

    /**
     * Reads the next character from port.
     * @param {Port} [port] - Input port (default: current-input-port).
     * @returns {string|object} Character or EOF.
     */
    'read-char': (...args) => {
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        requireOpenInputPort(port, 'read-char');
        if (!(port instanceof StringInputPort)) {
            throw new Error('read-char: unsupported port type');
        }
        return port.readChar();
    },

    /**
     * Peeks at the next character without consuming it.
     * @param {Port} [port] - Input port (default: current-input-port).
     * @returns {string|object} Character or EOF.
     */
    'peek-char': (...args) => {
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        requireOpenInputPort(port, 'peek-char');
        if (!(port instanceof StringInputPort)) {
            throw new Error('peek-char: unsupported port type');
        }
        return port.peekChar();
    },

    /**
     * Returns #t if a character is ready on port.
     * @param {Port} [port] - Input port (default: current-input-port).
     * @returns {boolean}
     */
    'char-ready?': (...args) => {
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        if (!isInputPort(port)) {
            throw new Error('char-ready?: expected input port');
        }
        if (!port.isOpen) return false;
        if (port instanceof StringInputPort) {
            return port.charReady();
        }
        return false;
    },

    /**
     * Reads a line from port.
     * @param {Port} [port] - Input port (default: current-input-port).
     * @returns {string|object} Line or EOF.
     */
    'read-line': (...args) => {
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        requireOpenInputPort(port, 'read-line');
        if (!(port instanceof StringInputPort)) {
            throw new Error('read-line: unsupported port type');
        }
        return port.readLine();
    },

    /**
     * Reads up to k characters from port.
     * @param {number} k - Number of characters to read.
     * @param {Port} [port] - Input port (default: current-input-port).
     * @returns {string|object} String or EOF.
     */
    'read-string': (k, ...args) => {
        if (typeof k !== 'number' || !Number.isInteger(k) || k < 0) {
            throw new Error('read-string: expected non-negative integer');
        }
        const port = args.length > 0 ? args[0] : ioPrimitives['current-input-port']();
        requireOpenInputPort(port, 'read-string');
        if (!(port instanceof StringInputPort)) {
            throw new Error('read-string: unsupported port type');
        }
        return port.readString(k);
    },

    // --------------------------------------------------------------------------
    // Output Operations
    // --------------------------------------------------------------------------

    /**
     * Writes a character to port.
     * @param {string} char - Character to write.
     * @param {Port} [port] - Output port (default: current-output-port).
     */
    'write-char': (char, ...args) => {
        if (typeof char !== 'string' || char.length !== 1) {
            throw new Error('write-char: expected character');
        }
        const port = args.length > 0 ? args[0] : currentOutputPort;
        requireOpenOutputPort(port, 'write-char');
        port.writeChar(char);
        return undefined;  // unspecified
    },

    /**
     * Writes a string to port.
     * @param {string} str - String to write.
     * @param {Port} [port] - Output port.
     * @param {number} [start] - Start index.
     * @param {number} [end] - End index.
     */
    'write-string': (str, ...args) => {
        if (typeof str !== 'string') {
            throw new Error('write-string: expected string');
        }
        let port = currentOutputPort;
        let start = 0;
        let end = str.length;

        if (args.length >= 1 && isOutputPort(args[0])) {
            port = args[0];
            if (args.length >= 2) start = args[1];
            if (args.length >= 3) end = args[2];
        } else if (args.length >= 1) {
            // First optional arg is start, not port
            start = args[0];
            if (args.length >= 2) end = args[1];
        }

        if (typeof start !== 'number' || start < 0 || start > str.length) {
            throw new Error('write-string: invalid start index');
        }
        if (typeof end !== 'number' || end < start || end > str.length) {
            throw new Error('write-string: invalid end index');
        }

        requireOpenOutputPort(port, 'write-string');
        port.writeString(str, start, end);
        return undefined;  // unspecified
    },

    /**
     * Outputs a newline to port.
     * @param {Port} [port] - Output port (default: current-output-port).
     */
    'newline': (...args) => {
        const port = args.length > 0 ? args[0] : currentOutputPort;
        requireOpenOutputPort(port, 'newline');
        port.writeChar('\n');
        return undefined;  // unspecified
    },

    /**
     * Displays a value (human-readable, strings not quoted).
     * @param {*} val - Value to display.
     * @param {Port} [port] - Output port (default: current-output-port).
     */
    'display': (val, ...args) => {
        const port = args.length > 0 ? args[0] : currentOutputPort;
        requireOpenOutputPort(port, 'display');
        const str = displayString(val);
        port.writeString(str);
        return undefined;  // unspecified
    },

    /**
     * Writes a value (machine-readable, strings quoted).
     * @param {*} val - Value to write.
     * @param {Port} [port] - Output port (default: current-output-port).
     */
    'write': (val, ...args) => {
        const port = args.length > 0 ? args[0] : currentOutputPort;
        requireOpenOutputPort(port, 'write');
        const str = writeString(val);
        port.writeString(str);
        return undefined;  // unspecified
    },

    // --------------------------------------------------------------------------
    // Port Control
    // --------------------------------------------------------------------------

    /**
     * Closes a port.
     * @param {Port} port
     */
    'close-port': (port) => {
        if (!isPort(port)) {
            throw new Error('close-port: expected port');
        }
        port.close();
        return undefined;  // unspecified
    },

    /**
     * Closes an input port.
     * @param {Port} port
     */
    'close-input-port': (port) => {
        if (!isInputPort(port)) {
            throw new Error('close-input-port: expected input port');
        }
        port.close();
        return undefined;  // unspecified
    },

    /**
     * Closes an output port.
     * @param {Port} port
     */
    'close-output-port': (port) => {
        if (!isOutputPort(port)) {
            throw new Error('close-output-port: expected output port');
        }
        port.close();
        return undefined;  // unspecified
    },

    /**
     * Flushes output to port (no-op for most ports).
     * @param {Port} [port] - Output port.
     */
    'flush-output-port': (...args) => {
        const port = args.length > 0 ? args[0] : currentOutputPort;
        if (!isOutputPort(port)) {
            throw new Error('flush-output-port: expected output port');
        }
        if (port instanceof ConsoleOutputPort) {
            port.flush();
        }
        return undefined;  // unspecified
    },
};

// Export for testing
export { Port, StringInputPort, StringOutputPort, ConsoleOutputPort, EOF_OBJECT };
