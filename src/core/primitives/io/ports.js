/**
 * Port Base Classes and Utilities
 * 
 * Defines the base Port class, EOF object, and helper predicates.
 */

// ============================================================================
// EOF Object
// ============================================================================

/**
 * Unique EOF object.
 * In R7RS, (eof-object) returns a unique object that satisfies eof-object?.
 */
export const EOF_OBJECT = Object.freeze({ type: 'eof-object', toString: () => '#<eof>' });

// ============================================================================
// Port Classes
// ============================================================================

/**
 * Base class for all ports.
 */
export class Port {
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

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Checks if a value is a Port.
 * @param {*} val
 * @returns {boolean}
 */
export function isPort(val) {
    return val instanceof Port;
}

/**
 * Checks if a value is an input port.
 * @param {*} val
 * @returns {boolean}
 */
export function isInputPort(val) {
    return val instanceof Port && val.isInput;
}

/**
 * Checks if a value is an output port.
 * @param {*} val
 * @returns {boolean}
 */
export function isOutputPort(val) {
    return val instanceof Port && val.isOutput;
}

/**
 * Validates that a value is an open input port.
 * @param {*} port
 * @param {string} procName
 */
export function requireOpenInputPort(port, procName) {
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
export function requireOpenOutputPort(port, procName) {
    if (!isOutputPort(port)) {
        throw new Error(`${procName}: expected output port, got ${typeof port}`);
    }
    if (!port.isOpen) {
        throw new Error(`${procName}: port is closed`);
    }
}
