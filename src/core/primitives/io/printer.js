import { Cons } from '../../interpreter/cons.js';
import { Symbol } from '../../interpreter/symbol.js';
import { Port, EOF_OBJECT } from './ports.js';
import { Rational } from '../rational.js';
import { Complex } from '../complex.js';
import { Char } from '../char_class.js';

// ============================================================================
// Printer Logic (Display/Write)
// ============================================================================

/**
 * Converts a Scheme value to its display representation.
 * @param {*} val
 * @returns {string}
 */
export function displayString(val) {
    return genericToString(val, 'display');
}

/**
 * Converts a Scheme value to its write (machine-readable) representation.
 * @param {*} val
 * @returns {string}
 */
export function writeString(val) {
    return genericToString(val, 'write');
}

/**
 * Internal generic string conversion with mode support.
 * @param {*} val - Value to convert
 * @param {'display'|'write'} mode - Printer mode
 * @returns {string}
 */
function genericToString(val, mode) {
    // 1. Primitive shared values
    if (val === null) return '()';
    if (val === true) return '#t';
    if (val === false) return '#f';
    if (val === EOF_OBJECT) return '#<eof>';
    if (val instanceof Port) return val.toString();

    // 2. Numbers (Shared logic)
    if (typeof val === 'number') {
        if (val === Infinity) return '+inf.0';
        if (val === -Infinity) return '-inf.0';
        if (Number.isNaN(val)) return '+nan.0';
        let s = String(val);
        if (Number.isInteger(val) && !s.includes('.') && !s.includes('e')) {
            s += '.0';
        }
        return s;
    }
    if (typeof val === 'bigint') return String(val);
    if (val instanceof Rational) return val.toString();
    if (val instanceof Complex) return val.toString();

    // 3. Procedures (Shared logic)
    if (typeof val === 'function' || (val && val.constructor && val.constructor.name === 'Closure')) {
        const name = val.constructor ? val.constructor.name : 'Unknown';
        if (name === 'Closure') return val.toString();
        return `#<procedure ${name}>`;
    }

    // 4. Strings (Mode-specific)
    if (typeof val === 'string') {
        if (mode === 'display') return val;
        // write: escape and quote
        return '"' + val
            .replace(/\\/g, '\\\\')
            .replace(/"/g, '\\"')
            .replace(/\n/g, '\\n')
            .replace(/\r/g, '\\r')
            .replace(/\t/g, '\\t') + '"';
    }

    // 5. Symbols (Mode-specific)
    if (val instanceof Symbol) {
        if (mode === 'display') return val.name;
        // write: check for escaping
        return writeSymbol(val.name);
    }

    // 6. Characters (Mode-specific)
    if (val instanceof Char) {
        if (mode === 'display') return val.toString();
        // write: Scheme representation
        const ch = val.toString();
        if (ch === ' ') return '#\\space';
        if (ch === '\n') return '#\\newline';
        if (ch === '\t') return '#\\tab';
        if (ch === '\r') return '#\\return';
        return '#\\' + ch;
    }

    // 7. Compound Structures (Recursive with same mode)
    const elemFn = mode === 'display' ? displayString : writeString;
    if (val instanceof Cons) return consToString(val, elemFn);
    if (Array.isArray(val)) return vectorToString(val, elemFn);
    if (val instanceof Uint8Array) return bytevectorToString(val);

    // 8. Object-like (Records, JS Objects)
    if (isObjectLike(val)) {
        return objectToString(val, elemFn);
    }

    return String(val);
}

/**
 * Writes a symbol, escaping with |...| if needed per R7RS.
 * Symbols that look like numbers, start with special chars, contain whitespace
 * or special characters, or are empty need to be wrapped in |...|.
 * @param {string} name - Symbol name
 * @returns {string} Properly escaped symbol representation
 */
function writeSymbol(name) {
    // Empty symbol
    if (name === '') {
        return '||';
    }

    // Check if the symbol needs escaping
    if (symbolNeedsEscaping(name)) {
        // Escape backslashes and vertical bars, then wrap in |...|
        const escaped = name
            .replace(/\\/g, '\\\\')
            .replace(/\|/g, '\\|');
        return '|' + escaped + '|';
    }

    return name;
}

/**
 * Checks if a symbol name needs |...| escaping.
 * @param {string} name - Symbol name
 * @returns {boolean} True if escaping is needed
 */
function symbolNeedsEscaping(name) {
    // Empty string always needs escaping
    if (name === '') return true;

    // Single dot needs escaping
    if (name === '.') return true;

    // Contains whitespace or special characters
    if (/[\s"'`,;()[\]{}|\\]/.test(name)) return true;

    // Looks like it could be parsed as a number
    // This includes: starts with digit, +digit, -digit, ., +., -.
    // Also +i, -i, +nan.0, -nan.0, +inf.0, -inf.0

    // Check if it looks like a numeric literal
    const lowerName = name.toLowerCase();

    // Pure numeric forms
    if (/^[+-]?\.?\d/.test(name)) return true;

    // +i, -i
    if (lowerName === '+i' || lowerName === '-i') return true;

    // +nan.0, -nan.0, +inf.0, -inf.0
    if (/^[+-]?(nan|inf)\.0/i.test(name)) return true;

    // Starts with # (could look like numeric prefix)
    if (name.startsWith('#')) return true;

    return false;
}


/**
 * Converts a Scheme value to its write representation with shared structure detection.
 * Uses datum labels (#n= and #n#) for cycles and shared objects.
 * @param {*} val
 * @returns {string}
 */
export function writeStringShared(val) {
    // First pass: find shared/cyclic objects
    const seen = new Map();  // object -> { count: number, id: number|null }
    let nextId = 0;

    function countOccurrences(obj) {
        if (obj === null || typeof obj !== 'object') return;
        if (typeof obj === 'function') return;

        // Skip non-compound types (but include object-like values)
        if (!(obj instanceof Cons) && !Array.isArray(obj) && !isObjectLike(obj)) return;

        if (seen.has(obj)) {
            const info = seen.get(obj);
            info.count++;
            if (info.id === null) {
                info.id = nextId++;
            }
        } else {
            seen.set(obj, { count: 1, id: null });
            if (obj instanceof Cons) {
                countOccurrences(obj.car);
                countOccurrences(obj.cdr);
            } else if (Array.isArray(obj)) {
                for (const elem of obj) {
                    countOccurrences(elem);
                }
            } else if (isObjectLike(obj)) {
                // Traverse object values for circular reference detection
                for (const key of Object.keys(obj)) {
                    if (key !== 'type' && key !== 'typeDescriptor') {
                        countOccurrences(obj[key]);
                    }
                }
            }
        }
    }

    countOccurrences(val);

    // Second pass: build output with datum labels
    const emitted = new Set();  // objects that have been output with #n=

    function emit(obj) {
        // Handle primitives
        if (obj === null) return '()';
        if (obj === true) return '#t';
        if (obj === false) return '#f';
        if (typeof obj === 'string') {
            return '"' + obj
                .replace(/\\/g, '\\\\')
                .replace(/"/g, '\\"')
                .replace(/\n/g, '\\n')
                .replace(/\r/g, '\\r')
                .replace(/\t/g, '\\t') + '"';
        }
        if (typeof obj === 'number') {
            if (obj === Infinity) return '+inf.0';
            if (obj === -Infinity) return '-inf.0';
            if (Number.isNaN(obj)) return '+nan.0';
            let s = String(obj);
            if (Number.isInteger(obj) && !s.includes('.') && !s.includes('e')) {
                s += '.0';
            }
            return s;
        }
        if (obj instanceof Symbol) return writeSymbol(obj.name); // Symbol
        if (obj === EOF_OBJECT) return '#<eof>';
        if (obj instanceof Port) return obj.toString();
        if (typeof obj === 'function') {
            const name = obj.constructor ? obj.constructor.name : 'Unknown';
            if (name === 'Closure') return obj.toString();
            return `#<procedure ${name}>`;
        }
        // Handle compound types with sharing detection
        if (obj instanceof Cons || Array.isArray(obj) || isObjectLike(obj)) {
            const info = seen.get(obj);
            if (info && info.id !== null) {
                if (emitted.has(obj)) {
                    // Already emitted - use reference
                    return `#${info.id}#`;
                } else {
                    // First emission - add label
                    emitted.add(obj);
                    if (obj instanceof Cons) {
                        return `#${info.id}=${consToStringShared(obj, emit)}`;
                    } else if (Array.isArray(obj)) {
                        return `#${info.id}=${vectorToStringShared(obj, emit)}`;
                    } else {
                        return `#${info.id}=${objectToStringShared(obj, emit)}`;
                    }
                }
            } else {
                // Not shared - normal output
                if (obj instanceof Cons) {
                    return consToStringShared(obj, emit);
                } else if (Array.isArray(obj)) {
                    return vectorToStringShared(obj, emit);
                } else {
                    return objectToStringShared(obj, emit);
                }
            }
        }

        if (obj instanceof Uint8Array) return bytevectorToString(obj);

        return String(obj);
    }

    function objectToStringShared(obj, emitFn) {
        const entries = Object.entries(obj).filter(([key]) => {
            return key !== 'type' && key !== 'typeDescriptor';
        });

        if (entries.length === 0) {
            return '#{}';
        }

        const parts = entries.map(([key, value]) => {
            const keyStr = formatObjectKey(key);
            const valStr = emitFn(value);
            return `(${keyStr} ${valStr})`;
        });

        return '#{' + parts.join(' ') + '}';
    }

    function consToStringShared(cons, emitFn) {
        const parts = [];
        let current = cons;
        const visitedInChain = new Set();

        while (current instanceof Cons) {
            // Check if this cons is shared and already referenced
            const info = seen.get(current);
            if (info && info.id !== null && emitted.has(current) &&
                (current !== cons || parts.length > 0)) {
                // Terminate with improper tail reference
                return '(' + parts.join(' ') + ' . #' + info.id + '#)';
            }

            // Detect cycle within same chain (not via datum labels)
            if (visitedInChain.has(current)) {
                return '(' + parts.join(' ') + ' . ...)';
            }
            visitedInChain.add(current);

            parts.push(emitFn(current.car));
            current = current.cdr;
        }

        if (current === null) {
            return '(' + parts.join(' ') + ')';
        } else {
            return '(' + parts.join(' ') + ' . ' + emitFn(current) + ')';
        }
    }

    function vectorToStringShared(vec, emitFn) {
        return '#(' + vec.map(emitFn).join(' ') + ')';
    }

    return emit(val);
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

function bytevectorToString(bv) {
    return '#u8(' + Array.from(bv).join(' ') + ')';
}

// ============================================================================
// Object Printing Helpers
// ============================================================================

/**
 * Checks if a value should be printed as a JS object literal #{...}.
 * Includes plain objects, records, and class instances.
 * Excludes arrays, typed arrays, Cons cells, Ports, closures, etc.
 * @param {*} val - The value to check.
 * @returns {boolean} True if val should print as #{...}.
 */
function isObjectLike(val) {
    if (val === null || typeof val !== 'object') return false;
    if (Array.isArray(val)) return false;
    if (val instanceof Uint8Array) return false;
    if (val instanceof Cons) return false;
    if (val instanceof Port) return false;
    if (val instanceof Symbol) return false;
    if (val === EOF_OBJECT) return false;
    // Check for char objects
    if (val instanceof Char) return false;
    if (val instanceof Rational) return false;
    if (val instanceof Complex) return false;
    // It's an object-like value (plain object, record, or class instance)
    return true;
}

/**
 * Converts a JavaScript object to its reader syntax representation.
 * Format: #{(key1 val1) (key2 val2) ...}
 * Keys that are valid Scheme identifiers are unquoted; others are quoted strings.
 * @param {Object} obj - The object to convert.
 * @param {Function} elemFn - Function to convert values (displayString or writeString).
 * @returns {string} The #{...} representation.
 */
function objectToString(obj, elemFn) {
    const entries = Object.entries(obj).filter(([key]) => {
        // Skip internal properties like 'type', 'typeDescriptor' for records
        // But include all user-defined fields
        return key !== 'type' && key !== 'typeDescriptor';
    });

    if (entries.length === 0) {
        return '#{}';
    }

    const parts = entries.map(([key, value]) => {
        const keyStr = formatObjectKey(key);
        const valStr = elemFn(value);
        return `(${keyStr} ${valStr})`;
    });

    return '#{' + parts.join(' ') + '}';
}

/**
 * Formats an object key for printing.
 * Valid Scheme identifiers are unquoted; others are quoted strings.
 * @param {string} key - The object key.
 * @returns {string} Formatted key.
 */
function formatObjectKey(key) {
    // Use the same logic as symbolNeedsEscaping to determine if quoting is needed
    if (symbolNeedsEscaping(key)) {
        // Quote the key as a string
        return '"' + key
            .replace(/\\/g, '\\\\')
            .replace(/"/g, '\\"')
            .replace(/\n/g, '\\n')
            .replace(/\r/g, '\\r')
            .replace(/\t/g, '\\t') + '"';
    }
    return key;
}
