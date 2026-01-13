import { Cons } from '../../interpreter/cons.js';
import { Symbol } from '../../interpreter/symbol.js';
import { Port, EOF_OBJECT } from './ports.js';

// ============================================================================
// Printer Logic (Display/Write)
// ============================================================================

/**
 * Converts a Scheme value to its display representation.
 * @param {*} val
 * @returns {string}
 */
export function displayString(val) {
    if (typeof val === 'function' || val && val.constructor && val.constructor.name === 'Closure') {
        const name = val.constructor ? val.constructor.name : 'Unknown';
        if (name === 'Closure') return val.toString();
        return `#<procedure ${name}>`;
    }
    if (val === null) return '()';
    if (val === true) return '#t';
    if (val === false) return '#f';
    if (typeof val === 'string') return val;  // display doesn't quote strings
    if (typeof val === 'number') {
        // R7RS special value formatting
        if (val === Infinity) return '+inf.0';
        if (val === -Infinity) return '-inf.0';
        if (Number.isNaN(val)) return '+nan.0';
        return String(val);
    }
    if (val instanceof Cons) return consToString(val, displayString);
    if (Array.isArray(val)) return vectorToString(val, displayString);
    if (val instanceof Symbol) return val.name; // Symbol - display doesn't wrap
    if (val === EOF_OBJECT) return '#<eof>';
    if (val instanceof Port) return val.toString();
    if (typeof val === 'function') {
        const name = val.constructor ? val.constructor.name : 'Unknown';
        if (name === 'Closure') return val.toString();
        return `#<procedure ${name}>`;
    }
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
export function writeString(val) {
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
    if (typeof val === 'number') {
        // R7RS special value formatting
        if (val === Infinity) return '+inf.0';
        if (val === -Infinity) return '-inf.0';
        if (Number.isNaN(val)) return '+nan.0';
        return String(val);
    }
    if (val instanceof Cons) return consToString(val, writeString);
    if (Array.isArray(val)) return vectorToString(val, writeString);
    if (val instanceof Symbol) {
        // Symbol - check if it needs |...| escaping
        return writeSymbol(val.name);
    }
    if (val === EOF_OBJECT) return '#<eof>';
    if (val instanceof Port) return val.toString();
    if (typeof val === 'function') {
        const name = val.constructor ? val.constructor.name : 'Unknown';
        if (name === 'Closure') return val.toString();
        return `#<procedure ${name}>`;
    }
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

        // Skip non-compound types
        if (!(obj instanceof Cons) && !Array.isArray(obj)) return;

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
        if (typeof obj === 'number') return String(obj);
        if (obj && obj.name && obj.description === undefined) return obj.name; // Symbol
        if (obj === EOF_OBJECT) return '#<eof>';
        if (obj instanceof Port) return obj.toString();
        if (typeof obj === 'function') {
            const name = obj.constructor ? obj.constructor.name : 'Unknown';
            if (name === 'Closure') return obj.toString();
            return `#<procedure ${name}>`;
        }
        if (obj && obj.type === 'record') {
            return `#<${obj.typeDescriptor.name}>`;
        }

        // Handle compound types with sharing detection
        if (obj instanceof Cons || Array.isArray(obj)) {
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
                    } else {
                        return `#${info.id}=${vectorToStringShared(obj, emit)}`;
                    }
                }
            } else {
                // Not shared - normal output
                if (obj instanceof Cons) {
                    return consToStringShared(obj, emit);
                } else {
                    return vectorToStringShared(obj, emit);
                }
            }
        }

        return String(obj);
    }

    function consToStringShared(cons, emitFn) {
        const parts = [];
        let current = cons;
        const visitedInChain = new Set();

        while (current instanceof Cons) {
            // Check if this cons is shared and already referenced
            const info = seen.get(current);
            if (info && info.id !== null && emitted.has(current) && current !== cons) {
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
