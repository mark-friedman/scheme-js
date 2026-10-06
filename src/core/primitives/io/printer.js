import { Cons } from '../../interpreter/cons.js';
import { Symbol } from '../../interpreter/symbol.js';
import { Port, EOF_OBJECT } from './ports.js';
import { Rational } from '../rational.js';
import { Complex } from '../complex.js';
import { Char } from '../char_class.js';
import { SchemeString } from '../string_class.js';
import { Flonum, inexactText } from '../../interpreter/number_representation.js';

// ============================================================================
// Printer Logic (Display/Write)
// ============================================================================

/**
 * Converts a Scheme value to its display representation.
 * @param {*} val
 * @returns {string}
 */
export function displayString(val) {
    return printValue(val, 'display', 'cycles');
}

/**
 * Converts a Scheme value to its write (machine-readable) representation.
 * @param {*} val
 * @returns {string}
 */
export function writeString(val) {
    return printValue(val, 'write', 'cycles');
}

/**
 * Converts a Scheme value to its write representation with shared structure detection.
 * Uses datum labels (#n= and #n#) for cycles and shared objects.
 * @param {*} val
 * @returns {string}
 */
export function writeStringShared(val) {
    return printValue(val, 'write', 'shared');
}

/**
 * Converts a Scheme value to its write representation without datum labels,
 * as `write-simple` writes it: it does not terminate on circular structure.
 * @param {*} val
 * @returns {string}
 */
export function writeStringSimple(val) {
    return printValue(val, 'write', 'none');
}

/**
 * Whether a value holds a cycle of pairs and vectors: one that can be
 * reached from itself through them. Records and other objects are not
 * followed, for a printer that does not write their fields.
 * @param {*} val
 * @returns {boolean}
 */
export function isCircular(val) {
    const isPairOrVector = (obj) => obj instanceof Cons || Array.isArray(obj);
    return isPairOrVector(val) && objectsToLabel(val, 'cycles', isPairOrVector).size > 0;
}

/**
 * Converts a value to text, giving datum labels (R7RS 2.4) to the objects
 * `labelling` asks for: 'cycles', one object in each cycle and no others, as
 * `write` and `display` must (R7RS 6.13.3), so that a value with no cycle has
 * no labels; 'shared', every object written more than once, as
 * `write-shared` does; or 'none'. Labels are numbered in the order they are
 * written.
 * @param {*} val - Value to convert
 * @param {'display'|'write'} mode - Printer mode
 * @param {'cycles'|'shared'|'none'} labelling - Which objects get labels
 * @returns {string}
 */
function printValue(val, mode, labelling) {
    if (!isCompound(val)) return atomToString(val, mode);
    const labelled = labelling === 'none' ? new Set() : objectsToLabel(val, labelling);
    const unlabelled = labelled.size === 0;
    const labels = new Map();  // labelled object -> its number, once written

    function emit(obj) {
        if (!isCompound(obj)) return atomToString(obj, mode);
        if (unlabelled || !labelled.has(obj)) return body(obj);
        const label = labels.get(obj);
        if (label !== undefined) return `#${label}#`;
        const next = labels.size;
        labels.set(obj, next);
        return `#${next}=${body(obj)}`;
    }

    function body(obj) {
        if (obj instanceof Cons) {
            // A pair in the list's tail that has a label ends the list after
            // a dot, so that its label can be written
            const parts = [emit(obj.car)];
            let rest = obj.cdr;
            while (rest instanceof Cons && (unlabelled || !labelled.has(rest))) {
                parts.push(emit(rest.car));
                rest = rest.cdr;
            }
            if (rest === null) return '(' + parts.join(' ') + ')';
            return '(' + parts.join(' ') + ' . ' + emit(rest) + ')';
        }
        if (Array.isArray(obj)) return '#(' + obj.map(emit).join(' ') + ')';
        return objectToString(obj, emit);
    }

    return emit(val);
}

/**
 * Finds the objects a value's written form gives datum labels.
 *
 * A depth-first walk: an object reached again while it is still being
 * walked, an ancestor of where it is reached, closes a cycle, and every
 * cycle has one such object -- the first of it the walk reaches. With
 * `labelling` 'shared', an object reached again after its walk ended is
 * labelled too. A list's pairs are walked in a loop down its cdrs, so a long
 * list does not take a stack frame per element.
 * @param {*} root - A pair, vector or record.
 * @param {'cycles'|'shared'} labelling - As for `printValue`.
 * @param {function(*): boolean} [follows] - Which values are walked into.
 * @returns {Set<Object>} The objects to label.
 */
function objectsToLabel(root, labelling, follows = isCompound) {
    const labelled = new Set();
    // Each object reached: WALKING while it is being walked, then WALKED
    const state = new Map();

    function visit(obj) {
        if (!follows(obj)) return;
        const reached = state.get(obj);
        if (reached !== undefined) {
            if (reached === WALKING || labelling === 'shared') labelled.add(obj);
            return;
        }
        if (obj instanceof Cons) {
            let pair = obj;
            let length = 0;
            while (pair instanceof Cons && !state.has(pair)) {
                state.set(pair, WALKING);
                length++;
                visit(pair.car);
                pair = pair.cdr;
            }
            visit(pair);
            for (let p = obj, i = 0; i < length; i++, p = p.cdr) state.set(p, WALKED);
            return;
        }
        state.set(obj, WALKING);
        const children = Array.isArray(obj) ? obj : objectFieldValues(obj);
        for (const child of children) visit(child);
        state.set(obj, WALKED);
    }

    visit(root);
    return labelled;
}

const WALKING = 1;
const WALKED = 2;

/**
 * Whether a value is written with the values it holds: a pair, a vector, or
 * a record or other object written as `#{...}`.
 * @param {*} val
 * @returns {boolean}
 */
function isCompound(val) {
    return val instanceof Cons || Array.isArray(val) || isObjectLike(val);
}


/**
 * Converts a value that holds no other values to text.
 * @param {*} val - Value to convert
 * @param {'display'|'write'} mode - Printer mode
 * @returns {string}
 */
function atomToString(val, mode) {
    // A mutable string is written as the characters it holds.
    if (val instanceof SchemeString) val = val.toString();

    // 1. Primitive shared values
    if (val === null) return '()';
    if (val === true) return '#t';
    if (val === false) return '#f';
    if (val === EOF_OBJECT) return '#<eof>';
    if (val instanceof Port) return val.toString();

    // 2. Numbers (Shared logic)
    // An integral number is an exact integer; any other, and a Flonum, an
    // inexact real (number_representation.js).
    if (typeof val === 'number') return Number.isInteger(val) ? String(val) : inexactText(val);
    if (val instanceof Flonum) return inexactText(val.value);
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

    // 7. Bytevectors, which hold only bytes
    if (val instanceof Uint8Array) return bytevectorToString(val);

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
    if (val instanceof SchemeString) return false;
    if (val === EOF_OBJECT) return false;
    // Check for char objects
    if (val instanceof Char) return false;
    if (val instanceof Rational) return false;
    if (val instanceof Complex) return false;
    if (val instanceof Flonum) return false;
    // It's an object-like value (plain object, record, or class instance)
    return true;
}

/**
 * Converts a JavaScript object to its reader syntax representation.
 * Format: #{(key1 val1) (key2 val2) ...}
 * Keys that are valid Scheme identifiers are unquoted; others are quoted strings.
 * @param {Object} obj - The object to convert.
 * @param {Function} elemFn - Converts each field's value to text.
 * @returns {string} The #{...} representation.
 */
function objectToString(obj, elemFn) {
    const entries = objectFields(obj);

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
 * The fields of an object written as `#{...}`, as [key, value] pairs: all but
 * a record's internal 'type' and 'typeDescriptor'.
 * @param {Object} obj
 * @returns {Array<[string, *]>}
 */
function objectFields(obj) {
    return Object.entries(obj).filter(([key]) => key !== 'type' && key !== 'typeDescriptor');
}

/**
 * The values of an object's fields, as `objectFields` gives them.
 * @param {Object} obj
 * @returns {Array<*>}
 */
function objectFieldValues(obj) {
    return objectFields(obj).map(([, value]) => value);
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
