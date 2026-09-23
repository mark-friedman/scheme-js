
import { Cons, toArray } from './cons.js';
// Vector is Array in this runtime
import { Complex } from '../primitives/complex.js';
import { Rational } from '../primitives/rational.js';
import { Char } from '../primitives/char_class.js';
import { Closure, Continuation } from './values.js';

// Registry for the JS Object Record constructor
let JsObjectRecord = null;

/**
 * Registers the js-object record constructor for use in conversion.
 * @param {Function} ctor - The record constructor class/function
 */
export function registerJsObjectRecord(ctor) {
    JsObjectRecord = ctor;
}

// ============================================================================
// Hpers
// ============================================================================

function isPrimitive(val) {
    return (val === null ||
        typeof val === 'undefined' ||
        typeof val === 'boolean' ||
        typeof val === 'number' ||
        typeof val === 'string' ||
        typeof val === 'symbol');
}

// ============================================================================
// Scheme -> JS Conversion
// ============================================================================

/**
 * Shallow conversion from Scheme to JS.

 - BigInt: converted to Number within safe range (default)
 - Rational: converted to Number, but may lose precision
 - Char: converted to String
 - Other: returned as-is
 */
export function schemeToJs(val) {
    // Convert BigInt to Number for JS API calls
    // (JS APIs like Date, Math, etc. require Number, not BigInt)
    if (typeof val === 'bigint') {
        if (val >= Number.MIN_SAFE_INTEGER && val <= Number.MAX_SAFE_INTEGER) {
            return Number(val);
        }
        throw new Error(`BigInt ${val} is outside safe integer range for JS API call.`);
    }
    if (val instanceof Char) return val.toString();
    if (val instanceof Rational) return val.toNumber();
    // Complex, Vector, Records, Cons -> passed as opaque objects
    return val;
}

/**
 * Deep converts a Scheme value to its JavaScript equivalent.
 *
 * - BigInt: converted to Number within safe range (default)
 * - Rational: converted to Number, but may lose precision
 * - Char: converted to String
 * - Vector: recursively converted to Array
 * - JsObjectRecord: recursively converted to plain Object
 * - Other: returned as-is
 *
 * @param {*} val - Value to convert
 * @param {Object} [options={}] - Conversion options
 * @param {boolean} [options.convertBigInt=true] - Whether to convert BigInt to Number
 * @returns {*} Converted value
 */
export function schemeToJsDeep(val, options = {}) {
    const convertBigInt = options.convertBigInt !== false;

    // 1. Primitive Conversion (Shallow Check first)
    if (typeof val === 'bigint') {
        if (!convertBigInt) return val;
        if (val >= Number.MIN_SAFE_INTEGER && val <= Number.MAX_SAFE_INTEGER) {
            return Number(val);
        }
        throw new Error(`BigInt ${val} is outside safe integer range for JS API call.`);
    }

    if (val instanceof Char) return val.toString();
    if (val instanceof Rational) return val.toNumber();

    // Vectors are Arrays in this implementation - recursively convert elements
    if (Array.isArray(val)) {
        return val.map(v => schemeToJsDeep(v, options));
    }

    // 4. js-object Record -> JS Object (Unwrap)
    // If we have the registry, check instanceof. If not, check static prop.
    if ((JsObjectRecord && val instanceof JsObjectRecord) ||
        (val && val.constructor && val.constructor.schemeName === 'js-object')) {

        // This record wraps properties directly on itself (transparent wrapper).
        // BUT it might have internal properties we don't want (like constructor, toString).
        // "duplciates the JS object's properties"

        const obj = {};
        // Copy own enumerable properties, recursively converting
        for (const key of Object.keys(val)) {
            obj[key] = schemeToJsDeep(val[key], options);
        }
        return obj;
    }

    // 5. General Fallback (Identity for unknown records, opaque types)
    return val;
}

// ============================================================================
// JS -> Scheme Conversion
// ============================================================================

/**
 * Convert JS integer Number to BigInt for Scheme compatibility.
 * Scheme uses BigInt for exact integers; JS integers should become BigInt.
 */
function maybeIntToBigInt(val) {
    if (typeof val === 'number' && Number.isInteger(val) && Number.isFinite(val)) {
        return BigInt(val);
    }
    return val;
}

/**
 * Shallow conversion JS -> Scheme.
 */
export function jsToScheme(val) {
    return maybeIntToBigInt(val);
}

/**
 * Deep recursive conversion JS -> Scheme.
 */
export function jsToSchemeDeep(val) {
    // Convert integers to BigInt
    val = maybeIntToBigInt(val);

    if (isPrimitive(val)) return val;

    // Arrays -> Vector
    if (Array.isArray(val)) {
        return val.map(jsToSchemeDeep);
    }

    // Plain JS Objects -> js-object Record
    if (val.constructor === Object) {
        if (!JsObjectRecord) {
            console.warn("js-object record type not registered. Returning JS Object as-is.");
            return val;
        }

        const rec = new JsObjectRecord();
        for (const [k, v] of Object.entries(val)) {
            rec[k] = jsToSchemeDeep(v);
        }
        return rec;
    }

    // Fallback
    return val;
}

// ============================================================================
// Numbers Stored in JavaScript Properties
// ============================================================================

// A JavaScript number carries no exactness. Scheme represents an exact integer
// as a BigInt and an inexact real as a number, but an integer that JavaScript
// code writes into a property is a number too, and the interop policy is that
// it reads back in Scheme as exact. So an integer-valued number in a property
// is ambiguous: it is a flonum if Scheme stored it and an exact integer if
// JavaScript did. Neither the property nor the value can say which, so every
// Scheme-side store -- record constructors and modifiers, `js-set!`, and
// `define-class` construction -- notes each integer-valued flonum it stores,
// by object and property, and every Scheme-side read consults the note.
//
// A note is consulted only while the property still holds the very number it
// records, so a later JavaScript write of any other value is read as
// JavaScript's. A note is not cleared when Scheme later stores a non-number,
// which keeps stores free of a lookup; the only cost is that JavaScript
// writing back that same integer afterwards reads as the flonum.
//
// A WeakMap keeps the notes off the objects themselves, so objects keep one
// shape and JavaScript sees no extra property.
const schemeFlonums = new WeakMap();

/**
 * Whether a value is an integer-valued JavaScript number: the one kind of
 * stored value whose exactness depends on who stored it.
 * @param {*} value - A stored value.
 * @returns {boolean} True for a finite, integer-valued number, including -0.
 */
export function isIntegerNumber(value) {
    return typeof value === 'number' && Number.isInteger(value);
}

/**
 * Notes a value Scheme has just stored in an object's property, so that it
 * reads back with its exactness. Only an integer-valued flonum needs a note;
 * anything else is ignored.
 * @param {Object|Function} object - The object written to.
 * @param {string} key - The property written.
 * @param {*} value - The Scheme value stored.
 */
export function noteSchemeStore(object, key, value) {
    if (!isIntegerNumber(value)) {
        return;
    }
    let keys = schemeFlonums.get(object);
    if (keys === undefined) {
        keys = new Map();
        schemeFlonums.set(object, keys);
    }
    keys.set(key, value);
}

/**
 * Converts a value read from an object's property to Scheme: an integer
 * JavaScript wrote becomes exact, and a flonum Scheme stored stays a flonum.
 * @param {*} object - The object read from.
 * @param {string} key - The property read.
 * @param {*} value - The value the property holds.
 * @returns {*} The Scheme value.
 */
export function storedToScheme(object, key, value) {
    if (!isIntegerNumber(value)) {
        return value;
    }
    const keys = schemeFlonums.get(object);
    // Object.is, so that a JavaScript 0 over a Scheme -0.0 counts as a new value.
    if (keys !== undefined && Object.is(keys.get(key), value)) {
        return value;
    }
    return BigInt(value);
}
