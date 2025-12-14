/**
 * Type Checking Utilities for Scheme Primitives
 * 
 * Provides reusable type predicates and assertion functions
 * that throw structured SchemeError instances.
 */

import { Cons } from './cons.js';
import { Symbol } from './symbol.js';
import { Closure, Continuation } from './values.js';
import { SchemeTypeError, SchemeArityError, SchemeRangeError } from './errors.js';

// =============================================================================
// Type Predicates
// =============================================================================

/**
 * Checks if value is a pair (Cons cell).
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isPair(x) {
    return x instanceof Cons;
}

/**
 * Checks if value is a proper list (null-terminated chain of pairs).
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isList(x) {
    if (x === null) return true;
    if (!(x instanceof Cons)) return false;
    // Use tortoise-hare algorithm to detect cycles
    let slow = x;
    let fast = x;
    while (fast !== null && fast instanceof Cons) {
        slow = slow.cdr;
        fast = fast.cdr;
        if (fast instanceof Cons) {
            fast = fast.cdr;
        }
        if (slow === fast && fast !== null) {
            return false; // Cycle detected
        }
    }
    return fast === null;
}

/**
 * Checks if value is a number.
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isNumber(x) {
    return typeof x === 'number';
}

/**
 * Checks if value is an integer.
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isInteger(x) {
    return typeof x === 'number' && Number.isInteger(x);
}

/**
 * Checks if value is a string.
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isString(x) {
    return typeof x === 'string';
}

/**
 * Checks if value is a symbol.
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isSymbol(x) {
    return x instanceof Symbol;
}

/**
 * Checks if value is a boolean.
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isBoolean(x) {
    return typeof x === 'boolean';
}

/**
 * Checks if value is a vector.
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isVector(x) {
    return Array.isArray(x);
}

/**
 * Checks if value is a procedure (closure, continuation, or JS function).
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isProcedure(x) {
    return typeof x === 'function' || x instanceof Closure || x instanceof Continuation;
}

/**
 * Checks if value is a character (single-character string for now).
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isChar(x) {
    // For now, characters are single-char strings
    // This may change if we add a dedicated Char type
    return typeof x === 'string' && x.length === 1;
}

// =============================================================================
// Assertion Helpers
// =============================================================================

/**
 * Asserts that a value satisfies a predicate, throwing SchemeTypeError if not.
 * @param {string} procName - Procedure name for error messages
 * @param {number} argPos - Argument position (1-indexed)
 * @param {*} value - Value to check
 * @param {Function} predicate - Predicate function
 * @param {string} typeName - Expected type name for error message
 * @returns {*} The value if valid
 * @throws {SchemeTypeError} If predicate fails
 */
export function assertType(procName, argPos, value, predicate, typeName) {
    if (!predicate(value)) {
        throw new SchemeTypeError(procName, argPos, typeName, value);
    }
    return value;
}

/**
 * Asserts value is a pair.
 * @param {string} procName - Procedure name
 * @param {number} argPos - Argument position (1-indexed)
 * @param {*} value - Value to check
 * @returns {Cons} The pair
 * @throws {SchemeTypeError}
 */
export function assertPair(procName, argPos, value) {
    return assertType(procName, argPos, value, isPair, 'pair');
}

/**
 * Asserts value is a proper list.
 * @param {string} procName - Procedure name
 * @param {number} argPos - Argument position (1-indexed)
 * @param {*} value - Value to check
 * @returns {Cons|null} The list
 * @throws {SchemeTypeError}
 */
export function assertList(procName, argPos, value) {
    return assertType(procName, argPos, value, isList, 'list');
}

/**
 * Asserts value is a number.
 * @param {string} procName - Procedure name
 * @param {number} argPos - Argument position (1-indexed)
 * @param {*} value - Value to check
 * @returns {number} The number
 * @throws {SchemeTypeError}
 */
export function assertNumber(procName, argPos, value) {
    return assertType(procName, argPos, value, isNumber, 'number');
}

/**
 * Asserts value is an integer.
 * @param {string} procName - Procedure name
 * @param {number} argPos - Argument position (1-indexed)
 * @param {*} value - Value to check
 * @returns {number} The integer
 * @throws {SchemeTypeError}
 */
export function assertInteger(procName, argPos, value) {
    return assertType(procName, argPos, value, isInteger, 'integer');
}

/**
 * Asserts value is a string.
 * @param {string} procName - Procedure name
 * @param {number} argPos - Argument position (1-indexed)
 * @param {*} value - Value to check
 * @returns {string} The string
 * @throws {SchemeTypeError}
 */
export function assertString(procName, argPos, value) {
    return assertType(procName, argPos, value, isString, 'string');
}

/**
 * Asserts value is a symbol.
 * @param {string} procName - Procedure name
 * @param {number} argPos - Argument position (1-indexed)
 * @param {*} value - Value to check
 * @returns {Symbol} The symbol
 * @throws {SchemeTypeError}
 */
export function assertSymbol(procName, argPos, value) {
    return assertType(procName, argPos, value, isSymbol, 'symbol');
}

/**
 * Asserts value is a vector.
 * @param {string} procName - Procedure name
 * @param {number} argPos - Argument position (1-indexed)
 * @param {*} value - Value to check
 * @returns {Vector} The vector
 * @throws {SchemeTypeError}
 */
export function assertVector(procName, argPos, value) {
    return assertType(procName, argPos, value, isVector, 'vector');
}

/**
 * Asserts value is a procedure.
 * @param {string} procName - Procedure name
 * @param {number} argPos - Argument position (1-indexed)
 * @param {*} value - Value to check
 * @returns {Function|Closure|Continuation} The procedure
 * @throws {SchemeTypeError}
 */
export function assertProcedure(procName, argPos, value) {
    return assertType(procName, argPos, value, isProcedure, 'procedure');
}

/**
 * Asserts correct number of arguments.
 * @param {string} procName - Procedure name
 * @param {Array} args - Arguments array
 * @param {number} min - Minimum required arguments
 * @param {number} [max=min] - Maximum allowed arguments (Infinity for variadic)
 * @throws {SchemeArityError}
 */
export function assertArity(procName, args, min, max = min) {
    const count = args.length;
    if (count < min || count > max) {
        throw new SchemeArityError(procName, min, max, count);
    }
}

/**
 * Asserts a numeric value is within a range.
 * @param {string} procName - Procedure name
 * @param {string} argName - Argument/parameter name
 * @param {number} value - Value to check
 * @param {number} min - Minimum valid value
 * @param {number} max - Maximum valid value
 * @returns {number} The value if valid
 * @throws {SchemeRangeError}
 */
export function assertRange(procName, argName, value, min, max) {
    if (value < min || value > max) {
        throw new SchemeRangeError(procName, argName, min, max, value);
    }
    return value;
}

/**
 * Asserts a valid index for a sized collection.
 * @param {string} procName - Procedure name
 * @param {number} index - Index to check
 * @param {number} size - Size of collection
 * @throws {SchemeRangeError}
 */
export function assertIndex(procName, index, size) {
    if (!Number.isInteger(index)) {
        throw new SchemeTypeError(procName, 2, 'exact integer', index);
    }
    if (index < 0 || index >= size) {
        throw new SchemeRangeError(procName, 'index', 0, size - 1, index);
    }
    return index;
}
