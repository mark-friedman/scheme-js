/**
 * Scheme Error Classes
 * 
 * Provides structured exception types for the Scheme interpreter.
 * All Scheme exceptions inherit from SchemeError.
 */

import { Cons, list } from './cons.js';
import { Symbol } from './symbol.js';

/**
 * Returns a human-readable type name for a value.
 * @param {*} value - Any value
 * @returns {string} Type description
 */
export function getTypeName(value) {
    if (value === null) return 'null';
    if (value === undefined) return 'undefined';
    if (typeof value === 'boolean') return 'boolean';
    if (typeof value === 'number') return 'number';
    if (typeof value === 'string') return 'string';
    if (typeof value === 'function') return 'procedure';
    if (value instanceof Symbol) return 'symbol';
    if (value instanceof Cons) return 'pair';
    if (Array.isArray(value)) return 'vector';
    if (value.constructor && value.constructor.name) {
        return value.constructor.name.toLowerCase();
    }
    return typeof value;
}

/**
 * Base class for all Scheme exceptions.
 * Captures stack trace and supports R7RS "irritants" concept.
 */
export class SchemeError extends Error {
    /**
     * @param {string} message - Error message
     * @param {Array} irritants - R7RS irritants (additional values related to error)
     * @param {string} [procName] - Name of procedure that raised the error
     */
    constructor(message, irritants = [], procName = null) {
        super(message);
        this.name = 'SchemeError';
        this.irritants = irritants;
        this.procName = procName;
        this.schemeStack = []; // Populated by interpreter when raised

        // Capture JS stack trace
        if (Error.captureStackTrace) {
            Error.captureStackTrace(this, this.constructor);
        }
    }

    /**
     * Returns the irritants as a Scheme list.
     * @returns {Cons|null}
     */
    getIrritantsList() {
        return list(...this.irritants);
    }

    /**
     * Formats the complete error with Scheme stack if available.
     * @returns {string}
     */
    toSchemeString() {
        let result = this.message;
        if (this.schemeStack && this.schemeStack.length > 0) {
            result += '\n  Scheme stack:';
            for (const frame of this.schemeStack) {
                result += `\n    at ${frame}`;
            }
        }
        return result;
    }
}

/**
 * Error for type mismatches in procedure arguments.
 */
export class SchemeTypeError extends SchemeError {
    /**
     * @param {string} procName - Procedure name
     * @param {number} argPos - Argument position (1-indexed)
     * @param {string} expectedType - Expected type name
     * @param {*} actualValue - The actual value received
     */
    constructor(procName, argPos, expectedType, actualValue) {
        const actualType = getTypeName(actualValue);
        const message = `${procName}: expected ${expectedType} at argument ${argPos}, got ${actualType}`;
        super(message, [actualValue], procName);
        this.name = 'SchemeTypeError';
        this.argPos = argPos;
        this.expectedType = expectedType;
        this.actualValue = actualValue;
    }
}

/**
 * Error for wrong number of arguments.
 */
export class SchemeArityError extends SchemeError {
    /**
     * @param {string} procName - Procedure name
     * @param {number} expectedMin - Minimum expected arguments
     * @param {number} expectedMax - Maximum expected arguments (Infinity for variadic)
     * @param {number} actualCount - Actual argument count
     */
    constructor(procName, expectedMin, expectedMax, actualCount) {
        let expected;
        if (expectedMin === expectedMax) {
            expected = `${expectedMin}`;
        } else if (expectedMax === Infinity) {
            expected = `at least ${expectedMin}`;
        } else {
            expected = `${expectedMin}-${expectedMax}`;
        }
        const message = `${procName}: wrong number of arguments (expected ${expected}, got ${actualCount})`;
        super(message, [], procName);
        this.name = 'SchemeArityError';
        this.expectedMin = expectedMin;
        this.expectedMax = expectedMax;
        this.actualCount = actualCount;
    }
}

/**
 * Error for index or value out of range.
 */
export class SchemeRangeError extends SchemeError {
    /**
     * @param {string} procName - Procedure name
     * @param {string} argName - Argument or parameter name
     * @param {number} minVal - Minimum valid value
     * @param {number} maxVal - Maximum valid value
     * @param {number} actualVal - Actual value received
     */
    constructor(procName, argName, minVal, maxVal, actualVal) {
        const message = `${procName}: ${argName} out of range [${minVal}, ${maxVal}], got ${actualVal}`;
        super(message, [actualVal], procName);
        this.name = 'SchemeRangeError';
        this.argName = argName;
        this.minVal = minVal;
        this.maxVal = maxVal;
        this.actualVal = actualVal;
    }
}
