/**
 * Syntax Object Primitives for Scheme.
 * 
 * Provides procedures for working with syntax objects.
 */

import { unwrapSyntax, SyntaxObject } from '../interpreter/syntax_object.js';

// =============================================================================
// Syntax Primitives
// =============================================================================

/**
 * Syntax primitives exported to Scheme.
 */
export const syntaxPrimitives = {
    /**
     * Converts a syntax object to its datum representation.
     * Recursively unwraps syntax objects in lists and vectors.
     * @param {*} stx - A syntax object or datum.
     * @returns {*} The unwrapped datum.
     */
    'syntax->datum': (stx) => unwrapSyntax(stx),

    /**
     * Checks if a value is a syntax object.
     * @param {*} obj - Value to check.
     * @returns {boolean} True if obj is a syntax object.
     */
    'syntax-object?': (obj) => obj instanceof SyntaxObject
};

// Mark primitives that should receive raw Scheme objects
syntaxPrimitives['syntax->datum'].skipBridge = true;
syntaxPrimitives['syntax-object?'].skipBridge = true;
