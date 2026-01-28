/**
 * @fileoverview Dot notation property access for Scheme reader.
 * Handles JavaScript-style property access syntax (e.g., obj.prop.nested).
 */

import { list } from '../cons.js';
import { intern } from '../symbol.js';

/**
 * Handles dot property access chains after an expression.
 * Transforms `expr.prop1.prop2` into `(js-ref (js-ref expr "prop1") "prop2")`.
 * 
 * @param {*} expr - The base expression
 * @param {Array} tokens - Remaining tokens (may be consumed)
 * @returns {*} The expression with property accesses chained
 */
export function handleDotAccess(expr, tokens) {
    let currentExpr = expr;

    while (tokens.length > 0) {
        const nextToken = tokens[0];

        // Check if next token is a dot property access:
        // 1. Starts with dot
        // 2. Not just "." (improper list delimiter)
        // 3. NO preceding space (adjacent)
        if (nextToken.value.startsWith('.') &&
            nextToken.value !== '.' &&
            !nextToken.hasPrecedingSpace) {

            // Consume token
            tokens.shift();

            // Split property chain (e.g. .a.b -> ["", "a", "b"])
            const parts = nextToken.value.split('.');

            for (let i = 1; i < parts.length; i++) {
                const prop = parts[i];
                if (prop.length === 0) {
                    // Skip empty parts (consecutive dots or trailing dot)
                    continue;
                }
                currentExpr = list(intern('js-ref'), currentExpr, prop);
            }
        } else {
            break;
        }
    }
    return currentExpr;
}

/**
 * Builds a nested property access form from a chain like ["obj", "prop1", "prop2"].
 * Returns: (js-ref (js-ref obj "prop1") "prop2")
 * @param {string[]} parts - Array of property path segments
 * @returns {Cons|Symbol} The nested js-ref form
 */
export function buildPropertyAccessForm(parts) {
    // Start with the base object (first part as a symbol)
    let result = intern(parts[0]);

    // Chain property accesses for remaining parts
    for (let i = 1; i < parts.length; i++) {
        result = list(intern('js-ref'), result, parts[i]);
    }

    return result;
}
