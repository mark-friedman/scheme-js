/**
 * JavaScript Interop Primitives for Scheme.
 * 
 * Provides JavaScript evaluation capability.
 */

import { assertString } from '../interpreter/type_check.js';

/**
 * Interop primitives exported to Scheme.
 */
export const interopPrimitives = {
    /**
     * Evaluates a string as JavaScript code.
     * @param {string} code - JavaScript code to evaluate.
     * @returns {*} Result of evaluation.
     */
    'js-eval': (code) => {
        assertString('js-eval', 1, code);
        return (1, eval)(code);
    }
};
