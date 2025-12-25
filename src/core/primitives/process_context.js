/**
 * Process Context Primitives
 * 
 * R7RS (scheme process-context) procedures.
 * Provides environment-aware implementations (Node.js vs browser).
 */

import { assertArity, assertString, assertInteger } from '../interpreter/type_check.js';

// Check if running in Node.js
const isNode = typeof process !== 'undefined' && process.versions && process.versions.node;

export const processContextPrimitives = {
    /**
     * command-line: Returns a list of command line arguments.
     * R7RS requires no arguments.
     * @returns {Array} List of command line arguments.
     */
    'command-line': (...args) => {
        assertArity('command-line', args, 0, 0);
        if (isNode) {
            return process.argv;
        }
        // Browser: return empty list with location as program name
        return [typeof location !== 'undefined' ? location.href : 'scheme'];
    },

    /**
     * exit: Exit the program with an optional status code.
     * R7RS allows 0 or 1 argument.
     * @param {number} [code=0] - Exit status code (must be exact integer).
     */
    'exit': (...args) => {
        assertArity('exit', args, 0, 1);
        let code = 0;
        if (args.length === 1) {
            // R7RS allows #t (success=0) or #f (failure=1) or exact integer
            if (args[0] === true) {
                code = 0;
            } else if (args[0] === false) {
                code = 1;
            } else {
                assertInteger('exit', 1, args[0]);
                code = args[0];
            }
        }
        if (isNode) {
            process.exit(code);
        }
        // Browser: no direct equivalent, throw an error
        throw new Error(`Program exit requested with code ${code}`);
    },

    /**
     * emergency-exit: Exit immediately without cleanup.
     * R7RS allows 0 or 1 argument.
     * @param {number} [code=0] - Exit status code.
     */
    'emergency-exit': (...args) => {
        assertArity('emergency-exit', args, 0, 1);
        let code = 0;
        if (args.length === 1) {
            if (args[0] === true) {
                code = 0;
            } else if (args[0] === false) {
                code = 1;
            } else {
                assertInteger('emergency-exit', 1, args[0]);
                code = args[0];
            }
        }
        if (isNode) {
            process.exit(code);
        }
        throw new Error(`Emergency exit requested with code ${code}`);
    },

    /**
     * get-environment-variable: Get an environment variable.
     * R7RS requires exactly 1 string argument.
     * @param {string} name - Variable name.
     * @returns {string|boolean} Value or #f if not found.
     */
    'get-environment-variable': (...args) => {
        assertArity('get-environment-variable', args, 1, 1);
        assertString('get-environment-variable', 1, args[0]);
        const name = args[0];
        if (isNode && process.env) {
            const value = process.env[name];
            return value !== undefined ? value : false;
        }
        return false;
    },

    /**
     * get-environment-variables: Get all environment variables as an alist.
     * R7RS requires no arguments.
     * @returns {Array} Association list of (name . value) pairs.
     */
    'get-environment-variables': (...args) => {
        assertArity('get-environment-variables', args, 0, 0);
        if (isNode && process.env) {
            return Object.entries(process.env);
        }
        return [];
    }
};
