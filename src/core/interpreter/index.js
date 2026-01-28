import { Interpreter } from './interpreter.js';
import { createGlobalEnvironment } from '../primitives/index.js';
import { registerBuiltinLibrary, createPrimitiveExports } from './library_loader.js';
import { InterpreterContext, globalContext } from './context.js';

/**
 * Options for creating an interpreter instance.
 * @typedef {Object} CreateInterpreterOptions
 * @property {boolean} [isolated=false] - If true, creates a fresh context isolated from other interpreters
 * @property {InterpreterContext} [context] - Explicit context to use (overrides isolated flag)
 */

/**
 * Factory for the Scheme interpreter core.
 * 
 * @param {CreateInterpreterOptions} [options={}] - Configuration options
 * @returns {{interpreter: Interpreter, env: Environment, context: InterpreterContext}}
 * 
 * @example
 * // Default: uses shared global context
 * const { interpreter, env } = createInterpreter();
 * 
 * // Isolated: creates fresh context (e.g., for sandboxed REPL)
 * const { interpreter, env, context } = createInterpreter({ isolated: true });
 * 
 * // Explicit context: use your own context instance
 * const myCtx = new InterpreterContext();
 * const { interpreter, env } = createInterpreter({ context: myCtx });
 */
export function createInterpreter(options = {}) {
    // Determine which context to use
    let context;
    if (options.context) {
        context = options.context;
    } else if (options.isolated) {
        context = new InterpreterContext();
    } else {
        context = globalContext;
    }

    const interpreter = new Interpreter(context);
    const env = createGlobalEnvironment(interpreter);

    // Initialize the Global Environment first
    interpreter.setGlobalEnv(env);

    // Register (scheme primitives)
    // This allows library files to (import (scheme primitives))
    const primitiveExports = createPrimitiveExports(env);
    registerBuiltinLibrary(['scheme', 'primitives'], primitiveExports, env);

    return { interpreter, env, context };
}

// Re-export InterpreterContext for convenience
export { InterpreterContext, globalContext };
