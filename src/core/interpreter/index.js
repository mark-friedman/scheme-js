import { Interpreter } from './interpreter.js';
import { createGlobalEnvironment } from '../primitives/index.js';
// import { LibraryRegistry } from './library.js'; // Unused now
import { registerBuiltinLibrary, createPrimitiveExports } from './library_loader.js';

/**
 * Factory for the Scheme interpreter core.
 * @returns {{interpreter: Interpreter, env: Environment}}
 */
export function createInterpreter() {
    const interpreter = new Interpreter();
    const env = createGlobalEnvironment(interpreter);

    // Initialize the Global Environment first
    interpreter.setGlobalEnv(env);

    // Register (scheme primitives)
    // This allows library files to (import (scheme primitives))
    const primitiveExports = createPrimitiveExports(env);
    registerBuiltinLibrary(['scheme', 'primitives'], primitiveExports, env);

    return { interpreter, env };
}
