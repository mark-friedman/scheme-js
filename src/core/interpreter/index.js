import { Interpreter } from './interpreter.js';
import { createGlobalEnvironment } from '../primitives/index.js';
import { LibraryRegistry } from './library.js';

/**
 * Factory for the Scheme interpreter core.
 * @returns {{interpreter: Interpreter, env: Environment}}
 */
export function createInterpreter() {
    const interpreter = new Interpreter();
    const env = createGlobalEnvironment(interpreter);

    // Initialize the Micro-Library system
    interpreter.libraries = new LibraryRegistry();

    interpreter.setGlobalEnv(env);
    return { interpreter, env };
}
