import { Environment } from '../src/core/environment.js';
import { createGlobalEnvironment } from '../src/primitives/index.js';
import { Interpreter } from '../src/core/interpreter.js';
import { setupRepl } from './repl.js';

// --- Main Entry Point ---

// Create the interpreter instance
const interpreter = new Interpreter();
const globalEnv = createGlobalEnvironment(interpreter);
interpreter.setGlobalEnv(globalEnv);

// Setup REPL UI
setupRepl(interpreter, globalEnv);
