import { createInterpreter } from '../src/core/interpreter/index.js';
import { setupRepl } from './repl.js';

// --- Main Entry Point ---

// Create the interpreter instance
const { interpreter, env } = createInterpreter();

// Setup REPL UI
setupRepl(interpreter, env);
