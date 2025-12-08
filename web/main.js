import { createLayer1 } from '../src/runtime/index.js';
import { setupRepl } from './repl.js';

// --- Main Entry Point ---

// Create the interpreter instance
const { interpreter, env } = createLayer1();

// Setup REPL UI
setupRepl(interpreter, env);
