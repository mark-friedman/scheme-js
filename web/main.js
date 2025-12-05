import { createLayer1 } from '../src/layer-1-kernel/index.js';
import { setupRepl } from './repl.js';

// --- Main Entry Point ---

// Create the interpreter instance
const { interpreter, env } = createLayer1();

// Setup REPL UI
setupRepl(interpreter, globalEnv);
