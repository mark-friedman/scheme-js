#!/usr/bin/env node

import repl from 'repl';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

import { createInterpreter } from './src/core/interpreter/index.js';
import { setFileResolver, loadLibrary, loadLibrarySync, parseImportSet, applyImports, parseDefineLibrary, evaluateLibraryDefinition } from './src/core/interpreter/library_loader.js';
import { analyze } from './src/core/interpreter/analyzer.js';
import { parse } from './src/core/interpreter/reader.js';
import { Cons, toArray, cdr, car } from './src/core/interpreter/cons.js';
import { Symbol } from './src/core/interpreter/symbol.js';
import { Closure, Continuation } from './src/core/interpreter/values.js';
import { LiteralNode } from './src/core/interpreter/ast.js';

import { prettyPrint } from './src/core/interpreter/printer.js';
import { SchemeDebugRuntime } from './src/core/debug/scheme_debug_runtime.js';
import { ReplDebugBackend } from './src/core/debug/repl_debug_backend.js';
import { ReplDebugCommands } from './src/core/debug/repl_debug_commands.js';
import readline from 'readline';



// --- Interpreter Setup ---

async function bootstrapInterpreter() {
    const { interpreter, env } = createInterpreter();

    // Setup synchronous file resolver for Node.js
    setFileResolver((libraryName) => {
        const parts = libraryName;
        // Search paths:
        // 1. Current directory
        // 2. src/core/scheme/

        const relativePath = parts.join('/');
        const fileName = parts[parts.length - 1];

        const searchDirs = [
            process.cwd(),
            path.join(process.cwd(), 'src/core/scheme'),
            path.join(__dirname, 'src/core/scheme'),
            // Extension libraries (non-R7RS)
            path.join(process.cwd(), 'src/extras/scheme'),
            path.join(__dirname, 'src/extras/scheme'),
            // Add test directories for compliance checking
            path.join(process.cwd(), 'tests/core/scheme/compliance/chibi_original'),
            path.join(process.cwd(), 'tests/core/scheme/compliance/chibi_revised')
        ];

        for (const dir of searchDirs) {
            // Check exact match (e.g. scheme/macros.scm)
            let p = path.join(dir, relativePath);
            if (fs.existsSync(p) && fs.statSync(p).isFile()) return fs.readFileSync(p, 'utf8');

            // Check .sld (e.g. scheme/base.sld)
            p = path.join(dir, relativePath + '.sld');
            if (fs.existsSync(p) && fs.statSync(p).isFile()) return fs.readFileSync(p, 'utf8');

            // Check .scm
            p = path.join(dir, relativePath + '.scm');
            if (fs.existsSync(p) && fs.statSync(p).isFile()) return fs.readFileSync(p, 'utf8');

            // Check flat filename .sld
            p = path.join(dir, fileName + '.sld');
            if (fs.existsSync(p) && fs.statSync(p).isFile()) return fs.readFileSync(p, 'utf8');

            // Check flat filename exact
            p = path.join(dir, fileName);
            if (fs.existsSync(p) && fs.statSync(p).isFile()) return fs.readFileSync(p, 'utf8');
        }

        throw new Error(`Library not found: ${libraryName.join(' ')}`);
    });

    // Define 'load' primitive
    env.define('load', (filename) => {
        if (typeof filename !== 'string') {
            throw new Error("load: argument must be a string");
        }
        if (!path.isAbsolute(filename)) {
            filename = path.resolve(process.cwd(), filename);
        }
        if (!fs.existsSync(filename)) {
            throw new Error(`load: file not found: ${filename}`);
        }
        const code = fs.readFileSync(filename, 'utf8');
        const exprs = parse(code);
        let result;
        for (const exp of exprs) {
            const ast = analyze(exp);
            result = interpreter.run(ast, env);
        }
        return result;
    });

    // Load standard libraries via import statement
    try {
        // Import R7RS-small libraries and scheme-js extras
        const imports = `
            (import (scheme base)
                    (scheme write)
                    (scheme read)
                    (scheme repl)
                    (scheme lazy)
                    (scheme case-lambda)
                    (scheme eval)
                    (scheme time)
                    (scheme complex)
                    (scheme cxr)
                    (scheme char)
                    (scheme file)
                    (scheme process-context)
                    (scheme-js promise)
                    (scheme-js interop))
        `;
        for (const sexp of parse(imports)) {
            interpreter.run(analyze(sexp), env, [], undefined, { jsAutoConvert: 'raw' });
        }
    } catch (e) {
        console.error("Failed to bootstrap REPL environment:", e);
        process.exit(1);
    }

    return { interpreter, env };
}

// --- REPL Logic ---

async function startRepl() {
    const { interpreter, env } = await bootstrapInterpreter();

    // Initialize Debugger
    const runtime = new SchemeDebugRuntime();
    const backend = new ReplDebugBackend(console.log);
    const commands = new ReplDebugCommands(interpreter, runtime, backend);
    runtime.setBackend(backend);
    interpreter.setDebugRuntime(runtime);

    let isEvaluating = false;

    // Add 'pause' primitive for manual debugging
    // Register primitives
    env.define('pause', (source = null, env = null, reason = 'manual pause') => {
        interpreter.debugRuntime?.pause(source, env, reason);
    });

    // Setup nested loop for Node.js REPL when paused
    backend.setOnPause((info) => {
        const rl = readline.createInterface({
            input: process.stdin,
            output: process.stdout,
            prompt: 'debug> '
        });

        rl.on('line', (line) => {
            line = line.trim();
            if (line === '') {
                rl.prompt();
                return;
            }

            if (commands.isDebugCommand(line)) {
                const output = commands.execute(line);
                console.log(output);

                const cmd = line.slice(1).split(/\s+/)[0].toLowerCase();
                const resumeCmds = ['continue', 'c', 'step', 's', 'next', 'n', 'finish', 'fin'];
                if (resumeCmds.includes(cmd)) {
                    rl.close();
                } else {
                    rl.prompt();
                }
            } else {
                const output = commands.execute(':eval ' + line);
                console.log(output);
                rl.prompt();
            }
        });

        rl.on('close', () => {
            // Nested loop closed, presumably because we are resuming
        });

        rl.prompt();
    });



    // Check command line args
    const args = process.argv.slice(2);

    if (args.length > 0) {
        // Handle -e "expression"
        if (args[0] === '-e') {
            const code = args[1];
            if (!code) {
                console.error("Error: -e requires an argument");
                process.exit(1);
            }
            try {
                const sexps = parse(code);
                let result;
                for (const sexp of sexps) {
                    result = interpreter.run(analyze(sexp), env);
                }
                console.log(prettyPrint(result));
                process.exit(0);
            } catch (e) {
                console.error(e.message);
                process.exit(1);
            }
        }
        // Handle file execution
        else {
            const filePath = args[0];
            try {
                // Use the 'load' primitive defined in bootstrap
                const loadProc = env.lookup('load');
                loadProc(filePath);
                process.exit(0);
            } catch (e) {
                console.error(`Error executing ${filePath}:`, e.message);
                process.exit(1);
            }
        }
    }

    // Start Interactive REPL
    console.log('Welcome to Scheme-JS');

    const replInstance = repl.start({
        prompt: '> ',
        eval: async (cmd, context, filename, callback) => {
            if (isEvaluating) return;

            cmd = cmd.trim();
            if (cmd === '') {
                return callback(null);
            }

            // Handle immediate debug commands
            if (commands.isDebugCommand(cmd)) {
                const output = commands.execute(cmd);
                return callback(null, output);
            }

            try {
                // If already paused (unlikely to get here if nested loop is active),
                // treat as eval.
                if (backend.isPaused()) {
                    const output = commands.execute(':eval ' + cmd);
                    return callback(null, output);
                }

                // Attempt to parse first to catch syntax errors early and check for recoverability
                // We parse SYNCHRONOUSLY here to detect syntax errors before running
                const sexps = parse(cmd);

                isEvaluating = true;
                let result;
                for (const sexp of sexps) {
                    // Check for Fast Mode (Debug Off)
                    if (runtime && !runtime.enabled) {
                        // FAST MODE: Synchronous execution for performance
                        result = interpreter.run(analyze(sexp), env, [], undefined, { jsAutoConvert: 'raw' });
                    } else {
                        // DEBUG MODE: Asynchronous execution for breakpoints/stepping
                        result = await interpreter.runAsync(analyze(sexp), env, { jsAutoConvert: 'raw' });
                    }
                }
                callback(null, result);

            } catch (e) {
                if (isRecoverableError(e)) {
                    return callback(new repl.Recoverable(e));
                }
                callback(e);
            } finally {
                isEvaluating = false;
            }
        },
        writer: (output) => {
            if (output === undefined) return '';
            if (typeof output === 'string' && output.startsWith(';;')) return output;
            return prettyPrint(output);
        }
    });
}

function isRecoverableError(error) {
    const msg = error.message;
    return msg === 'Unexpected EOF' ||
        msg.includes("Missing ')'") ||
        msg === 'Unterminated string';
}

startRepl();
