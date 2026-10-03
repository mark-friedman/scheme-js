#!/usr/bin/env node

import repl from 'repl';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

import { createInterpreter } from './src/core/interpreter/index.js';
import { setFileResolver, setLibraryLoadHook, setLibraryRestorer } from './src/core/interpreter/library_loader.js';
import { libraryNameToKey } from './src/core/interpreter/library_registry.js';
import { installLibraryTable, libraryRestorer } from './src/compiler/prebuilt.js';
import { attachTier } from './src/compiler/tiering.js';
import prebuiltLibraries from './src/packaging/compiled_libraries.js';
import { analyze } from './src/core/interpreter/analyzer.js';
import { parse } from './src/core/interpreter/reader.js';
import { SchemeReadError } from './src/core/interpreter/errors.js';
import { Cons, toArray, cdr, car } from './src/core/interpreter/cons.js';
import { Symbol } from './src/core/interpreter/symbol.js';
import { Closure, Continuation, callSchemeProcedure, NO_VALUES } from './src/core/interpreter/values.js';
import { LiteralNode } from './src/core/interpreter/ast.js';

import { prettyPrint } from './src/core/interpreter/printer.js';
import { SchemeDebugRuntime } from './src/debug/scheme_debug_runtime.js';
import { ReplDebugBackend } from './src/debug/repl_debug_backend.js';
import { ReplDebugCommands } from './src/debug/repl_debug_commands.js';
import readline from 'readline';



// --- Interpreter Setup ---

/**
 * The directories the shipped libraries' files are read from.
 * @type {Array<string>}
 */
const LIBRARY_DIRS = [path.join(__dirname, 'src/core/scheme'), path.join(__dirname, 'src/extras/scheme')];

/**
 * One of a shipped library's files, as loaded from disk, for checking a
 * prebuilt table against what was actually loaded.
 * @param {string} file - The file's name, as its library's table lists it.
 * @returns {string|undefined} Its source, or undefined if there is none.
 */
function shippedSource(file) {
    for (const dir of LIBRARY_DIRS) {
        const p = path.join(dir, file);
        if (fs.existsSync(p)) return fs.readFileSync(p, 'utf8');
    }
    return undefined;
}

/**
 * Whether a library has a prebuilt table, installed over it as it loads, so
 * that the compiler tier leaves its procedures alone.
 * @param {Array<string>} name - The library's name.
 * @returns {boolean}
 */
function isPrebuilt(name) {
    return prebuiltLibraries[libraryNameToKey(name)] !== undefined;
}

/**
 * Creates the interpreter and loads the standard libraries, their prebuilt
 * compiled code installed as each loads, as a browser page installs it.
 * @returns {Promise<{interpreter: Object, env: Object}>}
 */
async function bootstrapInterpreter() {
    const { interpreter, env } = createInterpreter();

    // A shipped library is restored from its table, without its source
    // running, and what the table holds for whatever its other forms made is
    // installed as it loads. A table whose sources no longer match the files,
    // as after editing one without rebuilding, leaves that library to load
    // from source, interpreted.
    setLibraryRestorer(libraryRestorer(prebuiltLibraries));
    setLibraryLoadHook((libraryName, libraryEnv) => {
        if (libraryEnv) installLibraryTable(prebuiltLibraries, libraryName, libraryEnv, shippedSource);
    });

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
        const exprs = parse(code, { filename });
        let result;
        for (const exp of exprs) {
            const ast = analyze(exp);
            result = interpreter.runTopLevel(ast, env);
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
                    (scheme-js interop)
                    (scheme-js define-macro))
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
    // `--no-compile` leaves the program's own code interpreted; the standard
    // library is compiled either way, as it ships.
    const args = process.argv.slice(2).filter((arg) => arg !== '--no-compile');
    const compile = !process.argv.slice(2).includes('--no-compile');

    const { interpreter, env } = await bootstrapInterpreter();
    if (compile) attachTier(interpreter, env, { isPrebuilt });

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

        rl.on('line', async (line) => {
            line = line.trim();
            if (line === '') {
                rl.prompt();
                return;
            }

            if (commands.isDebugCommand(line)) {
                const output = await commands.execute(line);
                console.log(output);

                const cmd = line.slice(1).split(/\s+/)[0].toLowerCase();
                const resumeCmds = ['continue', 'c', 'step', 's', 'next', 'n', 'finish', 'fin', 'abort', 'a'];
                if (resumeCmds.includes(cmd)) {
                    rl.close();
                } else {
                    rl.prompt();
                }
            } else {
                const output = await commands.execute(':eval ' + line);
                console.log(output);
                rl.prompt();
            }
        });

        rl.on('close', () => {
            // Nested loop closed, presumably because we are resuming
        });

        rl.prompt();
    });



    if (args.length > 0) {
        // A program's current ports are the process's standard input, output
        // and error, so it can stand in a pipeline. What it leaves in standard
        // output is written as the process exits, however it exits. The
        // interactive REPL below keeps the console ports: standard input is
        // where its own input comes from, through Node's readline.
        // Each is a Scheme procedure, called with Scheme values, so through
        // the call that converts nothing: a plain call would make an
        // integral inexact result exact before writing it.
        const scheme = (name) => (...args) => callSchemeProcedure(env.lookup(name), args);
        scheme('current-input-port')(scheme('standard-input-port')());
        scheme('current-output-port')(scheme('standard-output-port')());
        scheme('current-error-port')(scheme('standard-error-port')());
        const write = scheme('write');
        const display = scheme('display');
        const newline = scheme('newline');
        const errorPort = scheme('current-error-port')();

        // Handle -e "expression"
        if (args[0] === '-e') {
            const code = args[1];
            if (!code) {
                display("Error: -e requires an argument", errorPort);
                newline(errorPort);
                process.exit(1);
            }
            try {
                const sexps = parse(code);
                let result;
                for (const sexp of sexps) {
                    result = interpreter.runTopLevel(analyze(sexp), env, { jsAutoConvert: 'raw' });
                }
                // The last result as `write` writes it, after what the program
                // wrote, unless it is unspecified.
                if (result !== undefined && result !== NO_VALUES) {
                    write(result);
                    newline();
                }
                process.exit(0);
            } catch (e) {
                display(e.message, errorPort);
                newline(errorPort);
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
                display(`Error executing ${filePath}: ${e.message}`, errorPort);
                newline(errorPort);
                process.exit(1);
            }
        }
    }

    /**
     * Writes what an evaluation typed at the REPL displayed and did not end
     * with a newline, so that it is seen with the evaluation's result rather
     * than when something next ends a line.
     */
    const flushOutput = () => {
        const flush = env.lookup('flush-output-port');
        callSchemeProcedure(flush, []);
        callSchemeProcedure(flush, [callSchemeProcedure(env.lookup('current-error-port'), [])]);
    };

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
                const output = await commands.execute(cmd);
                return callback(null, output);
            }

            try {
                // If already paused (unlikely to get here if nested loop is active),
                // treat as eval.
                if (backend.isPaused()) {
                    const output = commands.execute(':eval ' + cmd);
                    return callback(null, output);
                }

                // Read all of the input before evaluating any of it. Input
                // that ends inside a datum is continued: Node's REPL prompts
                // for another line and calls this again with both. Only an
                // error reading the input is, never one evaluating raises,
                // which a `read` or `load` of incomplete text can.
                let sexps;
                try {
                    sexps = parse(cmd, { suppressLog: true });
                } catch (e) {
                    return callback(isRecoverableError(e) ? new repl.Recoverable(e) : e);
                }

                isEvaluating = true;
                let result;
                for (const sexp of sexps) {
                    // Check for Fast Mode (Debug Off)
                    if (runtime && !runtime.enabled) {
                        // FAST MODE: Synchronous execution for performance
                        result = interpreter.runTopLevel(analyze(sexp), env, { jsAutoConvert: 'raw' });
                    } else {
                        // DEBUG MODE: Asynchronous execution for breakpoints/stepping
                        result = await interpreter.runAsync(analyze(sexp), env, { jsAutoConvert: 'raw' });
                    }
                }
                flushOutput();
                callback(null, result);

            } catch (e) {
                flushOutput();
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

/**
 * Whether reading the REPL's input failed only because the input ended inside
 * a datum -- a list not closed, a string, |symbol| or block comment not ended,
 * a quote with nothing after it -- so that another line could complete it.
 * @param {*} error - What reading the input threw.
 * @returns {boolean}
 */
function isRecoverableError(error) {
    return error instanceof SchemeReadError && error.incomplete;
}

startRepl();
