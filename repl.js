#!/usr/bin/env node

import repl from 'repl';
import inspector from 'inspector';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

import { createInterpreter } from './src/core/interpreter/index.js';
import { setFileResolver, setLibraryLoadHook, setLibraryRestorer, programEnvironment, runProgramForm } from './src/core/interpreter/library_loader.js';
import { libraryNameToKey } from './src/core/interpreter/library_registry.js';
import { installLibraryTable, libraryRestorer } from './src/compiler/prebuilt.js';
import { attachTier } from './src/compiler/tiering.js';
import prebuiltLibraries from './src/packaging/compiled_libraries.js';
import prebuiltCompiler from './src/packaging/compiled_compiler.js';
import { registerCompilerHost } from './src/compiler/host.js';
import { registerBuildHost } from './src/compiler/build_host.js';
import { writeProgramBundle } from './src/packaging/ahead_bundle.js';
import { stringValue } from './src/core/primitives/string_class.js';
import { analyze } from './src/core/interpreter/expand.js';
import { parse } from './src/core/interpreter/reader.js';
import { SchemeReadError } from './src/core/interpreter/errors.js';
import { Cons, toArray, cdr, car, list } from './src/core/interpreter/cons.js';
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
 * The directories the shipped libraries' files are read from, and the
 * compiler's, which a program run from the CLI may import as any library.
 * @type {Array<string>}
 */
const LIBRARY_DIRS = ['src/core/scheme', 'src/extras/scheme', 'src/compiler'].map((dir) => path.join(__dirname, dir));

/**
 * The prebuilt tables a library is restored from: the shipped libraries', and
 * the compiler's.
 * @type {Object<string, Object>}
 */
const TABLES = { ...prebuiltLibraries, ...prebuiltCompiler };

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
    return TABLES[libraryNameToKey(name)] !== undefined;
}

/**
 * Whether a path names a file.
 * @param {string} p - The path.
 * @returns {boolean}
 */
function isFile(p) {
    return fs.existsSync(p) && fs.statSync(p).isFile();
}

/**
 * Where the file the library system asks for by a path lies: in the first
 * directory that has it, as the path joined (`scheme/macros.scm`), with
 * `.sld` (`scheme/base.sld`) or `.scm`, or by its last part alone, with
 * `.sld` or as it is.
 * @param {Array<string>} parts - The path: a library's name, or a file a
 *   library includes.
 * @param {Array<string>} searchDirs - The directories, in the order they are
 *   looked in.
 * @returns {string|null} The file's path, or null.
 */
function findLibraryFile(parts, searchDirs) {
    const relativePath = parts.join('/');
    const fileName = parts[parts.length - 1];
    for (const dir of searchDirs) {
        const candidates = [relativePath, relativePath + '.sld', relativePath + '.scm', fileName + '.sld', fileName]
            .map((name) => path.join(dir, name));
        const found = candidates.find(isFile);
        if (found !== undefined) return found;
    }
    return null;
}

/**
 * Where a file a library includes lies beside the library's own file, if it
 * does there: as most Schemes find an included file, relative to the file
 * that includes it.
 * @param {Array<string>} parts - The file's path (`include-path` in
 *   src/core/scheme/library_system.scm).
 * @param {Array<string>} library - The library's name, as the library system
 *   asks for its own file.
 * @param {Array<string>} searchDirs - The directories, in the order they are
 *   looked in.
 * @returns {string|null} The file's path, or null.
 */
function besideLibrary(parts, library, searchDirs) {
    const home = findLibraryFile(library, searchDirs);
    if (home === null) return null;
    const beside = path.join(path.dirname(home), parts[parts.length - 1]);
    return isFile(beside) ? beside : null;
}

/**
 * Creates the interpreter and loads the standard libraries, their prebuilt
 * compiled code installed as each loads, as a browser page installs it.
 * @param {Array<string>} [includeDirs=[]] - Directories a library is looked
 *   for in before any other, as `-I` names them.
 * @returns {Promise<{interpreter: Object, env: Object}>}
 */
async function bootstrapInterpreter(includeDirs = []) {
    const { interpreter, env } = createInterpreter();

    // What a program that imports the compiler's library needs that no
    // library file holds: what the compiler asks of the interpreter, and what
    // the build steps and the compiler's harnesses ask of the host.
    registerCompilerHost(env);
    registerBuildHost(env);

    // A shipped library is restored from its table, without its source
    // running, and what the table holds for whatever its other forms made is
    // installed as it loads. A table whose sources no longer match the files,
    // as after editing one without rebuilding, leaves that library to load
    // from source, interpreted.
    setLibraryRestorer(libraryRestorer(TABLES));
    setLibraryLoadHook((libraryName, libraryEnv) => {
        if (libraryEnv) installLibraryTable(TABLES, libraryName, libraryEnv, shippedSource);
    });

    // Setup synchronous file resolver for Node.js. A file a library
    // includes is looked for beside the library's own file first, so that
    // one named like it in a directory searched earlier -- the current one,
    // or a -I one -- is not taken for it.
    setFileResolver((libraryName, library) => {
        const searchDirs = [
            ...includeDirs,
            process.cwd(),
            path.join(process.cwd(), 'src/core/scheme'),
            path.join(__dirname, 'src/core/scheme'),
            path.join(__dirname, 'src/compiler'),
            // Extension libraries (non-R7RS)
            path.join(process.cwd(), 'src/extras/scheme'),
            path.join(__dirname, 'src/extras/scheme'),
            // Add test directories for compliance checking
            path.join(process.cwd(), 'tests/core/scheme/compliance/chibi_original'),
            path.join(process.cwd(), 'tests/core/scheme/compliance/chibi_revised')
        ];
        const file = (library && besideLibrary(libraryName, library, searchDirs))
            ?? findLibraryFile(libraryName, searchDirs);
        if (file === null) throw new Error(`Library not found: ${libraryName.join(' ')}`);
        return fs.readFileSync(file, 'utf8');
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
            const ast = analyze(exp, env);
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

// --- Building a program ahead of time ---

/**
 * Compiles a program ahead of time, with what it uses of each library, into
 * one ES module that runs it with no interpreter (`--build PROGRAM -o
 * OUTPUT`): the build, `build-program-file` in `(scheme-js ahead)`
 * (scripts/lib/ahead.scm), writes the program's table or says why it cannot
 * run so, and the table is made one file with its runtime
 * (src/packaging/ahead_bundle.js). Runs nothing of the program.
 * @param {string[]} args - The arguments after the options.
 * @param {string[]} includeDirs - The `-I` directories, where the program's
 *   libraries are looked for after its own directory.
 * @returns {Promise<never>} Exits: 0 with the file written, 1 with the
 *   reasons the program is refused, or why it could not be read or
 *   expanded, on standard error.
 */
async function buildAhead(args, includeDirs) {
    const program = args[1];
    const output = args[2] === '-o' ? args[3] : undefined;
    if (program === undefined || output === undefined) {
        console.error('usage: node repl.js --build PROGRAM -o OUTPUT');
        process.exit(1);
    }
    const { interpreter, env } = await bootstrapInterpreter([...includeDirs, path.join(__dirname, 'scripts/lib')]);
    const ahead = programEnvironment(parse('(import (scheme-js ahead))'), analyze, interpreter, env).env;
    const dirs = [path.dirname(path.resolve(program)), ...includeDirs,
        path.join(__dirname, 'src/core/scheme'), path.join(__dirname, 'src/extras/scheme')];
    let outcome;
    try {
        outcome = callSchemeProcedure(ahead.lookup('build-program-file'), [program, list(...dirs)]);
    } catch (e) {
        // A program that cannot be read or expanded, as running it would say.
        console.error(`Error building ${program}: ${e.message}`);
        process.exit(1);
    }
    if (outcome instanceof Cons) {
        for (const reason of toArray(outcome)) console.error(stringValue(reason));
        process.exit(1);
    }
    await writeProgramBundle(stringValue(outcome), output);
    process.exit(0);
}

// --- REPL Logic ---

/**
 * Whether Node's inspector is on, as `--inspect` and `--inspect-brk` turn it
 * on, so that a debugger may attach.
 * @returns {boolean}
 */
function inspectorOn() {
    return inspector.url() !== undefined;
}

async function startRepl() {
    // `--no-compile` leaves the program's own code interpreted; the standard
    // library is compiled either way, as it ships. Each `-I dir` names a
    // directory libraries are looked for in first, as other Schemes' CLIs do.
    // `--devtools` has every procedure compiled as it is defined, as it is
    // when Node's inspector is on unless `--no-devtools` says otherwise:
    // DevTools steps only into compiled code, and the tier compiles a
    // procedure only at its second call (src/compiler/tier.scm).
    const options = process.argv.slice(2);
    const includeDirs = [];
    const args = [];
    const flags = ['--no-compile', '--devtools', '--no-devtools'];
    for (let i = 0; i < options.length; i++) {
        if (options[i] === '-I' && i + 1 < options.length) includeDirs.push(path.resolve(options[++i]));
        else if (!flags.includes(options[i])) args.push(options[i]);
    }
    const compile = !options.includes('--no-compile');
    const forDevTools = options.includes('--devtools') || (inspectorOn() && !options.includes('--no-devtools'));

    if (args[0] === '--build') return buildAhead(args, includeDirs);

    const { interpreter, env } = await bootstrapInterpreter(includeDirs);
    if (compile) attachTier(interpreter, env, { isPrebuilt, eager: forDevTools });

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

        // A program that begins with import declarations sees only them; one
        // with none sees everything the REPL does.
        const runProgram = (sexps) => {
            const program = programEnvironment(sexps, analyze, interpreter, env);
            let result;
            for (const sexp of program.forms) {
                result = runProgramForm(sexp, analyze, interpreter, program.env, { jsAutoConvert: 'raw' });
            }
            return result;
        };

        // Handle -e "expression"
        if (args[0] === '-e') {
            const code = args[1];
            if (!code) {
                display("Error: -e requires an argument", errorPort);
                newline(errorPort);
                process.exit(1);
            }
            try {
                const result = runProgram(parse(code));
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
                const filename = path.resolve(process.cwd(), filePath);
                if (!fs.existsSync(filename)) throw new Error(`file not found: ${filename}`);
                runProgram(parse(fs.readFileSync(filename, 'utf8'), { filename }));
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
                        result = interpreter.runTopLevel(analyze(sexp, env), env, { jsAutoConvert: 'raw' });
                    } else {
                        // DEBUG MODE: Asynchronous execution for breakpoints/stepping
                        result = await interpreter.runAsync(analyze(sexp, env), env, { jsAutoConvert: 'raw' });
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
