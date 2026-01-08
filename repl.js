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
import { findMatchingParen } from './src/core/interpreter/expression_utils.js';



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

    // Load standard libraries
    try {
        // Use synchronous loading
        loadLibrarySync(['scheme', 'repl'], analyze, interpreter, env);
        loadLibrarySync(['scheme', 'complex'], analyze, interpreter, env);

        // Import them into global environment
        const importAst = analyze(['import', ['scheme', 'base'], ['scheme', 'repl'], ['scheme', 'write'], ['scheme', 'complex']]);
        interpreter.run(importAst, env);
    } catch (e) {
        console.error("Failed to bootstrap REPL environment:", e);
        process.exit(1);
    }

    return { interpreter, env };
}

// --- REPL Logic ---

async function startRepl() {
    const { interpreter, env } = await bootstrapInterpreter();

    // Standard eval helper
    const schemeEval = (code) => {
        const s_exps = parse(code);
        let result;
        for (const exp of s_exps) {
            const ast = analyze(exp);
            result = interpreter.run(ast, env);
        }
        return result;
    };

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
                const result = schemeEval(code);
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
    console.log('Welcome to Scheme-JS-4 REPL');

    // Shared state for pending input across lines
    let pendingInput = "";

    const replServer = repl.start({
        prompt: '> ',
        eval: (cmd, context, filename, callback) => {
            // cmd contains the full accumulated command including newlines
            const trimmedCmd = cmd.trim();
            if (trimmedCmd === '') {
                pendingInput = "";
                return callback(null);
            }

            try {
                const result = schemeEval(trimmedCmd);
                // Success - clear pending
                pendingInput = "";
                callback(null, result);
            } catch (e) {
                if (isRecoverableError(e)) {
                    // Recoverable - update pending input
                    pendingInput = cmd;
                    return callback(new repl.Recoverable(e));
                }
                // Error - clear pending
                pendingInput = "";
                callback(e);
            }
        },
        writer: (output) => {
            if (output === undefined) return '';
            return prettyPrint(output);
        }
    });

    setupParenMatching(replServer, () => pendingInput);
}

function setupParenMatching(replServer, getPendingInput) {
    if (!process.stdin.isTTY) return;

    // Listen to keypress on the input stream
    process.stdin.on('keypress', (str, key) => {
        // We only care about matching when we type something
        performMatch(replServer, getPendingInput());
    });
}

// Helper for visual matching
function performMatch(replServer, pendingInput) {
    // Wait for readline to process key and update line/cursor
    setImmediate(() => {
        const currentLine = replServer.line;
        const cursor = replServer.cursor;

        if (cursor <= 0) return;

        // Re-read line and cursor in case they changed rapidly
        const charToCheck = currentLine[cursor - 1];
        if (charToCheck !== ')' && charToCheck !== '"') return;

        const fullCode = pendingInput + currentLine;
        const globalIndex = pendingInput.length + (cursor - 1);

        const match = findMatchingParen(fullCode, globalIndex);

        if (match) {
            // Visual jump
            // match.line and match.column are relative to fullCode.
            // We need to convert to relative screen coordinates.

            // 1. Calculate how many lines up we need to go.
            // Current line index in fullCode?
            // pendingInput ends with newline if not empty.
            // Number of newlines in pendingInput = number of previous lines.
            const previousLinesCount = (pendingInput.match(/\n/g) || []).length;
            const currentLineIndex = previousLinesCount; // 0-based index of current line

            // match.line is 0-based line number in fullCode.
            const linesUp = currentLineIndex - match.line;

            // 2. Calculate column.
            // match.column is 0-based column index.
            // We need to account for prompt length!
            // Prompt: '> ' (2 chars) or '... ' (4 chars)?
            // Default REPL uses '> ' for first line, '... ' for others.
            // If match.line == 0, prompt is '> '.
            // If match.line > 0, prompt is '... '.

            let promptLen = 2; // '> '
            if (match.line > 0) {
                 promptLen = 4; // '... ' (usually, unless customized)
                 // Node default is '... '
            }

            // Target column on screen
            const targetCol = promptLen + match.column;

            // Current cursor position on screen
            // replServer prompt len
            const currentPromptLen = (previousLinesCount > 0) ? 4 : 2;
            const currentCol = currentPromptLen + cursor;

            // ANSI Escape Codes
            // Save cursor position: \x1B7 (DEC) or \x1B[s (ANSI)
            // Restore cursor position: \x1B8 (DEC) or \x1B[u (ANSI)
            // Move cursor up: \x1B[<n>A
            // Move cursor horizontal: \x1B[<n>G (1-based column)

            const stream = replServer.outputStream;

            // We use standard ANSI escapes
            // Save cursor
            stream.write('\x1B7');

            // Move to target
            if (linesUp > 0) {
                stream.write(`\x1B[${linesUp}A`);
            }
            stream.write(`\x1B[${targetCol + 1}G`); // 1-based

            // Highlight/Hold
            // We can't pause the thread (it freezes UI).
            // We can restore after a timeout.

            setTimeout(() => {
                stream.write('\x1B8');
            }, 350); // Increased duration to be more noticeable
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
