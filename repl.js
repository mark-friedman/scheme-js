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
        // Use synchronous loading to ensure libraries are registered
        // We load (scheme base) explicitly to avoid issues with implicit loading during import
        loadLibrarySync(['scheme', 'base'], analyze, interpreter, env);
        loadLibrarySync(['scheme', 'repl'], analyze, interpreter, env);
        loadLibrarySync(['scheme', 'read'], analyze, interpreter, env);
        loadLibrarySync(['scheme', 'write'], analyze, interpreter, env);
        loadLibrarySync(['scheme', 'complex'], analyze, interpreter, env);
        loadLibrarySync(['scheme-js', 'interop'], analyze, interpreter, env);
        loadLibrarySync(['scheme-js', 'promise'], analyze, interpreter, env);

        // Import (scheme repl) into global environment
        // Since (scheme repl) now exports everything from base + extras, this is sufficient.
        const importExprs = parse('(import (scheme repl))');
        for (const exp of importExprs) {
            const importAst = analyze(exp);
            interpreter.run(importAst, env);
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

    repl.start({
        prompt: '> ',
        eval: (cmd, context, filename, callback) => {
            cmd = cmd.trim();
            if (cmd === '') {
                return callback(null);
            }

            try {
                const result = schemeEval(cmd);
                callback(null, result);
            } catch (e) {
                if (isRecoverableError(e)) {
                    return callback(new repl.Recoverable(e));
                }
                callback(e);
            }
        },
        writer: (output) => {
            if (output === undefined) return '';
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
