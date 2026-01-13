import { createInterpreter } from '../src/core/interpreter/index.js';
import { setupRepl } from './repl.js';
import { setFileResolver } from '../src/core/interpreter/library_loader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { parse } from '../src/core/interpreter/reader.js';
import { prettyPrint } from '../src/core/interpreter/printer.js';
import { isCompleteExpression, findMatchingDelimiter } from '../src/core/interpreter/expression_utils.js';

// --- Main Entry Point ---

(async () => {
    // Create the interpreter instance
    const { interpreter, env } = createInterpreter();

    // 1. Setup browser-side file resolver
    // This allows the interpreter to load .sld and .scm files via fetch
    setFileResolver(async (libraryName) => {
        // libraryName is like ['scheme', 'base'] OR ['scheme', 'macros.scm']
        const fileName = libraryName[libraryName.length - 1];
        const searchDirs = ['../src/core/scheme/', '../src/extras/scheme/'];

        // If it already has an extension (like macros.scm), try that first
        if (fileName.endsWith('.sld') || fileName.endsWith('.scm')) {
            for (const dir of searchDirs) {
                const response = await fetch(dir + fileName);
                if (response.ok) return response.text();
            }
            throw new Error(`Failed to load ${fileName}: Not found`);
        }

        // Otherwise, try .sld then .scm in both directories
        for (const dir of searchDirs) {
            const sldResponse = await fetch(dir + fileName + '.sld');
            if (sldResponse.ok) return sldResponse.text();

            const scmResponse = await fetch(dir + fileName + '.scm');
            if (scmResponse.ok) return scmResponse.text();
        }

        throw new Error(`Failed to load library ${libraryName.join('.')}: Not found`);
    });

    // 2. Bootstrap standard libraries
    try {
        console.log("Bootstrapping REPL environment...");

        // Import R7RS-small libraries and scheme-js extras
        // Excludes (scheme file) and (scheme process-context) which require Node.js
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
                    (scheme-js promise)
                    (scheme-js interop))
        `;
        for (const exp of parse(imports)) {
            interpreter.run(analyze(exp), env);
        }

        console.log("REPL environment ready.");
    } catch (e) {
        console.error("Failed to bootstrap REPL environment:", e);
    }

    // Setup REPL UI
    setupRepl(interpreter, env, document, {
        parse,
        analyze,
        prettyPrint,
        isCompleteExpression,
        findMatchingDelimiter
    });
})();
