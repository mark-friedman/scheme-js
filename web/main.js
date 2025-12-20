import { createInterpreter } from '../src/core/interpreter/index.js';
import { setupRepl } from './repl.js';
import { setFileResolver, loadLibrary } from '../src/core/interpreter/library_loader.js';
import { analyze } from '../src/core/interpreter/analyzer.js';

// --- Main Entry Point ---

(async () => {
    // Create the interpreter instance
    const { interpreter, env } = createInterpreter();

    // 1. Setup browser-side file resolver
    // This allows the interpreter to load .sld and .scm files via fetch
    setFileResolver(async (libraryName) => {
        // libraryName is like ['scheme', 'base'] OR ['scheme', 'macros.scm']
        const fileName = libraryName[libraryName.length - 1];
        const baseDir = '../src/core/scheme/';

        // If it already has an extension (like macros.scm), try that first and only
        if (fileName.endsWith('.sld') || fileName.endsWith('.scm')) {
            const response = await fetch(baseDir + fileName);
            if (response.ok) return response.text();
            throw new Error(`Failed to load ${fileName}: ${response.statusText}`);
        }

        // Otherwise, try .sld then .scm (standard library trial)
        const sldResponse = await fetch(baseDir + fileName + '.sld');
        if (sldResponse.ok) return sldResponse.text();

        const scmResponse = await fetch(baseDir + fileName + '.scm');
        if (scmResponse.ok) return scmResponse.text();

        throw new Error(`Failed to load library ${libraryName.join('.')}: Not found`);
    });

    // 2. Bootstrap standard libraries
    // We want the REPL to have standard macros and procedures available.
    try {
        console.log("Bootstrapping REPL environment...");

        // Load (scheme repl) - this will also trigger loading (scheme base) due to dependency
        await loadLibrary(['scheme', 'repl'], analyze, interpreter, env);

        // Now that the libraries are loaded, we can use a standard Scheme import form
        // to populate the REPL environment. This is now supported by the analyzer.
        const importAst = analyze(['import', ['scheme', 'base'], ['scheme', 'repl']]);
        interpreter.run(importAst, env);

        console.log("REPL environment ready.");
    } catch (e) {
        console.error("Failed to bootstrap REPL environment:", e);
    }

    // Setup REPL UI
    setupRepl(interpreter, env);
})();
