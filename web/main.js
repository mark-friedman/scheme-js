import { createInterpreter } from '../src/core/interpreter/index.js';
import { setupRepl } from './repl.js';
import { setFileResolver, setLibraryLoadHook, loadLibrary } from '../src/core/interpreter/library_loader.js';
import { libraryNameToKey } from '../src/core/interpreter/library_registry.js';
import { installLibraryTable } from '../src/compiler/prebuilt.js';
import { attachTier } from '../src/compiler/tiering.js';
import prebuiltLibraries from '../src/packaging/compiled_libraries.js';
import { analyze } from '../src/core/interpreter/analyzer.js';
import { parse } from '../src/core/interpreter/reader.js';
import { prettyPrint } from '../src/core/interpreter/printer.js';
import { isCompleteExpression, findMatchingDelimiter, delimiterParens } from '../src/core/interpreter/expression_utils.js';
import {
    SchemeDebugRuntime,
    ReplDebugBackend,
    ReplDebugCommands
} from '../src/debug/index.js';

// --- Main Entry Point ---

(async () => {
    // Create the interpreter instance
    const { interpreter, env } = createInterpreter();

    // Initialize debugger. Attached rather than assigned, so that the runtime
    // can run compiled code as its closures while the program is debugged.
    const debugRuntime = new SchemeDebugRuntime();
    interpreter.setDebugRuntime(debugRuntime);

    // Add 'pause' primitive for debugging
    env.define('pause', (source = null, env = null, reason = 'manual pause') => {
        debugRuntime.pause(source, env, reason);
    });

    // Each shipped library's prebuilt code is installed as it loads, as the
    // bundle installs it, checked against the files actually fetched: one
    // edited since the last build leaves its library interpreted.
    const fetched = new Map();
    setLibraryLoadHook((libraryName, libraryEnv) => {
        if (libraryEnv) installLibraryTable(prebuiltLibraries, libraryName, libraryEnv, (file) => fetched.get(file));
    });
    const remember = async (response, fileName) => {
        const text = await response.text();
        fetched.set(fileName, text);
        return text;
    };

    // 1. Setup browser-side file resolver
    // This allows the interpreter to load .sld and .scm files via fetch
    setFileResolver(async (libraryName) => {
        // libraryName is like ['scheme', 'base'] OR ['scheme', 'macros.scm']
        const fileName = libraryName[libraryName.length - 1];
        const namespace = libraryName[0];

        // Determine search directories based on namespace
        // scheme/* libraries are in core, scheme-js/* are in extras
        let searchDirs;
        if (namespace === 'scheme-js') {
            searchDirs = ['../src/extras/scheme/', '../src/core/scheme/'];
        } else {
            searchDirs = ['../src/core/scheme/', '../src/extras/scheme/'];
        }

        // If it already has an extension (like macros.scm), try that first
        if (fileName.endsWith('.sld') || fileName.endsWith('.scm')) {
            for (const dir of searchDirs) {
                const response = await fetch(dir + fileName);
                if (response.ok) return remember(response, fileName);
            }
            throw new Error(`Failed to load ${fileName}: Not found`);
        }

        // Otherwise, try .sld then .scm in both directories
        for (const dir of searchDirs) {
            const sldResponse = await fetch(dir + fileName + '.sld');
            if (sldResponse.ok) return remember(sldResponse, fileName + '.sld');

            const scmResponse = await fetch(dir + fileName + '.scm');
            if (scmResponse.ok) return remember(scmResponse, fileName + '.scm');
        }

        throw new Error(`Failed to load library ${libraryName.join('.')}: Not found`);
    });

    // 2. Bootstrap standard libraries
    try {
        console.log("Bootstrapping REPL environment...");

        // List of libraries to pre-load asynchronously
        // The loadLibrary function caches them, so subsequent sync lookups will work
        const librariesToLoad = [
            ['scheme', 'base'],
            ['scheme', 'write'],
            ['scheme', 'read'],
            ['scheme', 'repl'],
            ['scheme', 'lazy'],
            ['scheme', 'case-lambda'],
            ['scheme', 'eval'],
            ['scheme', 'time'],
            ['scheme', 'complex'],
            ['scheme', 'cxr'],
            ['scheme', 'char'],
            ['scheme-js', 'promise'],
            ['scheme-js', 'interop'],
            ['scheme-js', 'define-macro']
        ];

        // Pre-load all libraries asynchronously (this populates the cache)
        for (const libName of librariesToLoad) {
            await loadLibrary(libName, analyze, interpreter, env);
        }

        // Now run the import statement synchronously - libraries are cached
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
                    (scheme-js interop)
                    (scheme-js define-macro))
        `;
        for (const exp of parse(imports)) {
            interpreter.run(analyze(exp), env);
        }

        // The program's own procedures are compiled as it runs.
        attachTier(interpreter, env, {
            isPrebuilt: (name) => prebuiltLibraries[libraryNameToKey(name)] !== undefined
        });

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
        findMatchingDelimiter,
        delimiterParens,
        ReplDebugBackend,
        ReplDebugCommands,
        SchemeDebugRuntime
    });
})();
