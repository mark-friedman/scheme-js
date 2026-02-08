import { createInterpreter } from '../core/interpreter/index.js';
import { parse } from '../core/interpreter/reader.js';
import { analyze } from '../core/interpreter/analyzer.js';
import { list } from '../core/interpreter/cons.js';
import { intern } from '../core/interpreter/symbol.js';
import { setFileResolver } from '../core/interpreter/library_loader.js';
import { BUNDLED_SOURCES } from './bundled_libraries.js';
import {
    SchemeDebugRuntime,
    ReplDebugBackend,
    ReplDebugCommands
} from '../debug/index.js';

// Create a single shared interpreter and environment instance
const { interpreter, env } = createInterpreter();

// =============================================================================
// Bootstrap: Load standard libraries from embedded sources
// =============================================================================

// Set file resolver to read from embedded sources (no file I/O needed)
setFileResolver((libraryName) => {
    const fileName = libraryName[libraryName.length - 1];

    // Try .sld then .scm
    if (BUNDLED_SOURCES[fileName + '.sld']) return BUNDLED_SOURCES[fileName + '.sld'];
    if (BUNDLED_SOURCES[fileName + '.scm']) return BUNDLED_SOURCES[fileName + '.scm'];
    if (BUNDLED_SOURCES[fileName]) return BUNDLED_SOURCES[fileName];

    throw new Error(`Library not found in bundled sources: ${libraryName.join('/')}`);
});

// Load standard libraries via import statement
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

// =============================================================================
// Public API
// =============================================================================

/**
 * Internal helper to parse, analyze, and execute Scheme code.
 * @param {string} code - The Scheme source code.
 * @returns {*} The result of the evaluation.
 */
function evalCode(code) {
    const asts = parse(code);
    if (asts.length === 0) return undefined;

    let ast;
    if (asts.length === 1) {
        ast = analyze(asts[0]);
    } else {
        ast = analyze(list(intern('begin'), ...asts));
    }

    return interpreter.run(ast);
}

/**
 * Evaluates Scheme code synchronously.
 * @param {string} code - The Scheme source code.
 * @returns {*} The result of the evaluation.
 */
export function schemeEval(code) {
    return evalCode(code);
}

/**
 * Evaluates Scheme code asynchronously.
 * Returns a Promise that resolves to the result.
 * @param {string} code - The Scheme source code.
 * @returns {Promise<*>} A promise resolving to the result.
 */
export function schemeEvalAsync(code) {
    return new Promise((resolve, reject) => {
        try {
            resolve(evalCode(code));
        } catch (e) {
            reject(e);
        }
    });
}

// Export the interpreter and environment for advanced usage (e.g. testing, extending)
export { interpreter, env };

// Export REPL utilities
export { parse } from '../core/interpreter/reader.js';
export { analyze } from '../core/interpreter/analyzer.js';
export { prettyPrint } from '../core/interpreter/printer.js';
export { isCompleteExpression, findMatchingDelimiter } from '../core/interpreter/expression_utils.js';
export { SchemeDebugRuntime, ReplDebugBackend, ReplDebugCommands };

