import { createInterpreter } from '../core/interpreter/index.js';
import { parse } from '../core/interpreter/reader.js';
import { analyze } from '../core/interpreter/analyzer.js';
import { list } from '../core/interpreter/cons.js';
import { intern } from '../core/interpreter/symbol.js';
import { setFileResolver, setLibraryLoadHook } from '../core/interpreter/library_loader.js';
import { BUNDLED_SOURCES } from './bundled_libraries.js';
import { installLibraryTable } from '../compiler/prebuilt.js';
import prebuiltLibraries from './compiled_libraries.js';
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

// =============================================================================
// Compiled libraries
// =============================================================================
//
// The libraries are themselves Scheme, so `map`, `assq` and `member` are
// interpreted closures until something compiles them, and interpreted they
// cost their callers about 10x -- 17x on every SRFI 125 lookup. Every library
// this bundle ships was compiled at build time instead, and each one's code is
// installed into it as it loads: the standard library below, and a library
// imported long after start-up alike.
//
// So nothing here runs the compiler. Nothing calls `new Function`, so a page
// with a strict Content-Security-Policy gets the compiled libraries rather
// than interpreted ones; and the compiler, most of what a bundle that can
// compile weighs, is not part of this one. It is a separate module, loaded by
// `loadCompiler` for a page that wants to compile code of its own.
//
// A library written inline is the program's own code, not one this bundle
// ships, so the hook leaves it alone. So is anything a stale build no longer
// covers: it stays interpreted, which is a tier and not a failure.

/**
 * What installing each shipped library's prebuilt table did, keyed by the
 * library's name as written, such as `srfi 125`.
 * @type {Map<string, {installed: Array<string>, skipped: Array<Object>, stale: boolean}>}
 */
export const libraryInstallation = new Map();

/**
 * The compiler, once `loadCompiler` has loaded it.
 * @type {Object|null}
 */
let compiler = null;

setLibraryLoadHook((libraryName, libraryEnv) => {
  const fileName = libraryName[libraryName.length - 1];
  if (BUNDLED_SOURCES[`${fileName}.sld`] === undefined || !libraryEnv) return;
  const outcome = installLibraryTable(
    prebuiltLibraries, libraryName, libraryEnv, (file) => BUNDLED_SOURCES[file]);
  if (outcome !== null) libraryInstallation.set(libraryName.join(' '), outcome);
  // With the compiler loaded, whatever the table did not cover need not stay
  // interpreted. `compileEnvironment` sees only closures still interpreted, so
  // it does not redo what was installed.
  if (compiler !== null) compiler.compileEnvironment(libraryEnv);
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

/**
 * Loads the compiler, for a page that wants to compile code of its own.
 *
 * Asynchronous because the compiler is a separate module, fetched the first
 * time it is asked for; the bundle does not carry it. Once it is loaded, a
 * shipped library imported afterwards also has anything its prebuilt table did
 * not cover compiled as it loads.
 *
 * @returns {Promise<Object>} The compiler's entry points: `compileProgram`,
 *   `compileEnvironment`, `tryCompileDefinition` and `tryCompileClosure`, as
 *   `src/compiler/index.js` documents them.
 */
export async function loadCompiler() {
  if (compiler === null) compiler = await import('./scheme_compiler.js');
  return compiler;
}

/**
 * Whether `loadCompiler` has loaded the compiler.
 * @returns {boolean} True once it has.
 */
export function isCompilerLoaded() {
  return compiler !== null;
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

