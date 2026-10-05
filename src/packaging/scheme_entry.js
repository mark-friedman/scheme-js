import { createInterpreter } from '../core/interpreter/index.js';
import { parse } from '../core/interpreter/reader.js';
import { analyze } from '../core/interpreter/expand.js';
import { setFileResolver, setLibraryLoadHook, setLibraryRestorer, programEnvironment, runProgramForm } from '../core/interpreter/library_loader.js';
import { BUNDLED_SOURCES } from './bundled_libraries.js';
import { installLibraryTable, libraryRestorer } from '../compiler/prebuilt.js';
import { rememberSourceText } from '../core/interpreter/source_texts.js';
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
// So installing them runs no compiler. Nothing calls `new Function`, so a page
// with a strict Content-Security-Policy gets the compiled libraries rather
// than interpreted ones; and the compiler, most of what a bundle that can
// compile weighs, is not part of this one. It is a separate module, which the
// bundle loads once it has started, to compile the page's own code (below).
//
// A library written inline is the program's own code, not one this bundle
// ships, so the hook leaves it alone. So is anything a stale build no longer
// covers: it stays interpreted, which is a tier and not a failure.

/**
 * What installing each shipped library's prebuilt table did, keyed by the
 * library's name as written, such as `srfi 125`: the procedures restored from
 * it as the library loaded, without its source running, and those installed
 * over the closures its other forms made.
 * @type {Map<string, {installed: Array<string>, restored: Array<string>, skipped: Array<Object>, stale: boolean}>}
 */
export const libraryInstallation = new Map();

// Each shipped library is restored from its table: its procedures bound from
// their compiled code, its other forms -- macros, records, values -- run, and
// its source never read but to check the table was built from it.
setLibraryRestorer(libraryRestorer(prebuiltLibraries));

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
    interpreter.run(analyze(exp, env), env);
}

/**
 * The compiler's module while it is being fetched, so that it is fetched once.
 * @type {Promise<Object>|null}
 */
let compilerLoading = null;

/**
 * Loads the compiler. The bundle does so itself once it has started, so a page
 * need not call this except to wait for it, or to use the compiler directly.
 *
 * Asynchronous because the compiler is a separate module, fetched the first
 * time it is asked for; the bundle does not carry it. Once it is loaded, a
 * shipped library imported afterwards also has anything its prebuilt table did
 * not cover compiled as it loads.
 *
 * @returns {Promise<Object>} The compiler's entry points, as
 *   `src/packaging/scheme_compiler.js` exports them.
 */
export async function loadCompiler() {
  if (compilerLoading === null) {
    compilerLoading = import('./scheme_compiler.js').then((module) => {
      compiler = module;
      attachIfWanted();
      return module;
    });
  }
  return compilerLoading;
}

/**
 * Whether `loadCompiler` has loaded the compiler.
 * @returns {boolean} True once it has.
 */
export function isCompilerLoaded() {
  return compiler !== null;
}

// =============================================================================
// Compiling the page's own code
// =============================================================================
//
// The page's own procedures are compiled as it runs, by the compiler tier
// (`src/compiler/tiering.js`), once the compiler has arrived: until then the
// page's code runs interpreted, and what it defined meanwhile is taken on when
// the tier attaches. A page turns this off with `setUserCodeCompilation(false)`
// -- before the compiler arrives, and it never attaches -- and a page whose
// Content-Security-Policy forbids `new Function` runs interpreted regardless.

/**
 * Whether the page's own code is to be compiled.
 * @type {boolean}
 */
let userCodeCompilation = true;

/**
 * Whether a library has a prebuilt table installed over it as it loads, so
 * that the tier leaves its procedures alone.
 * @param {Array<string>} libraryName - The library's name.
 * @returns {boolean}
 */
function isPrebuilt(libraryName) {
  return BUNDLED_SOURCES[`${libraryName[libraryName.length - 1]}.sld`] !== undefined;
}

/**
 * Attaches the tier, if the compiler is here, compilation is wanted, and no
 * tier is attached yet.
 */
function attachIfWanted() {
  if (compiler !== null && userCodeCompilation && !interpreter.tier) {
    compiler.attachTier(interpreter, env, { isPrebuilt });
  }
}

/**
 * Turns compiling the page's own code on or off. Off, what is compiled stays
 * compiled and nothing more is; on, the compiler is loaded if it has not been.
 * @param {boolean} enabled - Whether to compile it.
 */
export function setUserCodeCompilation(enabled) {
  userCodeCompilation = enabled;
  if (!enabled) {
    if (compiler !== null) compiler.detachTier(interpreter);
    return;
  }
  if (compiler === null) loadCompiler();
  else attachIfWanted();
}

// =============================================================================
// Public API
// =============================================================================

/**
 * Internal helper to parse, analyze, and execute Scheme code.
 * @param {string} code - The Scheme source code.
 * @param {EvalOptions} [options] - How it is read (`schemeEval`).
 * @returns {*} The result of the evaluation.
 */
function evalCode(code, options = {}) {
    const { filename, inline = false, program = false } = options;
    // Nothing could fetch an inline script's text, so it is kept for the
    // source maps of what is compiled from it.
    if (inline && filename !== undefined) rememberSourceText(filename, code);
    // Form by form, as a program's top level is run, so that the compiler
    // tier sees each: a script run as one `begin` would be one form to it.
    const forms = parse(code, filename === undefined ? undefined : { filename });
    const run = program ? programEnvironment(forms, analyze, interpreter, env) : { env, forms };
    let result;
    for (const form of run.forms) result = runProgramForm(form, analyze, interpreter, run.env);
    return result;
}

/**
 * How code is read.
 * @typedef {Object} EvalOptions
 * @property {string} [filename] - The name it is read under: in error
 *   messages, a stack trace, and the source maps of what is compiled from it.
 *   A URL a debugger can fetch it from, where there is one.
 * @property {boolean} [inline=false] - Whether nothing could fetch the code by
 *   its name, as a page's inline script, so that its text is kept for a
 *   debugger (`sourceText`).
 * @property {boolean} [program=false] - Whether the code is a program, as a
 *   page's script is: one that begins with import declarations runs in an
 *   environment of what they import and nothing else (R7RS 5.1), and what it
 *   defines is its own. Otherwise it is run in the shared environment, which
 *   sees everything and keeps what the code imports and defines.
 */

/**
 * Evaluates Scheme code synchronously.
 * @param {string} code - The Scheme source code.
 * @param {EvalOptions} [options] - How it is read.
 * @returns {*} The result of the evaluation.
 */
export function schemeEval(code, options) {
    return evalCode(code, options);
}

/**
 * Evaluates Scheme code asynchronously.
 * Returns a Promise that resolves to the result.
 * @param {string} code - The Scheme source code.
 * @param {EvalOptions} [options] - How it is read.
 * @returns {Promise<*>} A promise resolving to the result.
 */
export function schemeEvalAsync(code, options) {
    return new Promise((resolve, reject) => {
        try {
            resolve(evalCode(code, options));
        } catch (e) {
            reject(e);
        }
    });
}

// Export the interpreter and environment for advanced usage (e.g. testing, extending)
export { interpreter, env };

// The text of an inline script, by the name it was read under, for a debugger.
export { sourceText } from '../core/interpreter/source_texts.js';

// JavaScript calling Scheme. A procedure called as a plain function converts
// its arguments into Scheme and its result out of it; these are the parts of
// that call, for JavaScript that holds Scheme values or converts them itself
// (`docs/Interoperability.md`, *Calling Scheme from JavaScript*).
export { callSchemeProcedure } from '../core/interpreter/values.js';
export { schemeToJs, schemeToJsDeep, jsToScheme, jsToSchemeDeep } from '../core/interpreter/js_interop.js';

// Export REPL utilities
export { parse } from '../core/interpreter/reader.js';
export { analyze } from '../core/interpreter/expand.js';
export { prettyPrint } from '../core/interpreter/printer.js';
export { isCompleteExpression, findMatchingDelimiter, delimiterParens } from '../core/interpreter/expression_utils.js';
export { SchemeDebugRuntime, ReplDebugBackend, ReplDebugCommands };

// The page's code is compiled from when the compiler arrives, unless the page
// said otherwise before this ran. Started after everything above, so that the
// bundle's own start-up does not wait for it.
loadCompiler().catch((e) => console.warn('scheme-js: the compiler did not load, so code runs interpreted:', e));
