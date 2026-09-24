/**
 * @fileoverview Running the compiler's Scheme: lowering and code generation.
 *
 * Lowering lives in `ir.scm` and code generation in `emit.scm` and the files
 * beside it. This module is the door into both: it keeps the Scheme
 * interpreter they run in, marshals an analyzed AST across, and hands back the
 * IR, which stays Scheme data, and then the generated JavaScript.
 *
 * ## Why the pass is Scheme
 *
 * Because the compiler is meant to be. A Scheme compiler good enough to compile
 * a Scheme compiler is the goal, and it cannot be argued from priors -- it has
 * to be run. Written in JavaScript, the lowering was fast and told us nothing
 * about the language it compiles; written in Scheme it is the compiler's own
 * first customer, and every cost it pays is a cost a real program pays.
 *
 * It pays about 16x against the JavaScript it replaced, measured over 952
 * lambdas from the canonical benchmarks and the standard library, and that cost
 * is almost entirely off the path a user waits on: the standard library is
 * lowered at build time, and a program's definitions are lowered once each.
 *
 * ## The compiler is a library
 *
 * Its Scheme is the library `(scheme-js compiler)`, in `compiler.sld`, which
 * imports what it is written with -- `(scheme base)`, SRFI 1, SRFI 152 --
 * includes its files in order, and exports the entry points this module calls.
 * Which files make up the compiler, and in what order, is therefore said once,
 * in Scheme, where the build step that compiles them reads it too.
 *
 * ## The bootstrap, and why it terminates
 *
 * Lowering is what the compiler uses to produce code, so a compiler written in
 * the language it compiles has to start somewhere. It starts in the
 * interpreter, which can load the library from source with no compiler at all.
 * That is slow -- around 300x the JavaScript figure -- so it is not where it
 * stays:
 *
 *   1. The interpreter loads the library. The compiler now works, slowly.
 *   2. With it, the build compiles every library the bundle ships into
 *      `src/packaging/compiled_libraries.js`.
 *   3. With those, the build compiles the compiler's library into
 *      `src/packaging/compiled_compiler.js`. The compiler has compiled itself.
 *
 * Both tables are installed as their libraries load, and only after each
 * library's source has been interpreted, so a stale or missing build costs
 * speed and never correctness: `installLibraryTable` checks a fingerprint of
 * each library's sources and installs nothing from a table that has moved on.
 *
 * Step 2 is not an optimization of step 3. Lowering calls `memq` and `assq` on
 * every scope lookup and every global it records, and those are themselves
 * Scheme; with the library interpreted, compiling the compiler is worth 1.5x,
 * and with it compiled, 20x. The order matters more than either step.
 *
 * ## Libraries of its own
 *
 * The compiler loads its library, and the libraries that one imports, into a
 * registry of its own and an interpreter of its own, built once and shared.
 * It cannot share the program's: the program may have redefined a procedure of
 * `(scheme base)`, need not have loaded a standard library at all, and may be
 * a build step compiling those very libraries, which must see them load
 * rather than find the compiler's instances already there.
 */

import { createInterpreter } from '../core/interpreter/index.js';
import { analyze } from '../core/interpreter/analyzer.js';
import { loadLibrarySync } from '../core/interpreter/library_loader.js';
import { withPrivateLibraries, getLibraryEnv } from '../core/interpreter/library_registry.js';
import { BUNDLED_SOURCES } from '../packaging/bundled_libraries.js';
import { COMPILER_SOURCES } from '../packaging/compiler_sources.js';
import prebuiltLibraries from '../packaging/compiled_libraries.js';
import prebuiltCompiler from '../packaging/compiled_compiler.js';
import { installLibraryTable } from './prebuilt.js';
import { invoke, settle } from './runtime.js';
import { astToScheme, irToJs, toArray, toList } from './marshal.js';
import { intern } from '../core/interpreter/symbol.js';

/**
 * The compiler's library.
 * @type {string[]}
 */
export const COMPILER_LIBRARY = ['scheme-js', 'compiler'];

/**
 * The source of a file the compiler's libraries are made of: one of its own,
 * or one of a library the bundle ships.
 * @param {string} file - A file name, such as `ir.scm` or `1.sld`.
 * @returns {string|undefined} Its source, if there is such a file.
 */
export function compilerSourceOf(file) {
  return COMPILER_SOURCES[file] ?? BUNDLED_SOURCES[file];
}

/**
 * Finds a library's file, or a file a library includes, by the last part of
 * its name, as every resolver here does.
 * @param {string[]} name - A library name, or an include's path.
 * @returns {string} Its source.
 * @throws {Error} If there is no such file.
 */
function resolve(name) {
  const last = name[name.length - 1];
  const source = compilerSourceOf(`${last}.sld`) ?? compilerSourceOf(last);
  if (source === undefined) throw new Error(`the compiler cannot find ${name.join('/')}`);
  return source;
}

/**
 * The compiler, bootstrapped on first use, or null if it could not be.
 * @type {{interpreter: Object, env: Object, lowerLambda: Function,
 *   generateUnit: Function, jsName: Function, inlineExpansionNames: Function,
 *   controlGlobals: Set<string>, inlineNames?: Array<string>}|null}
 */
let pass = null;

/**
 * Why the bootstrap failed, if it did.
 *
 * A compiler that cannot start is not an error the caller should have to
 * handle: every entry point here already reports "this could not be lowered"
 * and leaves the definition to the interpreter, which is a tier and not a
 * fallback. So a failed bootstrap declines everything, once, with a reason that
 * says what happened.
 *
 * @type {string|null}
 */
let bootstrapFailure = null;

/**
 * Loads the compiler's library, and finds its entry points.
 *
 * Each library installs its prebuilt table as it loads, after its source has
 * run. The source is what defines the macros the analyzer needs and the
 * closures the prebuilt code replaces, so installing over it is a substitution
 * rather than a definition -- which is what lets the fingerprint check fail
 * towards leaving a procedure alone.
 *
 * @returns {Object} The interpreter, the library's environment, and what is
 *   read from its exports up front.
 */
function bootstrap() {
  const tables = { ...prebuiltLibraries, ...prebuiltCompiler };
  return withPrivateLibraries({
    resolver: resolve,
    hook: (name, env) => installLibraryTable(tables, name, env, compilerSourceOf)
  }, () => {
    const { interpreter, env } = createInterpreter();
    const exports = loadLibrarySync(COMPILER_LIBRARY, analyze, interpreter, env);

    // The control-global list is read across once, here, rather than asked
    // for per procedure. It is a membership test on a fixed list of fourteen
    // names, and marshalling a set of globals into Scheme to run it there
    // would cost more than the test. Reading it from `ir.scm` is what keeps it
    // one list: the lowering and the caller that declines on it cannot drift
    // apart if neither owns a second copy.
    return {
      interpreter,
      env: getLibraryEnv(COMPILER_LIBRARY),
      lowerLambda: exports.get('lower-lambda'),
      generateUnit: exports.get('generate-unit'),
      jsName: exports.get('js-name'),
      inlineExpansionNames: exports.get('inline-expansion-names'),
      controlGlobals: new Set(toArray(exports.get('control-globals')).map((s) => s.name))
    };
  });
}

/**
 * The compiler, bootstrapping it if this is the first call.
 * @returns {Object|null} What `bootstrap` returns, or null if it could not be
 *   built.
 */
function lowering() {
  if (pass !== null || bootstrapFailure !== null) return pass;
  try {
    pass = bootstrap();
  } catch (e) {
    bootstrapFailure = e.message ?? String(e);
  }
  return pass;
}

/**
 * Starts the compiler if it has not started, and says why it could not.
 *
 * Every entry point declines rather than throws when the compiler cannot
 * start, which is right for a program -- it runs interpreted -- and wrong for
 * a build step, which would otherwise write tables with nothing in them and
 * report success.
 *
 * @returns {string|null} Why the compiler could not start, or null if it did.
 */
export function compilerStartFailure() {
  lowering();
  return bootstrapFailure;
}

/**
 * Calls a Scheme procedure with Scheme values and waits for an answer.
 *
 * `invoke` picks the raw entry point where there is one, so this works whether
 * the procedure is interpreted or compiled -- which is exactly the state this
 * module cannot predict, since it depends on whether the prebuilt code matched.
 *
 * @param {Function} proc - A Scheme procedure.
 * @param {Array<*>} args - Scheme values.
 * @returns {*} Its result.
 */
function call(proc, args) {
  return settle(invoke(proc, args));
}

/**
 * Lowers a lambda to IR, reporting what it references.
 *
 * Lowering failure and *safety* are kept apart, because they are different
 * questions with different answers. A form the compiler cannot express is a
 * failure and is reported as `reason`. A form it can express but should not
 * compile -- because a continuation may be captured during its extent -- is a
 * judgement the caller makes, and needs more than this one lambda to make: see
 * `controlGlobalIn` for the local part of it and `compileProgram` for the
 * call-graph closure over it.
 *
 * @param {Object} lambdaNode - An analyzed `LambdaNode`.
 * @returns {{schemeIr: *, ir: Object, globals: Set<string>, callsUnknown: boolean,
 *   captures: boolean}|{reason: string}} The IR -- as Scheme data for code
 *   generation, and as JavaScript objects on request -- with what it
 *   references, or why it could not be lowered.
 */
export function lowerLambda(lambdaNode) {
  const scheme = lowering();
  if (scheme === null) {
    return { reason: `the Scheme lowering could not start: ${bootstrapFailure}` };
  }

  const result = toArray(call(scheme.lowerLambda, [astToScheme(lambdaNode)]));
  if (result[0].name === 'fail') return { reason: result[1] };

  const schemeIr = result[1];
  return {
    schemeIr,
    // The IR as JavaScript objects, for tests that inspect it. Code generation
    // reads the Scheme IR directly, so the conversion is only made if asked.
    get ir() { return irToJs(schemeIr); },
    globals: new Set(toArray(result[2]).map((symbol) => symbol.name)),
    callsUnknown: result[3],
    captures: result[4]
  };
}

/**
 * Generates a lowered procedure's JavaScript with the Scheme emitter,
 * `generate-unit` in `emit.scm`.
 *
 * @param {*} schemeIr - The procedure's IR, as `lower-lambda` returned it.
 * @param {Array<string>} globals - The globals it references, in order.
 * @param {string} name - Its display name.
 * @param {Array<string>} guarded - The globals with an inline expansion that
 *   are bound to their primitive in the environment being compiled for.
 * @returns {{source: string, constants: Array<*>}} The generated code.
 */
export function emitUnit(schemeIr, globals, name, guarded) {
  const scheme = lowering();
  if (scheme === null) throw new Error(`the Scheme compiler could not start: ${bootstrapFailure}`);
  const result = toArray(call(scheme.generateUnit, [
    schemeIr, toList(globals.map((g) => intern(g))), name, toList(guarded.map((g) => intern(g)))
  ]));
  return { source: result[0], constants: toArray(result[1]) };
}

/**
 * The interpreter the compiler's Scheme runs in, and its library's own
 * environment, for tests that exercise its internal procedures directly.
 * @returns {{interpreter: Object, env: Object}} The pair.
 * @throws {Error} If the compiler could not start.
 */
export function compilerEnvironment() {
  const scheme = lowering();
  if (scheme === null) throw new Error(`the Scheme compiler could not start: ${bootstrapFailure}`);
  return { interpreter: scheme.interpreter, env: scheme.env };
}

/**
 * The JavaScript identifier generated code uses for a renamed Scheme local,
 * from `emit.scm`.
 * @param {string} name - A renamed Scheme identifier.
 * @returns {string} The identifier.
 */
export function jsNameOf(name) {
  const scheme = lowering();
  if (scheme === null) throw new Error(`the Scheme compiler could not start: ${bootstrapFailure}`);
  return call(scheme.jsName, [intern(name)]);
}

/**
 * The globals that have an inline expansion, from `inline.scm`.
 * @returns {Array<string>} Their names.
 */
export function inlineExpansionNames() {
  const scheme = lowering();
  if (scheme === null) return [];
  if (scheme.inlineNames === undefined) {
    scheme.inlineNames = toArray(call(scheme.inlineExpansionNames, []))
      .map((s) => s.name);
  }
  return scheme.inlineNames;
}

/**
 * Returns the first control-transferring global in a set, or null.
 *
 * This is the base case of the safety analysis: a procedure that names
 * `call/cc` or `dynamic-wind` itself is obviously unsafe to compile. It is only
 * the base case: a procedure can sit in the dynamic extent of a capture made by
 * something it calls, without naming anything itself. `safety.js` closes this
 * over the call graph for that reason.
 *
 * Which names count is `control-globals` in `ir.scm`, beside the lowering that
 * has to agree with it.
 *
 * @param {Set<string>} globals - Globals a form references.
 * @returns {string|null} The offending name, or null.
 */
export function controlGlobalIn(globals) {
  const scheme = lowering();
  // Nothing can be compiled without the pass, so nothing reaches this and the
  // answer does not matter. Reporting no control global adds no second reason
  // for a decline that has already been made for the first.
  if (scheme === null) return null;

  for (const name of globals) {
    if (scheme.controlGlobals.has(name)) return name;
  }
  return null;
}
