/**
 * @fileoverview Running the compiler's Scheme.
 *
 * The compiler is Scheme: lowering in `ir.scm`, code generation in `emit.scm`
 * and the files beside it, what to compile and why not in `driver.scm` and
 * `safety.scm`, and when, for a program's own code, in `tier.scm`. This module
 * is the door into all of it: it starts the Scheme interpreter the compiler
 * runs in, and calls its entry points (`callCompiler`), for `index.js` and
 * `tiering.js`, and for tests that lower a lambda and inspect the IR.
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
 * lambdas from the canonical benchmarks and the standard library. For the
 * standard library that cost is off the path a user waits on, since it is
 * lowered at build time. For a program it is not: the tier compiles the
 * program's procedures as it runs, each once, at a few milliseconds each, and a
 * short program can spend most of its run doing it -- the canonical `scheme`
 * benchmark, run with the tier attached, nine tenths of it, compiling 93
 * procedures. Starting the compiler is on that path too: the tier's decisions
 * are Scheme, so a program with the tier attached starts it on attaching,
 * about 130 ms, most of it analyzing and running the source of the compiler
 * and of the libraries it imports, which its prebuilt tables then replace.
 *
 * ## The compiler is a library
 *
 * Its Scheme is the library `(scheme-js compiler)`, in `compiler.sld`, which
 * imports what it is written with -- `(scheme base)`, SRFI 1, SRFI 151,
 * SRFI 152 --
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
import { installLibraryTable, libraryRestorer } from './prebuilt.js';
import { registerCompilerHost } from './host.js';
import { setReentryPolicy } from '../core/interpreter/unwind.js';
import { callSchemeProcedure } from '../core/interpreter/values.js';

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
 * @type {{interpreter: Object, env: Object, exports: Map<string, Function>}|null}
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
 * Each library is restored from its prebuilt table as it loads, without its
 * source running: its procedures bound from their compiled code, and its other
 * forms -- the macros the analyzer needs, record types, values -- run in their
 * places. A table that no longer matches its sources leaves its library to
 * load from source, and then makes the closures the source made run compiled,
 * defining nothing -- which is what lets the fingerprint check fail towards
 * leaving a procedure alone.
 *
 * @returns {Object} The interpreter, the library's environment, and what is
 *   read from its exports up front.
 */
function bootstrap() {
  const tables = { ...prebuiltLibraries, ...prebuiltCompiler };
  return withPrivateLibraries({
    resolver: resolve,
    hook: (name, env) => installLibraryTable(tables, name, env, compilerSourceOf),
    restorer: libraryRestorer(tables)
  }, () => {
    const { interpreter, env } = createInterpreter();
    registerCompilerHost(env);
    const exports = loadLibrarySync(COMPILER_LIBRARY, analyze, interpreter, env);
    return { interpreter, env: getLibraryEnv(COMPILER_LIBRARY), exports };
  });
}

/**
 * Starts the compiler, and hands the runtime what it asks the compiler's
 * Scheme about as a program runs: whether a procedure whose saved frames are
 * being resumed is re-entered often enough to switch back (`note-resume` in
 * `tier.scm`). Until the compiler has started nothing is switched back, which
 * only a program running prebuilt library code with its own code interpreted
 * can see.
 * @returns {Object} What `bootstrap` returns.
 */
function start() {
  const scheme = bootstrap();
  setReentryPolicy(scheme.exports.get('note-resume'),
    Number(callSchemeProcedure(scheme.exports.get('first-resume-to-ask'), [])));
  return scheme;
}

/**
 * The compiler, bootstrapping it if this is the first call.
 * @returns {Object|null} What `bootstrap` returns, or null if it could not be
 *   built.
 */
function lowering() {
  if (pass !== null || bootstrapFailure !== null) return pass;
  try {
    pass = start();
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
 * Calls one of the compiler's entry points, starting the compiler if it has
 * not started.
 * @param {string} name - The entry point, as `compiler.sld` exports it.
 * @param {Array<*>} args - Its arguments, as Scheme values.
 * @returns {*} Its result; `undefined` if the compiler could not start, which
 *   `compilerStartFailure` says why.
 */
export function callCompiler(name, args) {
  const scheme = lowering();
  if (scheme === null) return undefined;
  return callSchemeProcedure(scheme.exports.get(name), args);
}
