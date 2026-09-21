/**
 * @fileoverview Running the compiler's lowering pass, which is Scheme.
 *
 * The lowering itself lives in `ir.scm`. This module is the door into it: it
 * keeps the Scheme interpreter the pass runs in, marshals an analyzed AST
 * across, and hands the IR back to code generation, which is still JavaScript.
 * Callers see the same two functions they always did.
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
 * ## The bootstrap, and why it terminates
 *
 * Lowering is what the compiler uses to produce code, so a compiler written in
 * the language it compiles has to start somewhere. It starts in the
 * interpreter, which can run `ir.scm` from source with no compiler at all. That
 * is slow -- around 300x the JavaScript figure -- so it is not where it stays:
 *
 *   1. The interpreter runs `ir.scm`. The compiler now works, slowly.
 *   2. With it, the build compiles the standard library into
 *      `src/packaging/compiled_stdlib.js`.
 *   3. With that, the build compiles `ir.scm` into
 *      `src/packaging/compiled_compiler.js`. The compiler has compiled itself.
 *
 * Only step 3's output is loaded here, and only after `ir.scm`'s source has
 * been interpreted, so a stale or missing build costs speed and never
 * correctness: `installPrebuilt` checks a fingerprint of the source it was
 * generated from and installs nothing if it has moved on.
 *
 * Step 2 is not an optimization of step 3. Lowering calls `memq` and `assq` on
 * every scope lookup and every global it records, and those are themselves
 * Scheme; with the library interpreted, compiling `ir.scm` is worth 1.5x, and
 * with it compiled, 20x. The order matters more than either step.
 *
 * ## A separate interpreter
 *
 * The pass runs in its own interpreter, built once and shared. It cannot run in
 * the environment being compiled: that environment belongs to the user's
 * program, which has its own bindings for names like `lower-node` and need not
 * have a standard library at all.
 */

import { createInterpreter } from '../core/interpreter/index.js';
import { parse } from '../core/interpreter/reader.js';
import { analyze } from '../core/interpreter/analyzer.js';
import { BUNDLED_SOURCES, COMPILER_SOURCES } from '../packaging/bundled_libraries.js';
import prebuiltStdlib, { LIBRARY_FILES } from '../packaging/compiled_stdlib.js';
import prebuiltCompiler, { COMPILER_FILES } from '../packaging/compiled_compiler.js';
import { installPrebuilt, fingerprintSources } from './prebuilt.js';
import { invoke, settle } from './runtime.js';
import { astToScheme, irToJs, toArray } from './marshal.js';

/**
 * The Scheme lowering, bootstrapped on first use, or null if it could not be.
 * @type {{lowerLambda: Function, controlGlobalIn: Function}|null}
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
 * Evaluates Scheme source in an environment.
 * @param {Object} interpreter - The interpreter.
 * @param {Object} env - The environment to evaluate in.
 * @param {string} source - Scheme source text.
 * @returns {void}
 */
function runSource(interpreter, env, source) {
  for (const form of parse(source)) {
    interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
  }
}

/**
 * Builds the interpreter the lowering runs in, and finds its entry points.
 *
 * @returns {{lowerLambda: Function, controlGlobalIn: Function}} The two Scheme
 *   procedures the compiler calls.
 */
function bootstrap() {
  const { interpreter, env } = createInterpreter();

  // The library's source runs first in both cases. It is what defines the
  // macros the analyzer needs and the closures the prebuilt code replaces, so
  // installing over it is a substitution rather than a definition -- which is
  // what lets the fingerprint check fail towards leaving a procedure alone.
  const librarySources = LIBRARY_FILES.map((file) => BUNDLED_SOURCES[file]);
  for (const source of librarySources) runSource(interpreter, env, source);
  installPrebuilt(env, prebuiltStdlib, fingerprintSources(librarySources));

  const compilerSources = COMPILER_FILES.map((file) => COMPILER_SOURCES[file]);
  for (const source of compilerSources) runSource(interpreter, env, source);
  installPrebuilt(env, prebuiltCompiler, fingerprintSources(compilerSources));

  // The control-global list is read across once, here, rather than asked for
  // per procedure. It is a membership test on a fixed list of fourteen names,
  // and marshalling a set of globals into Scheme to run it there would cost
  // more than the test. Reading it from `ir.scm` is what keeps it one list:
  // the lowering and the caller that declines on it cannot drift apart if
  // neither owns a second copy.
  return {
    lowerLambda: env.lookup('lower-lambda'),
    controlGlobals: new Set(toArray(env.lookup('control-globals')).map((s) => s.name))
  };
}

/**
 * The lowering pass, bootstrapping it if this is the first call.
 * @returns {{lowerLambda: Function, controlGlobalIn: Function}|null} The pass,
 *   or null if it could not be built.
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
 * @returns {{ir: Object, globals: Set<string>, callsUnknown: boolean,
 *   captures: boolean}|{reason: string}} The IR with what it references, or why
 *   it could not be lowered.
 */
export function lowerLambda(lambdaNode) {
  const scheme = lowering();
  if (scheme === null) {
    return { reason: `the Scheme lowering could not start: ${bootstrapFailure}` };
  }

  const result = toArray(call(scheme.lowerLambda, [astToScheme(lambdaNode)]));
  if (result[0].name === 'fail') return { reason: result[1] };

  return {
    ir: irToJs(result[1]),
    globals: new Set(toArray(result[2]).map((symbol) => symbol.name)),
    callsUnknown: result[3],
    captures: result[4]
  };
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
