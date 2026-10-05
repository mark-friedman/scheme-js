/**
 * @fileoverview Scheme-to-JavaScript compiler tier: the entry points
 * JavaScript calls.
 *
 * A second execution tier beside the interpreter, not a replacement for it. A
 * procedure is compiled when the compiler can handle every form in it, and
 * left interpreted otherwise, with a reason, so the tier can be widened
 * incrementally without any point at which the system is half-correct. The
 * interpreter also remains the reference semantics for differential testing and
 * the execution mode for contexts where generating code is not allowed.
 *
 * What to compile, and why not, is decided in the compiler's Scheme
 * (`driver.scm` and `safety.scm`). Each entry point here calls one of the
 * compiler's exports, as any JavaScript holding Scheme values calls Scheme,
 * with `callSchemeProcedure`, and reads the records that come back -- a record
 * is an object with a property per field -- into the plain objects JavaScript
 * callers use.
 *
 * A compiled procedure keeps the interpreter's value representation and calls
 * the interpreter's own primitives, so the two tiers interoperate without any
 * conversion at the call boundary.
 */

import * as R from './runtime.js';
import { compilerExports, compilerStartFailure } from './lowering.js';
import { toArray } from '../core/interpreter/cons.js';
import { callSchemeProcedure } from '../core/interpreter/values.js';

export { runCompiledThunk } from './host.js';

/**
 * @typedef {Object} CompileResult
 * @property {boolean} compiled - Whether compilation succeeded.
 * @property {string} [name] - The procedure's name.
 * @property {Function} [procedure] - The generated procedure.
 * @property {string} [source] - Generated JavaScript, for inspection.
 * @property {string} [reason] - Why compilation was declined.
 */

/**
 * @typedef {Object} CompileOptions
 * @property {boolean} [declineCaptures=false] - Decline a procedure that
 *   captures a continuation, or that a capture could unwind through, as the
 *   tier once did by default. It is compiled otherwise: nearly every capture is
 *   an escape, which compiled code pays for easily, and one whose
 *   continuations are re-entered over and over is switched back to its closure
 *   as the program runs, where it has one. `safety.scm` has the measurements.
 * @property {boolean} [strict=false] - With `declineCaptures`, also decline a
 *   procedure that calls a callee it cannot name.
 */

/**
 * A Scheme string, or #f, as JavaScript reads it.
 * @param {*} s - A Scheme string, or false.
 * @returns {string|undefined}
 */
const text = (s) => (s === false || s === undefined ? undefined : String(s));

/**
 * Why nothing could be compiled, when the compiler could not start.
 * @returns {string}
 */
const notStarted = () => `the Scheme compiler could not start: ${compilerStartFailure()}`;

/**
 * A `compiled` or `declined` record as a `CompileResult`.
 * @param {Object} outcome - The record.
 * @returns {CompileResult}
 */
function resultOf(outcome) {
  if (outcome.procedure !== undefined) {
    return { compiled: true, name: text(outcome.name), procedure: outcome.procedure, source: text(outcome.source) };
  }
  const result = { compiled: false, reason: text(outcome.reason) };
  if (outcome.source !== false) result.source = text(outcome.source);
  return result;
}

/**
 * A list of `declined` records as `{name, reason}` objects.
 * @param {*} declines - The list.
 * @returns {Array<{name: string, reason: string}>}
 */
const declinesOf = (declines) => toArray(declines).map((d) => ({ name: text(d.name), reason: text(d.reason) }));

/**
 * Attempts to compile a top-level procedure definition.
 * @param {Object} ast - An analyzed top-level node.
 * @param {Object} env - The environment the definition belongs to.
 * @param {CompileOptions} [options] - Options.
 * @returns {CompileResult} The outcome.
 */
export function tryCompileDefinition(ast, env, options = {}) {
  const compiler = compilerExports();
  if (compiler === null) return { compiled: false, reason: notStarted() };
  return resultOf(callSchemeProcedure(compiler.get('compile-definition'),
    [ast, env, options.declineCaptures === true]));
}

/**
 * Attempts to compile a top-level expression, or the value of a top-level
 * definition that is not a procedure, as a procedure of no arguments to call
 * once (`runCompiledThunk`).
 * @param {Object} ast - The analyzed expression.
 * @param {Object} env - The environment its globals resolve in.
 * @param {CompileOptions} [options] - Options.
 * @returns {CompileResult} The outcome; `procedure` is the thunk.
 */
export function tryCompileExpression(ast, env, options = {}) {
  const compiler = compilerExports();
  if (compiler === null) return { compiled: false, reason: notStarted() };
  return resultOf(callSchemeProcedure(compiler.get('compile-expression'),
    [ast, env, false, options.declineCaptures === true]));
}

/**
 * Attempts to compile an interpreted closure, which keeps everything needed to
 * rebuild the lambda it came from, so a procedure that was loaded and
 * interpreted can be compiled afterwards without its source.
 * @param {Function} closure - An interpreted Scheme closure.
 * @param {string} name - The name to compile it under.
 * @param {CompileOptions} [options] - Options.
 * @returns {CompileResult} The outcome.
 */
export function tryCompileClosure(closure, name, options = {}) {
  const compiler = compilerExports();
  if (compiler === null) return { compiled: false, reason: notStarted() };
  return resultOf(callSchemeProcedure(compiler.get('compile-closure'),
    [closure, name, options.declineCaptures === true]));
}

/**
 * Generates code for every procedure in an environment worth compiling,
 * without installing anything: what the build writes into the bundle.
 * @param {Object} env - The environment to read.
 * @param {CompileOptions & {ownOnly?: boolean}} [options] - Options, and
 *   `ownOnly`: only the procedures made in `env` itself, not those it imported.
 * @returns {{generated: Array<Object>, declined: Array<{name: string, reason: string}>,
 *   unavailable?: string}} One entry per procedure, with its source, constants,
 *   parameter names and the globals it references.
 */
export function generateEnvironment(env, options = {}) {
  const compiler = compilerExports();
  if (compiler === null) return { generated: [], declined: [], unavailable: notStarted() };
  const result = callSchemeProcedure(compiler.get('generate-environment'),
    [env, options.ownOnly === true, options.declineCaptures === true, options.strict === true]);
  const generated = toArray(result.car).map((code) => ({
    name: text(code.name),
    closure: code.closure,
    source: text(code.source),
    constants: toArray(code.constants),
    params: code.closure.params,
    rest: code.closure.restParam,
    globals: toArray(code.globals).map((symbol) => symbol.name)
  }));
  return { generated, declined: declinesOf(result.cdr) };
}

/**
 * Compiles the interpreted procedures already living in an environment,
 * replacing each in place.
 * @param {Object} env - The environment to compile in place.
 * @param {CompileOptions} [options] - Options.
 * @returns {{compiled: Array<string>, declined: Array<{name: string, reason: string}>,
 *   unavailable?: string}} What was compiled, why anything else was not, and --
 *   if code generation is forbidden here at all -- why nothing was.
 */
export function compileEnvironment(env, options = {}) {
  const compiler = compilerExports();
  if (compiler === null) return { compiled: [], declined: [], unavailable: notStarted() };
  const result = callSchemeProcedure(compiler.get('compile-environment'), [env, options.strict === true]);
  if (result === false) {
    return {
      compiled: [],
      declined: [],
      unavailable: 'generating code is not permitted here, so the library stays interpreted'
    };
  }
  return { compiled: toArray(result.car).map((c) => text(c.name)), declined: declinesOf(result.cdr) };
}

/**
 * Runs a program's top-level forms in order, compiling each procedure
 * definition over the closure it makes, and each expression worth it as a
 * thunk called once.
 * @param {Array<Object>} asts - Analyzed top-level nodes, in order.
 * @param {Object} env - The environment to define into.
 * @param {Object} interpreter - The interpreter.
 * @param {CompileOptions} [options] - Options.
 * @returns {{compiled: Array<string>, declined: Array<{name: string, reason: string}>,
 *   unitDeclined: (string|null), unsafe: Map<string, string>, expressions: number,
 *   value: *}} What happened; the first procedure the capture rule declined, if
 *   it was asked for; how many expressions or definitions' values were
 *   compiled; and the last form's value.
 */
export function compileProgram(asts, env, interpreter, options = {}) {
  const compiler = compilerExports();
  if (compiler === null) throw new Error(notStarted());
  const run = callSchemeProcedure(compiler.get('compile-program'),
    [asts, env, interpreter, options.declineCaptures === true, options.strict === true]);
  const unsafe = new Map(toArray(run.unsafe).map((pair) => [pair.car.name, text(pair.cdr)]));
  const first = [...unsafe][0];
  return {
    compiled: toArray(run.compiled).map(text),
    declined: declinesOf(run.declined),
    unitDeclined: first === undefined ? null : `${first[0]}: ${first[1]}`,
    unsafe,
    expressions: Number(run.expressions),
    value: run.value
  };
}

/**
 * Decides which of a unit's procedure definitions a capture could unwind
 * through, as `declineCaptures` does (`safety.scm`).
 * @param {Array<Object>} asts - The unit's analyzed top-level nodes, in order.
 * @param {Object} env - The environment the unit is being defined into.
 * @param {{strict?: boolean}} [options] - Options.
 * @returns {Map<string, string>} Declined names, each mapped to the reason.
 */
export function unsafeDefinitions(asts, env, options = {}) {
  const compiler = compilerExports();
  if (compiler === null) return new Map();
  const unsafe = callSchemeProcedure(compiler.get('program-unsafe-definitions'), [asts, env, options.strict === true]);
  return new Map(toArray(unsafe).map((pair) => [pair.car.name, text(pair.cdr)]));
}

export { R as compilerRuntime };
