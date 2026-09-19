/**
 * @fileoverview Scheme-to-JavaScript compiler tier.
 *
 * A second execution tier beside the interpreter, not a replacement for it. A
 * top-level procedure definition is compiled when the compiler can handle every
 * form in it, and left interpreted otherwise, so the tier can be widened
 * incrementally without any point at which the system is half-correct. The
 * interpreter also remains the reference semantics for differential testing and
 * the execution mode for contexts where generating code is not allowed.
 *
 * Design decisions and their measurements are recorded in
 * `docs/compiler_strategy.md`; the calling convention was chosen in Stage 2a.
 */

import { LambdaNode, DefineNode } from '../core/interpreter/ast_nodes.js';
import { lowerLambda } from './ir.js';
import { generate } from './codegen.js';
import * as R from './runtime.js';

/**
 * @typedef {Object} CompileResult
 * @property {boolean} compiled - Whether compilation succeeded.
 * @property {string} [name] - The renamed procedure name.
 * @property {Function} [procedure] - The generated procedure.
 * @property {string} [source] - Generated JavaScript, for inspection.
 * @property {string} [reason] - Why compilation was declined.
 */

/**
 * Attempts to compile a top-level procedure definition.
 *
 * @param {Object} ast - An analyzed top-level node.
 * @param {Object} env - The environment the definition belongs to.
 * @returns {CompileResult} The outcome.
 */
export function tryCompileDefinition(ast, env) {
  if (!(ast instanceof DefineNode)) {
    return { compiled: false, reason: 'not a top-level definition' };
  }
  const value = ast.valueExpr;
  if (!(value instanceof LambdaNode)) {
    return { compiled: false, reason: 'definition is not a procedure' };
  }

  const lowered = lowerLambda(value);
  if (lowered.reason) {
    return { compiled: false, reason: lowered.reason };
  }

  const { source, constants } = generate(lowered.ir, lowered.globals, ast.name, env);

  let procedure;
  try {
    // `new Function` rather than `eval` so the generated code gets its own
    // scope and cannot see, or be confused with, the compiler's own bindings.
    procedure = new Function('R', 'E', 'K', source)(R, env, constants);
  } catch (e) {
    return { compiled: false, reason: `code generation failed: ${e.message}`, source };
  }

  return { compiled: true, name: ast.name, procedure, source };
}

/**
 * Attempts to compile an already-created interpreted closure.
 *
 * A Scheme closure retains everything needed to rebuild the lambda it came
 * from -- its parameters, body and defining environment -- so a procedure that
 * was loaded and interpreted can be recompiled afterwards without going back
 * to source. That is what makes it possible to compile the standard library
 * after it has been bootstrapped, rather than having to thread the compiler
 * through the library loader.
 *
 * @param {Function} closure - An interpreted Scheme closure.
 * @param {string} name - The name to compile it under.
 * @returns {CompileResult} The outcome.
 */
export function tryCompileClosure(closure, name) {
  if (typeof closure !== 'function' || closure.body === undefined) {
    return { compiled: false, reason: 'not an interpreted closure' };
  }
  const lambda = new LambdaNode(
    closure.params, closure.body, closure.restParam, name,
    closure.originalParams, closure.originalRestParam);

  const lowered = lowerLambda(lambda);
  if (lowered.reason) return { compiled: false, reason: lowered.reason };

  // Compiled in the closure's *own* environment, so its free variables resolve
  // the way they did when it was interpreted -- a library procedure's globals
  // live in that library's environment, not in the interaction environment.
  const env = closure.env;
  const { source, constants } = generate(lowered.ir, lowered.globals, name, env);

  let procedure;
  try {
    procedure = new Function('R', 'E', 'K', source)(R, env, constants);
  } catch (e) {
    return { compiled: false, reason: `code generation failed: ${e.message}`, source };
  }
  return { compiled: true, name, procedure, source };
}

/**
 * Compiles every compilable top-level definition in a program, installing each
 * generated procedure into the environment in place of the interpreted one.
 *
 * ## Continuation safety
 *
 * Declining *individual* procedures that mention `call/cc` is **not** sound, and
 * the `btsearch` benchmark demonstrated why: `in-range` was correctly declined,
 * but `btsearch` and `enumerate` -- which sit in the dynamic extent of the
 * capture and have to be re-entered when the search backtracks -- were
 * compiled, and a compiled frame cannot be re-entered. The program returned a
 * wrong answer rather than failing.
 *
 * So the rule is coarser: if *any* definition in the unit references a
 * control-transferring global, the whole unit is left to the interpreter. That
 * is sound for a self-contained unit, and it is what makes the tier usable now.
 *
 * It is still not sound in general: a compiled procedure can call into another
 * unit that captures a continuation within its extent. Until compiled frames
 * are re-enterable -- the unwind protocol chosen in Stage 2a, which is the next
 * increment -- **this tier is opt-in and must not be enabled by default.**
 *
 * @param {Array<Object>} asts - Analyzed top-level nodes, in order.
 * @param {Object} env - The environment to define into.
 * @param {Object} interpreter - The interpreter, for the remaining forms.
 * @param {Object} [options] - Options.
 * @param {boolean} [options.allowContinuationUnsafe=false] - Compile
 *   per-procedure even when the unit uses continuations. For investigating the
 *   unsoundness, not for running programs.
 * @returns {{compiled: Array<string>, declined: Array<{name: string, reason: string}>,
 *   unitDeclined: (string|null)}} What happened, and why if the unit was refused.
 */
export function compileProgram(asts, env, interpreter, options = {}) {
  const compiled = [];
  const declined = [];

  // A dry run over the whole unit first, so one procedure's use of a
  // continuation can veto compiling its neighbours.
  let unitDeclined = null;
  if (!options.allowContinuationUnsafe) {
    for (const ast of asts) {
      if (!(ast instanceof DefineNode)) continue;
      const probe = tryCompileDefinition(ast, env);
      if (!probe.compiled && /control global/.test(probe.reason ?? '')) {
        unitDeclined = `${ast.name}: ${probe.reason}`;
        break;
      }
    }
  }

  for (const ast of asts) {
    const result = unitDeclined
      ? { compiled: false, reason: `unit uses continuations (${unitDeclined})` }
      : tryCompileDefinition(ast, env);

    if (result.compiled) {
      env.define(result.name, result.procedure);
      compiled.push(result.name);
    } else {
      // Run it the ordinary way. Order is preserved because this happens in
      // the same pass rather than in a second one.
      interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
      if (ast instanceof DefineNode) declined.push({ name: ast.name, reason: result.reason });
    }
  }

  return { compiled, declined, unitDeclined };
}

export { R as compilerRuntime };
