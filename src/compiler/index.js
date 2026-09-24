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
 * A compiled procedure keeps the interpreter's value representation and calls
 * the interpreter's own primitives, so the two tiers interoperate without any
 * conversion at the call boundary.
 */

import { LambdaNode, DefineNode } from '../core/interpreter/ast_nodes.js';
import { lowerLambda, controlGlobalIn } from './lowering.js';
import { unsafeDefinitions, unsafeClosures } from './safety.js';
import { generate } from './codegen.js';
import * as R from './runtime.js';
import { substituteLibraryValues } from '../core/interpreter/library_registry.js';

/**
 * Largest generated source, in characters, that a procedure may produce.
 *
 * A procedure is emitted twice, and so is every procedure nested inside it --
 * once within each form of its parent -- so a genuinely nested closure costs a
 * multiple per level of nesting, measured at about 4.2x. Deep enough, that
 * exceeds JavaScript's own maximum string length and fails while the source is
 * still being assembled.
 *
 * What used to make this acute was that a `let` is an immediately-applied
 * lambda by the time the compiler sees it, so a chain of bindings was a chain
 * of nested procedures -- twenty-nine deep at the worst point in the benchmark
 * corpus. Those are now reduced to bindings during lowering and nest nothing,
 * which took the deepest procedure in the corpus from twenty-nine levels to
 * seven and removed this decline entirely.
 *
 * The bound stays for closures that really are nested. Declining is an ordinary
 * decline rather than a failure, and costs nothing real: a procedure whose body
 * is megabytes of JavaScript would not have been fast.
 */
const MAX_SOURCE = 4 * 1024 * 1024;


/**
 * Generates a procedure's source, declining rather than throwing if it is
 * unreasonably large or code generation fails.
 *
 * @param {Object} lowered - The result of `lowerLambda`.
 * @param {string} name - The procedure's name.
 * @param {Object} env - The environment to resolve globals in.
 * @returns {{source: string, constants: Array<*>}|{reason: string}} The
 *   generated code, or why it was declined.
 */
function generateBounded(lowered, name, env) {
  let result;
  try {
    result = generate(lowered.schemeIr, lowered.globals, name, env);
  } catch (e) {
    return { reason: `code generation failed: ${e.message}` };
  }
  if (result.source.length > MAX_SOURCE) {
    return {
      reason: `generated source is ${result.source.length} characters, over the `
        + `${MAX_SOURCE} limit; the procedure nests too deeply to emit twice`
    };
  }
  return result;
}

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
 * @param {Object} [options] - Options.
 * @param {boolean} [options.allowCaptures=false] - Compile a procedure that
 *   captures a continuation. It works, and it is slower than interpreting it,
 *   so it is off by default; see `safety.js` for the measurements.
 * @returns {CompileResult} The outcome.
 */
export function tryCompileDefinition(ast, env, options = {}) {
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

  // A procedure that names `call/cc`, `dynamic-wind` or the like is declined
  // because there is no IR for those forms, not because compiling it would be
  // wrong. A caller that can see the whole unit should use `compileProgram`,
  // which additionally holds back the procedures a capture would unwind
  // through -- a speed judgement rather than a correctness one.
  const control = controlGlobalIn(lowered.globals);
  if (control !== null) {
    return { compiled: false, reason: `references control global '${control}'` };
  }
  if (lowered.captures && options.allowCaptures !== true) {
    return {
      compiled: false,
      reason: 'captures a continuation, which costs more compiled than interpreted'
    };
  }

  const generated = generateBounded(lowered, ast.name, env);
  if (generated.reason !== undefined) return { compiled: false, reason: generated.reason };
  const { source, constants } = generated;

  let procedure;
  try {
    // `new Function` rather than `eval` so the generated code gets its own
    // scope and cannot see, or be confused with, the compiler's own bindings.
    procedure = new Function('R', 'E', 'K', source)(R, env, constants);
  } catch (e) {
    return { compiled: false, reason: `code generation failed: ${e.message}`, source };
  }
  R.recordSource(procedure, value.source ?? ast.source);

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
  const control = controlGlobalIn(lowered.globals);
  if (control !== null) {
    return { compiled: false, reason: `references control global '${control}'` };
  }

  // Compiled in the closure's *own* environment, so its free variables resolve
  // the way they did when it was interpreted -- a library procedure's globals
  // live in that library's environment, not in the interaction environment.
  const env = closure.env;
  const generated = generateBounded(lowered, name, env);
  if (generated.reason !== undefined) return { compiled: false, reason: generated.reason };
  const { source, constants } = generated;

  let procedure;
  try {
    procedure = new Function('R', 'E', 'K', source)(R, env, constants);
  } catch (e) {
    return { compiled: false, reason: `code generation failed: ${e.message}`, source };
  }
  R.recordSource(procedure, closure.source);
  return { compiled: true, name, procedure, source };
}

/**
 * Rebuilds the lambda an interpreted closure came from.
 * @param {Function} closure - An interpreted Scheme closure.
 * @param {string} name - Its name.
 * @returns {Object} A `LambdaNode`.
 */
function lambdaOf(closure, name) {
  return new LambdaNode(
    closure.params, closure.body, closure.restParam, name,
    closure.originalParams, closure.originalRestParam);
}

/**
 * Generates code for every procedure in an environment worth compiling,
 * without installing anything.
 *
 * Separated from installing so that one policy serves both callers: the build
 * step that writes the generated source into the bundle, and
 * `compileEnvironment`, which turns it into procedures immediately. Two
 * implementations of "which procedures do we compile" would drift, and the
 * build's answer has to match the runtime's or the bundle would contain code
 * for procedures the runtime does not expect.
 *
 * @param {Object} env - The environment to read.
 * @param {Object} [options] - As for `compileEnvironment`.
 * @returns {{generated: Array<Object>, declined: Array<{name: string, reason: string}>}}
 *   One entry per procedure, with its source, constants, parameter names and
 *   the globals it references.
 */
export function generateEnvironment(env, options = {}) {
  const entries = [];
  for (const [name, value] of env.bindings) {
    // An interpreted closure, as opposed to a primitive or an already
    // compiled procedure: only these carry a body to compile.
    if (typeof value === 'function' && value.body !== undefined) {
      entries.push({ name, closure: value });
    }
  }

  // Decided over the whole set before anything is generated, because whether
  // one procedure is worth compiling depends on what its callees do.
  const unsafe = options.allowContinuationUnsafe
    ? new Map()
    : unsafeClosures(entries, env, { strict: options.strict === true });

  const generated = [];
  const declined = [];
  for (const { name, closure } of entries) {
    const unsafeReason = unsafe.get(name);
    if (unsafeReason !== undefined) {
      declined.push({ name, reason: unsafeReason });
      continue;
    }

    const lambda = lambdaOf(closure, name);
    const lowered = lowerLambda(lambda);
    if (lowered.reason) {
      declined.push({ name, reason: lowered.reason });
      continue;
    }
    const control = controlGlobalIn(lowered.globals);
    if (control !== null) {
      declined.push({ name, reason: `references control global '${control}'` });
      continue;
    }
    if (lowered.captures && options.allowCaptures !== true) {
      declined.push({
        name,
        reason: 'captures a continuation, which costs more compiled than interpreted'
      });
      continue;
    }

    // Generated against the closure's *own* environment, so its free variables
    // resolve the way they did when it was interpreted -- a library
    // procedure's globals live in that library's environment, not in the
    // interaction environment.
    const result = generateBounded(lowered, name, closure.env);
    if (result.reason !== undefined) {
      declined.push({ name, reason: result.reason });
      continue;
    }

    generated.push({
      name, closure, source: result.source, constants: result.constants,
      params: lambda.params, rest: lambda.restParam, globals: [...lowered.globals]
    });
  }

  return { generated, declined };
}

/**
 * Compiles every compilable top-level definition in a program, installing each
 * generated procedure into the environment in place of the interpreted one.
 *
 * ## Procedures a capture would unwind through
 *
 * A compiled procedure can be part of a captured continuation: on learning that
 * a callee is capturing, it saves its locals and where it had got to, and is
 * resumed from there when the continuation is invoked. So compiling such a
 * procedure is correct.
 *
 * It is often not *fast*, which is why they are still declined by default. A
 * procedure that a capture unwinds through pays to suspend and resume on every
 * capture, and on a capture-heavy program that costs more than interpreting it.
 * The analysis that finds them is in `safety.js`, which explains the measured
 * trade-off.
 *
 * One shape is genuinely not supported and is refused rather than answered:
 * a capture crossing more than one boundary between compiled and interpreted
 * code. See `CallCCNode` in `src/core/interpreter/ast_nodes.js`.
 *
 * @param {Array<Object>} asts - Analyzed top-level nodes, in order.
 * @param {Object} env - The environment to define into.
 * @param {Object} interpreter - The interpreter, for the remaining forms.
 * @param {Object} [options] - Options.
 * @param {boolean} [options.allowContinuationUnsafe=false] - Compile every
 *   definition, including those a capture unwinds through.
 * @param {boolean} [options.strict=false] - Also decline a procedure that calls
 *   a callee it cannot name.
 * @returns {{compiled: Array<string>, declined: Array<{name: string, reason: string}>,
 *   unitDeclined: (string|null)}} What happened, and why if the unit was refused.
 */
export function compileProgram(asts, env, interpreter, options = {}) {
  const compiled = [];
  const declined = [];

  // Which definitions a capture could unwind through. Computed over the whole
  // unit before anything is compiled, because the answer for one procedure
  // depends on what its callees do -- `make-maze` names no control global and
  // is still reached, because `dig-maze` escapes through it.
  const unsafe = options.allowContinuationUnsafe
    ? new Map()
    : unsafeDefinitions(asts, env, { strict: options.strict === true });

  // Kept for callers that ask "was the unit refused wholesale?". It is no
  // longer how the decision is made; a unit that uses continuations in one
  // corner can now have the rest of it compiled.
  const firstUnsafe = unsafe.size > 0 ? [...unsafe.entries()][0] : null;
  const unitDeclined = firstUnsafe === null ? null : `${firstUnsafe[0]}: ${firstUnsafe[1]}`;

  for (const ast of asts) {
    const unsafeReason = ast instanceof DefineNode ? unsafe.get(ast.name) : undefined;
    const result = unsafeReason !== undefined
      ? { compiled: false, reason: unsafeReason }
      : tryCompileDefinition(ast, env,
        { allowCaptures: options.allowContinuationUnsafe === true });

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

  return { compiled, declined, unitDeclined, unsafe };
}

/**
 * Compiles the interpreted procedures already living in an environment.
 *
 * The standard library is loaded and interpreted before anything considers
 * compiling it, so it cannot be reached through `compileProgram`, which works
 * from source. A Scheme closure keeps its parameters, body and defining
 * environment, so it can be compiled after the fact instead.
 *
 * ## Why this matters more than it looks
 *
 * `memq`, `assq`, `map` and `assoc` are themselves Scheme. A compiled
 * procedure that calls one crosses into the interpreter on what is very often
 * its hottest path, and the cost is not small: on the `ir.js` lowering ported
 * to Scheme, compiling the library alongside it was worth **10.8x**, and moved
 * what the compiler tier was worth on that code from 1.44x to 15.5x. Figures
 * taken with the library interpreted measure that boundary rather than the
 * quality of the generated code.
 *
 * Each procedure is replaced in place, so callers pick up the compiled version
 * without being recompiled themselves: compiled code resolves a global through
 * an accessor that remembers the frame rather than the value.
 *
 * @param {Object} env - The environment to compile in place.
 * @param {Object} [options] - Options.
 * @param {boolean} [options.strict=false] - Also decline a procedure that calls
 *   a callee it cannot name.
 * @returns {{compiled: Array<string>, declined: Array<{name: string, reason: string},
 *   unavailable: (string|undefined)}} What was compiled, why anything else was
 *   not, and -- if code generation is forbidden here at all -- why nothing was.
 */
export function compileEnvironment(env, options = {}) {
  // Generating code needs `new Function`, which a strict Content-Security-Policy
  // forbids. Probing once rather than discovering it per procedure keeps the
  // cost of an unsupported environment to a single thrown error, and returning
  // a reason rather than throwing lets the caller carry on interpreted -- which
  // is the whole point of keeping the interpreter as a permanent tier.
  try {
    new Function('return 1');
  } catch (e) {
    return {
      compiled: [],
      declined: [],
      unavailable: 'generating code is not permitted here, so the library stays interpreted'
    };
  }

  const { generated, declined } = generateEnvironment(env, options);

  const compiled = [];
  const replaced = new Map();
  for (const entry of generated) {
    try {
      const procedure = R.recordSource(
        new Function('R', 'E', 'K', entry.source)(R, entry.closure.env, entry.constants),
        entry.closure.source);
      env.define(entry.name, procedure);
      replaced.set(entry.closure, procedure);
      compiled.push(entry.name);
    } catch (e) {
      declined.push({ name: entry.name, reason: `code generation failed: ${e.message}` });
    }
  }

  // Libraries imported the interpreted closures by value; see
  // `substituteLibraryValues`.
  substituteLibraryValues(replaced);
  return { compiled, declined };
}

export { R as compilerRuntime };
