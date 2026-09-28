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

import {
  LambdaNode, DefineNode, BeginNode, LetRecNode, TailAppNode, LiteralNode
} from '../core/interpreter/ast_nodes.js';
import { Executable } from '../core/interpreter/stepables_base.js';
import { lowerLambda, controlGlobalIn } from './lowering.js';
import { unsafeDefinitions, unsafeClosures } from './safety.js';
import { generate } from './codegen.js';
import * as R from './runtime.js';
import { substituteLibraryValues, recordCompiledOver } from '../core/interpreter/library_registry.js';

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
 * @param {boolean} [options.declineCaptures=false] - Decline a procedure that
 *   captures a continuation, as the tier once did by default. It is compiled
 *   otherwise: nearly every capture is an escape, which compiled code pays for
 *   easily, and one whose continuations are re-entered over and over is
 *   switched back to its closure as the program runs, where it has one
 *   (`noteResume` in `src/core/interpreter/unwind.js`). See `safety.js` for
 *   the measurements.
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

  return compileLambda(value, ast.name, env, value.source ?? ast.source, options);
}

/**
 * Compiles a lambda node: the work `tryCompileDefinition` and
 * `tryCompileExpression` share once each has its lambda.
 *
 * @param {LambdaNode} value - The lambda.
 * @param {string} name - The name to compile it under.
 * @param {Object} env - The environment its globals resolve in.
 * @param {Object|null} span - Its source span, for the debugger.
 * @param {Object} options - As for `tryCompileDefinition`.
 * @returns {CompileResult} The outcome.
 */
function compileLambda(value, name, env, span, options) {
  const lowered = lowerLambda(value);
  if (lowered.reason) {
    return { compiled: false, reason: lowered.reason };
  }

  // A procedure that names `dynamic-wind`, `guard` or the like is declined
  // because there is no IR for those forms, not because compiling it would be
  // wrong.
  const control = controlGlobalIn(lowered.globals);
  if (control !== null) {
    return { compiled: false, reason: `references control global '${control}'` };
  }
  if (lowered.captures && options.declineCaptures === true) {
    return { compiled: false, reason: 'captures a continuation, and captures are declined' };
  }

  const generated = generateBounded(lowered, name, env);
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
  R.recordSource(procedure, span);

  return { compiled: true, name, procedure, source };
}

// ---------------------------------------------------------------------------
// Top-level expressions
// ---------------------------------------------------------------------------

/**
 * Whether an analyzed form defines at top level: a definition, or a `begin`
 * whose definitions splice into the environment it runs in. Wrapped in a
 * procedure they would become internal definitions instead.
 * @param {Object} ast - An analyzed form.
 * @returns {boolean}
 */
function definesAtTopLevel(ast) {
  return ast instanceof DefineNode
    || (ast instanceof BeginNode && ast.expressions.some(definesAtTopLevel));
}

/**
 * Whether an analyzed form makes a procedure or loops: what makes compiling it
 * worth what compiling costs. Straight-line code runs once, and compiling it
 * costs more than running it.
 * @param {Object} ast - An analyzed form.
 * @returns {boolean}
 */
export function makesProceduresOrLoops(ast) {
  const seen = new Set();
  const visit = (node) => {
    if (node === null || typeof node !== 'object' || seen.has(node)) return false;
    seen.add(node);
    if (Array.isArray(node)) return node.some(visit);
    if (!(node instanceof Executable)) return false;
    if (node instanceof LambdaNode || node instanceof LetRecNode) return true;
    return Object.values(node).some(visit);
  };
  return visit(ast);
}

/**
 * Attempts to compile a top-level expression, or the value of a top-level
 * definition that is not a procedure, as a procedure of no arguments to call
 * once.
 *
 * A program's own procedures are often not top-level definitions at all:
 * `benchmarks/r7rs/src/nboyer.scm` defines stubs, then assigns every real
 * procedure from inside one top-level `(let () ...)`, so compiling definitions
 * alone left the whole program interpreted. A definition whose value is made
 * by an expression -- a closure over a table, say -- is the same case.
 *
 * Declined, and so interpreted: a form that defines at top level, and one that
 * makes no procedure and has no loop.
 *
 * @param {Object} ast - The analyzed expression.
 * @param {Object} env - The environment its globals resolve in.
 * @param {Object} [options] - As for `tryCompileDefinition`.
 * @returns {CompileResult} The outcome; `procedure` is the thunk.
 */
export function tryCompileExpression(ast, env, options = {}) {
  if (definesAtTopLevel(ast)) return { compiled: false, reason: 'defines at top level' };
  if (!makesProceduresOrLoops(ast)) {
    return { compiled: false, reason: 'makes no procedure and has no loop, so runs once' };
  }
  const thunk = new LambdaNode([], ast, null, 'top-level');
  return compileLambda(thunk, 'top-level', env, ast.source ?? null, options);
}

/**
 * Calls a compiled thunk from the interpreter, and returns its value.
 *
 * From the interpreter rather than directly, so that it runs as compiled code
 * the interpreter called: a continuation captured in it, or frames moved to the
 * heap when it recurses deeply, are finished where they should be.
 *
 * @param {Object} interpreter - The interpreter.
 * @param {Object} env - The environment.
 * @param {Function} thunk - The compiled thunk.
 * @returns {*} Its value.
 */
export function runCompiledThunk(interpreter, env, thunk) {
  return R.settle(interpreter.run(new TailAppNode(new LiteralNode(thunk), []), env, [], undefined,
    { jsAutoConvert: 'raw' }));
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
 * @param {Object} [options] - As for `tryCompileDefinition`.
 * @returns {CompileResult} The outcome.
 */
export function tryCompileClosure(closure, name, options = {}) {
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
  if (lowered.captures && options.declineCaptures === true) {
    return { compiled: false, reason: 'captures a continuation, and captures are declined' };
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
 * Whether a closure was created in an environment or somewhere inside it.
 *
 * A procedure a library defines closes over the library's environment, or over
 * a scope nested in it when it was made by a `let` around a `lambda`. One it
 * imported closes over the environment of the library that defined it, which
 * is never inside this one.
 *
 * @param {Function} closure - An interpreted closure.
 * @param {Object} env - The environment.
 * @returns {boolean} True if the closure's environment is `env` or nested in it.
 */
function definedWithin(closure, env) {
  for (let scope = closure.env; scope; scope = scope.parent) {
    if (scope === env) return true;
  }
  return false;
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
 * @param {Object} [options] - As for `compileEnvironment`, and:
 * @param {boolean} [options.ownOnly=false] - Only the procedures defined in
 *   `env` itself, leaving out those it imported. A library's environment holds
 *   a copy of everything it imports, and a table built for that library must
 *   not carry a second compiled copy of another library's procedures.
 * @returns {{generated: Array<Object>, declined: Array<{name: string, reason: string}>}}
 *   One entry per procedure, with its source, constants, parameter names and
 *   the globals it references.
 */
export function generateEnvironment(env, options = {}) {
  const entries = [];
  for (const [name, value] of env.bindings) {
    // An interpreted closure, as opposed to a primitive or an already
    // compiled procedure: only these carry a body to compile.
    if (typeof value === 'function' && value.body !== undefined
      && (options.ownOnly !== true || definedWithin(value, env))) {
      entries.push({ name, closure: value });
    }
  }

  // Only when asked: the old rule, which declines every procedure a capture
  // could unwind through, decided over the whole set because the answer for
  // one depends on what its callees do.
  const unsafe = options.declineCaptures === true
    ? unsafeClosures(entries, env, { strict: options.strict === true })
    : new Map();

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
    if (lowered.captures && options.declineCaptures === true) {
      declined.push({ name, reason: 'captures a continuation, and captures are declined' });
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
 * procedure is correct, and it is done by default: nearly every capture is an
 * escape, which compiled code pays for easily. What does not pay is a
 * continuation re-entered over and over, and a procedure whose frames are is
 * switched back to its closure as the program runs -- which is why each
 * procedure is compiled over the closure the interpreter made of its
 * definition, and recorded so (`recordCompiledOver`), as the debugger needs
 * too. `declineCaptures` restores the old rule, from `safety.js`, which
 * explains the measured trade-off.
 *
 * @param {Array<Object>} asts - Analyzed top-level nodes, in order.
 * @param {Object} env - The environment to define into.
 * @param {Object} interpreter - The interpreter, for the remaining forms.
 * @param {Object} [options] - Options.
 * @param {boolean} [options.declineCaptures=false] - Decline every definition
 *   a capture could unwind through, and every one that captures.
 * @param {boolean} [options.strict=false] - Also decline a procedure that calls
 *   a callee it cannot name.
 * A top-level expression, or the value of a definition that is not a
 * procedure, is compiled as a thunk and called once where
 * `tryCompileExpression` accepts it, and run by the interpreter otherwise.
 *
 * @returns {{compiled: Array<string>, declined: Array<{name: string, reason: string}>,
 *   unitDeclined: (string|null), expressions: number, value: *}} What happened,
 *   and why if the unit was refused; how many expressions or definitions'
 *   values were compiled; and the last form's value.
 */
export function compileProgram(asts, env, interpreter, options = {}) {
  const compiled = [];
  const declined = [];

  // The old rule, only when asked: which definitions a capture could unwind
  // through, computed over the whole unit before anything is compiled,
  // because the answer for one procedure depends on what its callees do.
  const unsafe = options.declineCaptures === true
    ? unsafeDefinitions(asts, env, { strict: options.strict === true })
    : new Map();

  // Kept for callers that ask "was the unit refused wholesale?". It is no
  // longer how the decision is made; a unit that uses continuations in one
  // corner can now have the rest of it compiled.
  const firstUnsafe = unsafe.size > 0 ? [...unsafe.entries()][0] : null;
  const unitDeclined = firstUnsafe === null ? null : `${firstUnsafe[0]}: ${firstUnsafe[1]}`;

  const compileOptions = { declineCaptures: options.declineCaptures === true };
  const run = (ast) => interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' });
  let value;
  let expressions = 0;
  for (const ast of asts) {
    const unsafeReason = ast instanceof DefineNode ? unsafe.get(ast.name) : undefined;
    const procedure = ast instanceof DefineNode && ast.valueExpr instanceof LambdaNode;

    if (procedure) {
      // Defined as the interpreter defines it, then compiled from the closure
      // that made, so that the pair is recorded: a debugger runs the closure
      // while the program is debugged, and a procedure whose continuations
      // are re-entered is switched back to it for good.
      run(ast);
      value = undefined;
      const closure = env.lookup(ast.name);
      const result = unsafeReason !== undefined
        ? { compiled: false, reason: unsafeReason }
        : tryCompileClosure(closure, ast.name, compileOptions);
      if (result.compiled) {
        env.rebind(ast.name, result.procedure);
        recordCompiledOver(new Map([[closure, result.procedure]]), env);
        compiled.push(ast.name);
      } else {
        declined.push({ name: ast.name, reason: result.reason });
      }
      continue;
    }

    const result = unsafeReason !== undefined
      ? { compiled: false, reason: unsafeReason }
      : tryCompileExpression(ast instanceof DefineNode ? ast.valueExpr : ast, env, compileOptions);
    if (result.compiled) {
      // An expression, or a definition's value: called once, here.
      value = runCompiledThunk(interpreter, env, result.procedure);
      if (ast instanceof DefineNode) {
        env.define(ast.name, value);
        value = undefined;
      }
      expressions++;
    } else {
      // Run it the ordinary way. Order is preserved because this happens in
      // the same pass rather than in a second one.
      value = run(ast);
      if (ast instanceof DefineNode) declined.push({ name: ast.name, reason: result.reason });
    }
  }

  return { compiled, declined, unitDeclined, unsafe, expressions, value };
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
  // `substituteLibraryValues`. The closures are kept, for a debugger to run
  // instead (`recordCompiledOver`).
  substituteLibraryValues(replaced);
  recordCompiledOver(replaced, env);
  return { compiled, declined };
}

export { R as compilerRuntime };
