/**
 * @fileoverview Compiling a program's own code as it runs.
 *
 * The standard library is compiled at build time; a program's own procedures
 * are compiled here, while the program runs, by a tier attached to its
 * interpreter (`attachTier`). The interpreter reports two things to it: a
 * closure bound to a top-level name, by `define` or `set!`, and a waiting
 * closure's count of calls running out. It asks one thing: to run a top-level
 * form.
 *
 * ## When
 *
 * Generating a procedure's code costs about a millisecond, so compiling every
 * definition as it is made would cost a page with five hundred of them half a
 * second before anything ran, much of it for code run once. So:
 *
 *  - A top-level procedure whose body loops or makes procedures is compiled
 *    when it is bound. A loop inside a procedure called once is where a
 *    program spends its time and is what a call count could never catch.
 *  - Any other is compiled on its second call, so a procedure called once is
 *    never compiled.
 *  - A top-level expression is compiled only if it loops; see `runTopLevel`.
 *
 * Switching needs no on-stack replacement. Both tiers look a top-level name up
 * at every call, so once the compiled procedure is bound, the next call -- a
 * recursive call below frames already made, or the next iteration of a loop
 * written as a tail call -- runs compiled, and the frames already made finish
 * interpreted. A procedure nested in a top-level one is compiled with it.
 *
 * ## Over the closure, for the debugger
 *
 * Each procedure is compiled from the interpreted closure the program made,
 * and the pair is recorded (`recordCompiledOver`), so that while the program
 * is being debugged it runs as that closure again and its breakpoints fire, as
 * the standard library's do. Nothing is compiled while the program is being
 * debugged; a procedure that would have been is compiled on its first call
 * after.
 *
 * Nor while one of the program's libraries is being loaded. The compiler is
 * Scheme, and running it defines things; inside a library's body those
 * definitions would be registered with the scopes that library's macros
 * resolve their free identifiers through. So a library's procedures are
 * compiled from their first call once it has loaded, however they loop. An expression compiled as a thunk has no closure to go back to, which
 * is why only loops are: a procedure such a loop makes and keeps stays
 * compiled while the program is debugged.
 *
 * Kept in JavaScript beside `index.js`, whose entry points it calls: it is
 * the interpreter's side of compiling, working on closures, environments and
 * the interpreter's state, and decides nothing the compiler's Scheme decides.
 */

import { tryCompileClosure, tryCompileExpression, makesProceduresOrLoops } from './index.js';
import { unsafeClosures } from './safety.js';
import { globalContext } from '../core/interpreter/context.js';
import { recordCompiledOver, substituteLibraryValues } from '../core/interpreter/library_registry.js';
import { DefineNode, BeginNode, LetRecNode, LiteralNode, TailAppNode } from '../core/interpreter/ast_nodes.js';
import { Executable } from '../core/interpreter/stepables_base.js';

/**
 * How many calls a procedure that neither loops nor makes procedures waits
 * before it is compiled: compiled on its second.
 * @type {number}
 */
const CALLS_BEFORE_COMPILING = 2;

/**
 * Whether an analyzed form contains a loop: a named `let`, a `do`, or a group
 * of local procedures, which the analyzer makes a `LetRecNode` of.
 * @param {Object} ast - An analyzed form.
 * @returns {boolean}
 */
function containsLoop(ast) {
  const seen = new Set();
  const visit = (node) => {
    if (node === null || typeof node !== 'object' || seen.has(node)) return false;
    seen.add(node);
    if (Array.isArray(node)) return node.some(visit);
    if (!(node instanceof Executable)) return false;
    if (node instanceof LetRecNode) return true;
    return Object.values(node).some(visit);
  };
  return visit(ast);
}

/**
 * Whether an analyzed form defines at top level, directly or in a `begin`.
 * @param {Object} ast - An analyzed form.
 * @returns {boolean}
 */
function defines(ast) {
  return ast instanceof DefineNode || (ast instanceof BeginNode && ast.expressions.some(defines));
}

/**
 * Whether a closure is a program's own: made in its global environment or a
 * scope inside it, and not inside a library's, whose environment is a child
 * of the global one too.
 * @param {Function} closure - An interpreted closure.
 * @param {Object} env - The program's global environment.
 * @returns {boolean}
 */
function programsOwn(closure, env) {
  for (let scope = closure.env; scope; scope = scope.parent) {
    if (scope === env) return true;
    if (scope.libraryName !== undefined) return false;
  }
  return false;
}

/**
 * The compiler tier of one program.
 */
class Tier {
  /**
   * @param {Object} interpreter - The program's interpreter.
   * @param {Object} env - The program's global environment.
   * @param {Object} options - As for `attachTier`.
   */
  constructor(interpreter, env, options) {
    this.interpreter = interpreter;
    this.env = env;
    this.isPrebuilt = options.isPrebuilt ?? (() => false);
    this.declineCaptures = options.declineCaptures === true;

    /**
     * Closures waiting to be compiled, each mapped to the name it was bound to
     * and the environment that binds it.
     * @type {WeakMap<Function, {name: string, env: Object}>}
     */
    this.waiting = new WeakMap();

    /**
     * What happened to each top-level name the tier tried: `'compiled'`, or
     * why it was not.
     * @type {Map<string, string>}
     */
    this.outcomes = new Map();

    /**
     * How many top-level expressions were compiled.
     * @type {number}
     */
    this.expressions = 0;
  }

  /**
   * Whether an environment's top-level procedures are the tier's: the
   * program's own, or a library's the program loaded that has no prebuilt
   * table -- a shipped library's code is compiled at build time, and installed
   * over it as it loads.
   * @param {Object|null} env - The environment binding a name.
   * @returns {boolean}
   */
  manages(env) {
    if (!env) return false;
    if (env === this.env) return true;
    return env.libraryName !== undefined && !this.isPrebuilt(env.libraryName);
  }

  /**
   * A closure has been bound to a top-level name: compiled now if its body
   * loops or makes procedures, and set to wait for its second call otherwise.
   * @param {string} name - The name.
   * @param {Function} closure - The closure.
   * @param {Object|null} env - The environment that binds the name.
   */
  bound(name, closure, env) {
    if (!this.manages(env)) return;
    this.waiting.set(closure, { name, env });
    if (this.deferring()) {
      closure.tierCountdown = 1;
    } else if (makesProceduresOrLoops(closure.body)) {
      this.compile(closure);
    } else {
      closure.tierCountdown = CALLS_BEFORE_COMPILING;
    }
  }

  /**
   * A waiting closure's calls have run out. While compiling must wait, it
   * waits for one more call.
   * @param {Function} closure - The closure.
   */
  due(closure) {
    if (this.deferring()) {
      closure.tierCountdown = 1;
      return;
    }
    this.compile(closure);
  }

  /**
   * Whether compiling now must wait: while the program is being debugged, or a
   * library is being loaded.
   * @returns {boolean}
   */
  deferring() {
    return this.interpreter.debugging || globalContext.definingScopes.length > 0;
  }

  /**
   * Compiles a waiting closure and binds the compiled procedure in its place,
   * if its name still holds it and the compiler accepts it.
   * @param {Function} closure - The closure.
   */
  compile(closure) {
    const entry = this.waiting.get(closure);
    this.waiting.delete(closure);
    closure.tierCountdown = 0;
    if (entry === undefined) return;
    const { name, env } = entry;
    // Rebound since: another closure, and its own count, have the name now.
    if (env.bindings.get(name) !== closure) return;

    // A procedure that captures, or reaches one that does, is compiled too:
    // one whose continuations are re-entered over and over is switched back
    // to its closure as the program runs (`noteResume` in
    // `src/core/interpreter/unwind.js`). The old rule declined them all.
    if (this.declineCaptures) {
      const unsafe = unsafeClosures([{ name, closure }], env).get(name);
      if (unsafe !== undefined) {
        this.outcomes.set(name, unsafe);
        return;
      }
    }
    const result = tryCompileClosure(closure, name, { declineCaptures: this.declineCaptures });
    if (!result.compiled) {
      this.outcomes.set(name, result.reason);
      return;
    }
    this.install(closure, result.procedure, name, env);
    this.outcomes.set(name, 'compiled');
  }

  /**
   * Binds a compiled procedure where its closure was bound: under its name,
   * under any other name in the program holding the closure, and -- for a
   * library's procedure -- in every library and in the program that imported
   * it, since an import copies the value.
   * @param {Function} closure - The closure.
   * @param {Function} procedure - What it compiled to.
   * @param {string} name - The name that binds it.
   * @param {Object} env - The environment that binds it.
   */
  install(closure, procedure, name, env) {
    const replaced = new Map([[closure, procedure]]);
    env.rebind(name, procedure);
    for (const [other, value] of this.env.bindings) {
      if (value === closure) this.env.rebind(other, procedure);
    }
    if (env !== this.env) substituteLibraryValues(replaced);
    recordCompiledOver(replaced, this.env);
  }

  /**
   * Runs a top-level form, compiling it first if it is an expression that
   * loops. An expression that only makes procedures is interpreted: the
   * procedures it binds are compiled when bound, over their closures, and one
   * it makes and keeps elsewhere would, compiled here, have no closure for a
   * debugger to go back to. Definitions run interpreted, which is where the
   * tier sees what they bind.
   * @param {Object} ast - The analyzed form.
   * @param {Object} env - The environment to run it in.
   * @param {Object} [options] - As for `Interpreter.run`.
   * @returns {*} Its value.
   */
  runTopLevel(ast, env, options) {
    if (!this.deferring() && env === this.env && !defines(ast) && containsLoop(ast)) {
      const result = tryCompileExpression(ast, env);
      if (result.compiled) {
        this.expressions++;
        return this.interpreter.run(new TailAppNode(new LiteralNode(result.procedure), []), env, [], undefined, options);
      }
    }
    return this.interpreter.run(ast, env, [], undefined, options);
  }
}

/**
 * Attaches a compiler tier to a program's interpreter, so that the program's
 * own procedures are compiled as it runs. Procedures the program has already
 * bound at top level are taken on as if bound now.
 *
 * @param {Object} interpreter - The interpreter.
 * @param {Object} env - The program's global environment.
 * @param {Object} [options] - Options.
 * @param {function(Array<string>): boolean} [options.isPrebuilt] - Whether a
 *   library has a prebuilt table installed over it as it loads, so that its
 *   procedures are not the tier's. None has, by default.
 * @param {boolean} [options.declineCaptures=false] - Decline procedures that
 *   capture a continuation, or reach one that does, as the tier once did.
 * @returns {Tier|null} The tier, or null if code cannot be generated here -- a
 *   Content-Security-Policy forbids `new Function` -- and the program runs
 *   interpreted, which is a tier and not a failure. The compiler itself starts
 *   at the first procedure compiled, so a program that compiles nothing does
 *   not wait for it; if it cannot start, every procedure is declined.
 */
export function attachTier(interpreter, env, options = {}) {
  try {
    new Function('return 1');
  } catch (e) {
    return null;
  }

  const tier = new Tier(interpreter, env, options);
  interpreter.tier = tier;
  for (const [name, value] of [...env.bindings]) {
    if (typeof value === 'function' && value.body !== undefined && programsOwn(value, env)) {
      tier.bound(name, value, env);
    }
  }
  return tier;
}

/**
 * Stops compiling a program's code. What is compiled stays compiled.
 * @param {Object} interpreter - The interpreter.
 */
export function detachTier(interpreter) {
  interpreter.tier = null;
}
