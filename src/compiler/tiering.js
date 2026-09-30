/**
 * @fileoverview Compiling a program's own code as it runs: attaching the tier
 * to a program's interpreter.
 *
 * The tier's decisions -- which procedures, when, and what to do with the
 * compiled ones -- are Scheme, in `tier.scm`. What is here is the
 * interpreter's end of it: the object the interpreter holds as
 * `interpreter.tier`, whose three methods it calls where a closure is bound to
 * a top-level name, where a waiting closure's calls run out, and for each
 * top-level form, and which passes each on to the Scheme.
 */

import { LiteralNode, TailAppNode } from '../core/interpreter/ast_nodes.js';
import { callCompiler } from './lowering.js';

/**
 * The compiler tier of one program, as its interpreter sees it.
 */
class Tier {
  /**
   * @param {Object} interpreter - The program's interpreter.
   * @param {Object} state - The tier, as `make-tier` made it.
   * @param {Map<string, string>} outcomes - What happened to each top-level
   *   name the tier tried: `'compiled'`, or why it was not.
   */
  constructor(interpreter, state, outcomes) {
    this.interpreter = interpreter;
    this.state = state;
    this.outcomes = outcomes;
  }

  /**
   * How many top-level expressions the tier compiled.
   * @type {number}
   */
  get expressions() {
    return Number(this.state.expressions);
  }

  /**
   * A closure has been bound to a top-level name.
   * @param {string} name - The name.
   * @param {Function} closure - The closure.
   * @param {Object|null} env - The environment that binds the name.
   */
  bound(name, closure, env) {
    callCompiler('tier-bound!', [this.state, name, closure, env ?? false]);
  }

  /**
   * A waiting closure's calls have run out.
   * @param {Function} closure - The closure.
   */
  due(closure) {
    callCompiler('tier-due!', [this.state, closure]);
  }

  /**
   * Runs a top-level form, compiled if the tier compiles it.
   * @param {Object} ast - The analyzed form.
   * @param {Object} env - The environment to run it in.
   * @param {Object} [options] - As for `Interpreter.run`.
   * @returns {*} Its value.
   */
  runTopLevel(ast, env, options) {
    const thunk = callCompiler('tier-top-level-procedure', [this.state, ast, env]);
    const form = thunk ? new TailAppNode(new LiteralNode(thunk), []) : ast;
    return this.interpreter.run(form, env, [], undefined, options);
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
 *   Content-Security-Policy forbids `new Function` -- or the compiler could not
 *   start, and the program runs interpreted, which is a tier and not a failure.
 */
export function attachTier(interpreter, env, options = {}) {
  const outcomes = new Map();
  const state = callCompiler('make-tier',
    [interpreter, env, options.isPrebuilt ?? (() => false), options.declineCaptures === true, outcomes]);
  if (!state) return null;
  const tier = new Tier(interpreter, state, outcomes);
  interpreter.tier = tier;
  return tier;
}

/**
 * Stops compiling a program's code. What is compiled stays compiled.
 * @param {Object} interpreter - The interpreter.
 */
export function detachTier(interpreter) {
  interpreter.tier = null;
}
