/**
 * @fileoverview Compiling a program's own code as it runs: attaching the tier
 * to a program's interpreter.
 *
 * The tier is Scheme, in `tier.scm`: which procedures to compile, when, and
 * what to do with the compiled ones. Its record, which the interpreter holds
 * as `interpreter.tier`, carries a Scheme procedure for each thing the
 * interpreter tells or asks it -- a closure bound to a top-level name, a
 * waiting closure's calls run out, a top-level form to run -- and the
 * interpreter calls them itself. What is here only makes the record and hands
 * it to the interpreter.
 */

import { callCompiler } from './lowering.js';

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
 * @returns {Object|null} The tier's record, whose `outcomes` is a `Map` from
 *   each name the tier tried to `'compiled'` or why not, and whose
 *   `expressions` counts the top-level forms it compiled; or null if code
 *   cannot be generated here -- a Content-Security-Policy forbids `new
 *   Function` -- or the compiler could not start, and the program runs
 *   interpreted, which is a tier and not a failure.
 */
export function attachTier(interpreter, env, options = {}) {
  const tier = callCompiler('make-tier',
    [interpreter, env, options.isPrebuilt ?? (() => false), options.declineCaptures === true, new Map()]);
  if (!tier) return null;
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
