/**
 * @fileoverview Which procedures the compiler tier declines, and why.
 *
 * ## What this used to be for, and no longer is
 *
 * A compiled procedure runs in a JavaScript stack frame, which nothing can read
 * back, so it could not appear in a captured continuation the way an
 * interpreted frame does. A capture made while such a frame was live therefore
 * dropped everything that frame still had to do, and the program got a **wrong
 * answer rather than an error**. Two benchmarks demonstrated it:
 *
 *  - `btsearch`: `enumerate` had to be re-entered when the search backtracked.
 *  - `maze` (`benchmarks/r7rs/src/maze.scm`): `dig-maze` escapes with
 *    `(quit #f)`, and the escape unwound past compiled `make-maze`, which then
 *    returned `#f` -- the escape value itself.
 *
 * Neither `enumerate` nor `make-maze` mentions `call/cc`, so declining
 * procedures by what they *name* could never have worked. This module exists
 * because of that, and closed the rule over the call graph instead.
 *
 * Compiled procedures can now put themselves into a continuation, so none of
 * that is a correctness argument any more. See
 * `src/core/interpreter/unwind.js`. What is left here is a **performance**
 * rule, and a useful one: a procedure that a capture repeatedly unwinds through
 * pays to suspend and resume every time, which costs more than interpreting it
 * outright. Measured on `btsearch`, declining those procedures is the
 * difference between 1.82x faster and 2x slower.
 *
 * ## The rule
 *
 * A procedure is declined if it can reach a control-transferring global:
 * directly, through another procedure in the same unit, or through an
 * interpreted closure already in the environment -- which is how a standard
 * library procedure that captures gets caught.
 *
 * A procedure that captures a continuation is declined on the same footing.
 * That is a change of reason rather than of behaviour: it used to be declined
 * because `call/cc` could not be compiled at all, and is now declined because
 * doing so is slower.
 *
 * `strict` additionally declines a procedure that calls a callee the compiler
 * cannot name -- a parameter, typically -- since the caller has no way to know
 * what it was handed. That was once necessary, because it is the only way to
 * catch the `btsearch` shape, where the capture arrives as an argument. It is
 * now off by default: it declines most higher-order code for a correctness
 * benefit that no longer exists, and on `btsearch` and `oddeven` it costs about
 * 1.8x each.
 *
 * ## What this rule never caught, and does not need to
 *
 * A global rebound *after* compilation to something that captures. Compiled
 * code resolves globals through a live accessor, so it calls the new binding,
 * and nothing re-runs this analysis. That is no longer a soundness hole: such a
 * capture is handled like any other.
 */

import { lowerLambda, controlGlobalIn } from './lowering.js';
import { LambdaNode, DefineNode } from '../core/interpreter/ast_nodes.js';
import { isSchemeClosure } from '../core/interpreter/values.js';

/**
 * Describes what one procedure references, for the reachability closure.
 * @typedef {Object} Facts
 * @property {Set<string>} globals - Globals it references.
 * @property {boolean} callsUnknown - Whether it calls a callee it cannot name.
 * @property {string|null} control - A control global it names directly, if any.
 * @property {boolean} captures - Whether it captures a continuation itself.
 */

/**
 * Extracts the reference facts for an analyzed lambda.
 * @param {Object} lambdaNode - A `LambdaNode`.
 * @returns {Facts|null} The facts, or null if it could not be lowered.
 */
function factsForLambda(lambdaNode) {
  const lowered = lowerLambda(lambdaNode);
  if (lowered.reason !== undefined) return null;
  return {
    globals: lowered.globals,
    callsUnknown: lowered.callsUnknown,
    control: controlGlobalIn(lowered.globals),
    captures: lowered.captures === true
  };
}

/**
 * Rebuilds the lambda behind an interpreted closure.
 *
 * A Scheme closure retains its parameters, body and defining environment, so a
 * procedure that was loaded and interpreted can still be analysed -- which is
 * what lets this reach into the standard library rather than assuming
 * everything outside the unit is safe.
 *
 * @param {Function} closure - An interpreted Scheme closure.
 * @param {string} name - Its name, for diagnostics.
 * @returns {Object|null} A `LambdaNode`, or null if this is not a closure.
 */
function lambdaOfClosure(closure, name) {
  if (typeof closure !== 'function' || closure.body === undefined) return null;
  return new LambdaNode(
    closure.params, closure.body, closure.restParam, name,
    closure.originalParams, closure.originalRestParam);
}

/**
 * Decides which of a unit's definitions are safe to compile.
 *
 * @param {Array<Object>} asts - The unit's analyzed top-level nodes, in order.
 * @param {Object} env - The environment the unit is being defined into.
 * @param {Object} [options] - Options.
 * @param {boolean} [options.strict=false] - Also decline a procedure that calls
 *   a callee it cannot name. Conservative, and costly: it declines most
 *   higher-order code.
 * @returns {Map<string, string>} Declined definition names, each mapped to the
 *   reason -- a path the reader can follow back to a control global.
 */
export function unsafeDefinitions(asts, env, options = {}) {
  /** @type {Map<string, Facts>} Facts for definitions in this unit. */
  const local = new Map();
  for (const ast of asts) {
    if (!(ast instanceof DefineNode)) continue;
    const value = ast.valueExpr ?? ast.value;
    if (!(value instanceof LambdaNode)) continue;
    const facts = factsForLambda(value);
    if (facts !== null) local.set(ast.name, facts);
  }
  return unsafeFromFacts(local, env, options);
}

/**
 * Decides which of a set of already-created closures are safe to compile.
 *
 * The same question as `unsafeDefinitions` asks, for procedures that exist as
 * values rather than as source. That is the standard library's situation: it
 * has been loaded and interpreted before anything considers compiling it, and
 * a closure retains its parameters, body and defining environment, so it can
 * still be analysed as a unit with its neighbours.
 *
 * @param {Array<{name: string, closure: Function}>} entries - The procedures.
 * @param {Object} env - The environment they live in.
 * @param {Object} [options] - Options, as for `unsafeDefinitions`.
 * @returns {Map<string, string>} Declined names, each mapped to the reason.
 */
export function unsafeClosures(entries, env, options = {}) {
  /** @type {Map<string, Facts>} */
  const local = new Map();
  for (const { name, closure } of entries) {
    const lambda = lambdaOfClosure(closure, name);
    if (lambda === null) continue;
    const facts = factsForLambda(lambda);
    if (facts !== null) local.set(name, facts);
  }
  return unsafeFromFacts(local, env, options);
}

/**
 * The reachability closure itself, over facts already gathered.
 *
 * @param {Map<string, Facts>} local - Facts for the procedures being decided.
 * @param {Object} env - The environment to resolve other names in.
 * @param {Object} options - Options, as for `unsafeDefinitions`.
 * @returns {Map<string, string>} Declined names, each mapped to the reason.
 */
function unsafeFromFacts(local, env, options) {
  const { strict = false } = options;

  // Memoized verdict for globals *outside* the unit, so the standard library is
  // walked once per unit rather than once per reference.
  /** @type {Map<string, string|null>} */
  const external = new Map();

  /**
   * Whether a global outside the unit can reach a control transfer.
   * @param {string} name - The global's name.
   * @param {Set<string>} visiting - Names on the current path, to stop cycles.
   * @returns {string|null} A reason, or null if it looks safe.
   */
  function externalReason(name, visiting) {
    if (external.has(name)) return external.get(name);
    if (visiting.has(name)) return null;   // a cycle is not itself evidence
    visiting.add(name);

    // Resolved optimistically: a primitive not on the control list, a compiled
    // procedure (already vetted), or an unbound forward reference is taken as
    // safe. Only an interpreted closure can be looked inside.
    let verdict = null;
    const holder = env.findEnv(name);
    const value = holder === null ? undefined : holder.bindings.get(name);
    if (isSchemeClosure(value) || (typeof value === 'function' && value.body !== undefined)) {
      const lambda = lambdaOfClosure(value, name);
      const facts = lambda === null ? null : factsForLambda(lambda);
      if (facts === null) {
        // Cannot be lowered, so cannot be vetted. Treated as safe: this is the
        // same assumption the tier already makes about every primitive, and
        // tightening it here would decline nearly everything.
        verdict = null;
      } else if (facts.control !== null) {
        verdict = `${name} references '${facts.control}'`;
      } else if (facts.captures) {
        verdict = `${name} captures a continuation`;
      } else if (strict && facts.callsUnknown) {
        verdict = `${name} calls a procedure it is given`;
      } else {
        for (const g of facts.globals) {
          const inner = externalReason(g, visiting);
          if (inner !== null) { verdict = `${name} -> ${inner}`; break; }
        }
      }
    }

    visiting.delete(name);
    external.set(name, verdict);
    return verdict;
  }

  /** @type {Map<string, string>} */
  const unsafe = new Map();

  // Base cases: a direct reference, or -- under `strict` -- a call to something
  // the procedure was handed.
  for (const [name, facts] of local) {
    if (facts.control !== null) {
      unsafe.set(name, `references control global '${facts.control}'`);
    } else if (facts.captures) {
      // The compiler *can* compile this -- a capture is an ordinary call site
      // that suspends. It is held back because it is slower: every capture
      // unwinds and reifies the frames between it and the interpreter, and a
      // program that captures in a loop pays that each time. Measured on
      // `btsearch`, compiling it is the difference between 2.00x faster and
      // 2x slower; `ctak` goes from 0.99x to 0.69x. Two programs do improve --
      // `contfib` 1.03x to 1.92x, `threads` 1.12x to 1.66x -- so this is a
      // default rather than a rule, and `allowContinuationUnsafe` lifts it.
      unsafe.set(name, 'captures a continuation, which costs more compiled than interpreted');
    } else if (strict && facts.callsUnknown) {
      unsafe.set(name, 'calls a procedure it is given, which may capture a continuation');
    }
  }

  // Reachability into the environment, which the fixpoint below does not cover
  // because these names are not defined in this unit.
  for (const [name, facts] of local) {
    if (unsafe.has(name)) continue;
    for (const g of facts.globals) {
      if (local.has(g)) continue;
      const reason = externalReason(g, new Set());
      if (reason !== null) { unsafe.set(name, `reaches ${reason}`); break; }
    }
  }

  // Fixpoint over the unit's own call graph.
  let changed = true;
  while (changed) {
    changed = false;
    for (const [name, facts] of local) {
      if (unsafe.has(name)) continue;
      for (const g of facts.globals) {
        if (!unsafe.has(g)) continue;
        unsafe.set(name, `reaches ${g}, which ${unsafe.get(g)}`);
        changed = true;
        break;
      }
    }
  }

  return unsafe;
}
