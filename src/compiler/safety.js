/**
 * @fileoverview Which procedures are safe for the compiler tier to take.
 *
 * ## The problem
 *
 * A compiled procedure runs in a JavaScript stack frame that the interpreter's
 * frame stack does not represent. If a continuation is captured while that
 * frame is live, the frame cannot be restored, and the program gets a **wrong
 * answer rather than an error**. Two benchmarks have demonstrated it:
 *
 *  - `btsearch` (R15): `enumerate` had to be re-entered when the search
 *    backtracked.
 *  - `maze` (R34): `dig-maze` escapes with `(quit #f)` and the escape unwound
 *    past compiled `make-maze`, which returned `#f` -- the escape value.
 *
 * Neither `enumerate` nor `make-maze` mentions `call/cc`. Declining procedures
 * by what they *name* therefore cannot work, and that is what
 * `tryCompileDefinition` still does on its own.
 *
 * ## The two rules that were tried, and why neither is usable
 *
 * | rule | sound? | useful? |
 * |---|---|---|
 * | decline a procedure that names a control global | no -- both cases above | yes |
 * | decline the whole unit if any procedure names one | for a closed unit | **no**: one `apply` anywhere disables everything, and 0 of 41 canonical benchmarks compiled |
 *
 * ## What this module does instead
 *
 * Closes the rule over the **call graph**. A procedure is unsafe if it can
 * reach a control global: directly, through another procedure in the same unit,
 * or through an interpreted closure already in the environment -- which is how
 * a standard-library procedure that captures gets caught.
 *
 * `strict` additionally treats a call to a callee the compiler cannot name -- a
 * parameter, typically -- as unsafe, because the caller has no way to know what
 * it was handed. That is the `btsearch` case: `enumerate` invokes `cont`.
 * Without it the analysis catches `maze` and misses `btsearch`.
 *
 * ## What it still does not catch
 *
 * A global rebound *after* compilation to something that captures. Compiled
 * code resolves globals through a live accessor, so it would call the new
 * binding, and nothing re-runs this analysis. Closing that needs re-enterable
 * frames -- increment 2b -- which is the real fix and makes all of this
 * unnecessary. This module is the interim that makes the tier simultaneously
 * usable and hard to trip, which neither earlier rule managed.
 */

import { lowerLambda, controlGlobalIn } from './ir.js';
import { LambdaNode, DefineNode } from '../core/interpreter/ast_nodes.js';
import { isSchemeClosure } from '../core/interpreter/values.js';

/**
 * Describes what one procedure references, for the reachability closure.
 * @typedef {Object} Facts
 * @property {Set<string>} globals - Globals it references.
 * @property {boolean} callsUnknown - Whether it calls a callee it cannot name.
 * @property {string|null} control - A control global it names directly, if any.
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
    control: controlGlobalIn(lowered.globals)
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
 * @param {boolean} [options.strict=true] - Treat a call to an unnameable callee
 *   as unsafe. Required to catch the `btsearch` shape.
 * @returns {Map<string, string>} Unsafe definition names, each mapped to the
 *   reason -- a path the reader can follow back to a control global.
 */
export function unsafeDefinitions(asts, env, options = {}) {
  const { strict = true } = options;

  /** @type {Map<string, Facts>} Facts for definitions in this unit. */
  const local = new Map();
  for (const ast of asts) {
    if (!(ast instanceof DefineNode)) continue;
    const value = ast.valueExpr ?? ast.value;
    if (!(value instanceof LambdaNode)) continue;
    const facts = factsForLambda(value);
    if (facts !== null) local.set(ast.name, facts);
  }

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
