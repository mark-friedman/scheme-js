/**
 * @fileoverview Runtime support for compiled Scheme procedures.
 *
 * Deliberately thin. Compiled code uses the interpreter's own value
 * representation and its own primitives, so there is no parallel runtime to
 * keep in step: a `Cons` is a `Cons`, an exact integer is a `BigInt`, and `+`
 * is the same function the interpreter calls.
 *
 * There *is* one conversion at the boundary, and getting it wrong cost ten of
 * the canonical R7RS benchmarks (R26). An interpreted closure is a callable
 * JavaScript function whose wrapper exists for JavaScript callers, so calling
 * it the ordinary way converts values as if they were leaving Scheme. `invoke`
 * below is how compiled code avoids that.
 */

import { TailCall, SCHEME_PRIMITIVE, SCHEME_RAW_CALL } from '../core/interpreter/values.js';
import { Cons } from '../core/interpreter/cons.js';

export { TailCall, Cons, SCHEME_RAW_CALL };

/**
 * Invokes a callee with Scheme values, without crossing the JavaScript boundary.
 *
 * A compiled procedure and a JavaScript primitive are both plain functions that
 * take Scheme values, so they are called directly. An *interpreted* closure is
 * also a plain function, but calling it that way means entering Scheme from
 * JavaScript, and its wrapper converts accordingly -- exact integers become
 * doubles, bignums beyond 2^53 throw. Compiled code is not a JavaScript caller,
 * so it uses the closure's raw entry instead.
 *
 * The check is one property load on a value already in hand. It is worth
 * stating why it cannot be hoisted to compile time: the callee of a Scheme call
 * is a value, not a name, and which tier it belongs to is not known until the
 * call happens.
 *
 * @param {Function} fn - The callee.
 * @param {Array<*>} args - Scheme values.
 * @returns {*} The callee's result, which may be a pending `TailCall`.
 */
export function invoke(fn, args) {
  const raw = fn[SCHEME_RAW_CALL];
  return raw === undefined ? fn(...args) : raw(...args);
}

/**
 * Performs one step of a pending tail call.
 *
 * Compiled procedures signal a tail call by returning the interpreter's own
 * `TailCall`, which is why an interpreted procedure and a compiled one are
 * interchangeable at a call site in either direction.
 *
 * @param {TailCall} pending - The pending call.
 * @returns {*} The callee's result, which may itself be a `TailCall`.
 */
export function step(pending) {
  return invoke(pending.func, pending.args);
}

/**
 * Drives a call to completion, resolving any chain of tail calls.
 * @param {*} value - A value or a pending `TailCall`.
 * @returns {*} The final value.
 */
export function settle(value) {
  while (value instanceof TailCall) {
    value = invoke(value.func, value.args);
  }
  return value;
}

/**
 * Builds a memoizing accessor for a global binding.
 *
 * Compiled code cannot capture a global's value at compile time: a definition
 * may be forward-referenced, and Scheme allows a top-level binding to be
 * redefined afterwards -- by a later `define`, or by the REPL running the
 * compiled code. The accessor therefore resolves lazily, and re-resolves if the
 * binding is replaced, while costing a single property load in the common case.
 *
 * @param {Object} env - The environment to resolve in.
 * @param {string} name - The renamed global's name.
 * @returns {function(): *} An accessor returning the current value.
 */
export function globalAccessor(env, name) {
  let holder = null;
  return () => {
    // The frame holding the binding is found once and then remembered, so a
    // reference costs a single hash lookup rather than a walk up the
    // environment chain. Profiling the first version, which called `findEnv`
    // on every reference, put 11% of compiled runtime in this function and the
    // chain walk it performed.
    //
    // Caching the frame rather than the value is what keeps it correct: a later
    // `define` or `set!` mutates that frame's map in place, so the new value is
    // observed. Scheme has no way to *remove* a binding, so a frame that once
    // held the name still holds it.
    if (holder !== null) return holder.bindings.get(name);
    holder = env.findEnv(name);
    if (holder === null) {
      // Not yet defined -- a forward reference, or a JavaScript global. Resolve
      // the slow way and do not cache, so a later definition is picked up.
      return env.lookup(name);
    }
    return holder.bindings.get(name);
  };
}

/**
 * Returns the value a name is bound to right now, or null if it is unbound.
 *
 * Used at compile time to identify which primitive a global currently denotes,
 * so generated code can guard an inline fast path against that exact function
 * and fall back if the binding is ever replaced.
 *
 * @param {Object} env - The environment.
 * @param {string} name - The name.
 * @returns {*} The current value, or null if unbound.
 */
export function currentBinding(env, name) {
  const holder = env.findEnv(name);
  return holder === null ? null : holder.bindings.get(name);
}

/**
 * Builds a Scheme list from an array, for a rest parameter.
 * @param {Array<*>} items - The trailing arguments.
 * @returns {*} A proper Scheme list.
 */
export function listFrom(items) {
  let list = null;
  for (let i = items.length - 1; i >= 0; i--) list = new Cons(items[i], list);
  return list;
}

/**
 * Marks a generated function as a Scheme procedure.
 *
 * `SCHEME_PRIMITIVE` tells the interpreter this function speaks Scheme values
 * directly, so it must not convert arguments at the boundary the way it does
 * for a foreign JavaScript function -- that conversion would turn exact
 * integers into doubles.
 *
 * @param {Function} fn - The generated function.
 * @param {string} name - The Scheme procedure's name, for stack traces.
 * @returns {Function} The same function.
 */
export function markProcedure(fn, name) {
  fn[SCHEME_PRIMITIVE] = true;
  fn.$compiled = true;
  fn.schemeName = name;
  fn.toString = () => `#<compiled-procedure${name && name !== 'anonymous' ? ' ' + name : ''}>`;
  return fn;
}
