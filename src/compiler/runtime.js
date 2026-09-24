/**
 * @fileoverview Runtime support for compiled Scheme procedures.
 *
 * Deliberately thin. Compiled code uses the interpreter's own value
 * representation and its own primitives, so there is no parallel runtime to
 * keep in step: a `Cons` is a `Cons`, an exact integer is a `BigInt`, and `+`
 * is the same function the interpreter calls.
 *
 * There *is* one conversion at the boundary, and getting it wrong silently
 * corrupted ten benchmarks. An interpreted closure is a callable
 * JavaScript function whose wrapper exists for JavaScript callers, so calling
 * it the ordinary way converts values as if they were leaving Scheme. `invoke`
 * below is how compiled code avoids that.
 */

import { TailCall, SCHEME_PRIMITIVE, SCHEME_RAW_CALL } from '../core/interpreter/values.js';
// The capture protocol belongs to the interpreter, which owns what a
// continuation is; this module only makes it reachable from generated code.
import { UNWIND, reify, beginCompiledCapture } from '../core/interpreter/unwind.js';
import { SchemeError } from '../core/interpreter/errors.js';
import { Cons } from '../core/interpreter/cons.js';
// Kept by the interpreter, which sees every binding write; generated code only
// reads a cell, once per inlined primitive.
import { primitiveCell } from '../core/interpreter/primitive_bindings.js';

export { TailCall, Cons, SCHEME_RAW_CALL, UNWIND, reify, SchemeError, primitiveCell };

/**
 * Reports a capture beneath a redefined inlined primitive.
 *
 * An inline expansion is not a call site the resumable form splits at, so there
 * is no point for the frame to resume from. Only reachable when the guarded
 * binding has been replaced by something that captures.
 *
 * A function rather than a thrown literal at each site because the message is
 * long and there are hundreds of sites: inlining it accounted for a fifth of
 * the generated standard library.
 *
 * @returns {void}
 * @throws {SchemeError} Always.
 */
export function captureUnderPrimitive() {
  throw new SchemeError(
    'call/cc: a continuation was captured beneath a redefined primitive, which '
    + 'cannot be resumed. Run this program with the compiler tier disabled.');
}

/**
 * Calls whatever a primitive's name is bound to now, from an inline expansion
 * whose fast path does not apply.
 *
 * Reached when the binding is no longer the primitive that was inlined, so it
 * may be anything at all -- including an interpreted closure, whose plain call
 * signature converts Scheme values as if they were leaving Scheme, and which
 * may return a pending tail call; hence `invoke` and `settle`. Also reached
 * when the operands are outside the fast path, with the primitive itself.
 *
 * A redefinition that captures a continuation has nowhere to resume: an inline
 * expansion is not a call site the resumable form splits at. The check for
 * that lives here, on the slow path, rather than after every expansion, where
 * it was a statement per inlined primitive that the fast path never needed.
 *
 * @param {Function} fn - The name's current binding.
 * @param {Array<*>} args - Scheme values.
 * @returns {*} The call's value.
 * @throws {SchemeError} If a continuation was captured beneath it.
 */
export function callBinding(fn, args) {
  const value = settle(invoke(fn, args));
  if (value === UNWIND) captureUnderPrimitive();
  return value;
}

/**
 * Reports a capture in a procedure with no resumable form.
 * @returns {void}
 * @throws {SchemeError} Always.
 */
export function captureWithoutResume() {
  throw new SchemeError(
    'call/cc: a continuation was captured in or beneath a compiled procedure '
    + 'that has no resumable form. Run this program with the compiler tier '
    + 'disabled.');
}

/**
 * Captures the current continuation from compiled code.
 *
 * Compiled code cannot build a continuation itself: a continuation is the
 * interpreter's frame stack, and the compiled frames between here and the
 * interpreter are JavaScript frames that nothing can read. So this does not
 * return one. It records what the capture will need and returns the unwind
 * sentinel, which makes every compiled frame on the way out record itself --
 * the same protocol as a capture made by an interpreted callee, entered from
 * the other end.
 *
 * The value therefore arrives later, at the resume point, rather than from
 * this call. Generated code is written accordingly: it spills and returns
 * immediately afterwards.
 *
 * @param {*} receiver - The procedure to apply to the continuation.
 * @returns {symbol} `UNWIND`, always.
 */
export function capture(receiver) {
  beginCompiledCapture(receiver);
  return UNWIND;
}

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
 * Used at compile time to decide whether a global still denotes the primitive
 * an inline expansion reproduces; an expansion is only emitted if it does.
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

/**
 * Records where a compiled procedure's source is, under the same property an
 * interpreted closure uses.
 *
 * Generated code cannot know this -- it is produced from IR, which carries no
 * positions -- so it is attached afterwards by whatever installs the procedure,
 * from the closure or definition it replaces. Using the interpreted closure's
 * own property name means the debugger asks one question of either tier:
 * `procedure.source` says where it was defined, and `$compiled` says whether it
 * will stop at a breakpoint there.
 *
 * @param {Function} procedure - A compiled procedure.
 * @param {Object|null|undefined} source - The span it was compiled from.
 * @returns {Function} The same procedure.
 */
export function recordSource(procedure, source) {
  if (source) procedure.source = source;
  return procedure;
}
