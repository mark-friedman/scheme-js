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
 * `vector-ref` for an inline expansion, whose guard has established that the
 * name is still bound to the primitive.
 *
 * The common case -- an array and an exact index inside it -- converts the
 * index to a JavaScript number once and compares numbers. Doing the same
 * checks inline was slower than the primitive, because comparing a `bigint`
 * with a length costs more than converting it; and calling the primitive
 * through the generic call path cost vector-heavy programs up to half their
 * time, measured as a ceiling. Every other case, and so every error, is the
 * primitive's own.
 *
 * @param {*} vector - The vector operand.
 * @param {*} index - The index operand.
 * @returns {*} The element.
 */
export function vectorRef(vector, index) {
  if (Array.isArray(vector) && typeof index === 'bigint') {
    const i = Number(index);
    if (i >= 0 && i < vector.length) return vector[i];
  }
  return primitiveCell('vector-ref').primitive(vector, index);
}

/**
 * `vector-set!` for an inline expansion, as `vectorRef` is for `vector-ref`.
 * @param {*} vector - The vector operand.
 * @param {*} index - The index operand.
 * @param {*} value - The value to store.
 * @returns {*} What the primitive returns.
 */
export function vectorSet(vector, index, value) {
  if (Array.isArray(vector) && typeof index === 'bigint') {
    const i = Number(index);
    if (i >= 0 && i < vector.length) {
      vector[i] = value;
      return null;
    }
  }
  return primitiveCell('vector-set!').primitive(vector, index, value);
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
 * The cell a global read resolves to before its first read.
 *
 * Generated code reads a global as `(C.v ?? G())`: the cell's value, or, when
 * that is `undefined` or `null`, the resolver `G`, which finds the real cell
 * with `globalCell`, keeps it in `C` and returns its value. Starting every
 * site at this cell sends its first read to the resolver. A value that really
 * is `null` -- the empty list -- or `undefined` takes the resolver on every
 * read, which is slower and still right.
 *
 * @type {{v: undefined}}
 */
export const UNRESOLVED = Object.freeze({ v: undefined });

/**
 * Resolves a global for compiled code: the cell of the frame holding it.
 *
 * Compiled code cannot take a global's value at compile time: a definition
 * may be a forward reference, and Scheme allows a top-level binding to be
 * redefined or assigned afterwards -- by a later `define`, a `set!`, or the
 * REPL running the compiled code. The frame holding the name keeps a cell for
 * it current through every write (`Environment.cellFor`), so compiled code
 * reads the cell, one property load, rather than looking the name up in the
 * frame's map on every reference as it used to -- a hash lookup that cost up
 * to 1.5x of compiled run time on call-heavy code.
 *
 * The frame is found once. A later `define` of the same name in a frame
 * nearer the reader would shadow it and go unseen; that was equally true of
 * the lookup this replaces, and does not arise for the top-level and library
 * frames compiled procedures close over.
 *
 * @param {Object} env - The environment the compiled procedure closes over.
 * @param {string} name - The global's name.
 * @returns {{v: *}} The cell to read it through.
 * @throws {SchemeUnboundError} If the name is bound nowhere, in Scheme or as a
 *   JavaScript global; the site then stays unresolved, so a later definition
 *   is picked up.
 */
export function globalCell(env, name) {
  const holder = env.findEnv(name);
  if (holder !== null) return holder.cellFor(name);
  // A JavaScript global, or nothing (in which case this throws). A JavaScript
  // global has no frame to keep a cell current, so it is read afresh each
  // time, and a Scheme definition of the name made later is found then.
  env.lookup(name);
  return {
    get v() {
      const found = env.findEnv(name);
      return found === null ? env.lookup(name) : found.bindings.get(name);
    }
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
