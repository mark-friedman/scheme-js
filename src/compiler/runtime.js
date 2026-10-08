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

import {
  TailCall, Values, SCHEME_PRIMITIVE, SCHEME_RAW_CALL, callWithSchemeValues, callForeign, createCompiledProcedure,
  createNativeContinuation, callSchemeProcedure
} from '../core/interpreter/values.js';
import { schemeToJsDeep } from '../core/interpreter/js_interop.js';
// The capture protocol belongs to the interpreter, which owns what a
// continuation is; this module only makes it reachable from generated code.
import {
  UNWIND, reify, beginCompiledCapture, beginFlush, compiledStack, suspendForPrimitive, restoreFlush,
  CAPTURE_UNDER_PRIMITIVE, runAhead as runAheadWith
} from '../core/interpreter/unwind.js';
import { SchemeError, SchemeApplicationError, SchemeArityError } from '../core/interpreter/errors.js';
import { Cons } from '../core/interpreter/cons.js';
// Kept by the interpreter, which sees every binding write; generated code only
// reads a cell, once per inlined primitive.
import { primitiveCell } from '../core/interpreter/primitive_bindings.js';
import {
  inexactReal, heldDouble, addNumbers, subNumbers, mulNumbers, addReals, subReals, mulReals,
  lessReals, lessEqualReals, equalReals
} from '../core/interpreter/number_representation.js';
export { Flonum, inexactReal } from '../core/interpreter/number_representation.js';
// What compiled code makes `call-with-values` of (`lower-call-with-values` in
// ir.scm): the primitives themselves, whatever the environment the code runs
// in binds under their names.
export { applyProcedure, valuesToList } from '../core/primitives/apply.js';

export { TailCall, Cons, SCHEME_RAW_CALL, SCHEME_PRIMITIVE, UNWIND, reify, SchemeError, primitiveCell, callForeign };

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
  throw new SchemeError(CAPTURE_UNDER_PRIMITIVE);
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
 * The binding may have been redefined to something that is not a procedure at
 * all, which is reported as the interpreter reports it.
 *
 * @param {Function} fn - The name's current binding.
 * @param {Array<*>} args - Scheme values.
 * @returns {*} The call's value.
 * @throws {SchemeError} If a continuation was captured beneath it.
 * @throws {SchemeApplicationError} If the binding is not a procedure.
 */
export function callBinding(fn, args) {
  if (typeof fn !== 'function') notAProcedure(fn);
  // No resume point, so compiled code beneath may not move its frames either,
  // and a continuation captured beneath is refused.
  const saved = suspendForPrimitive();
  let value;
  try {
    value = settle(invoke(fn, args));
  } finally {
    restoreFlush(saved);
  }
  if (value === UNWIND) captureUnderPrimitive();
  return value;
}

/**
 * `vector-ref` for an inline expansion, whose guard has established that the
 * name is still bound to the primitive.
 *
 * The common case -- an array and an index inside it, an exact integer, which
 * is a JavaScript number (src/core/interpreter/number_representation.js) --
 * indexes the array; calling the primitive through the generic call path cost
 * vector-heavy programs up to half their time, measured as a ceiling. Every
 * other case, and so every error, is the primitive's own: an index that is not
 * an integer is not inside the array.
 *
 * @param {*} vector - The vector operand.
 * @param {*} index - The index operand.
 * @returns {*} The element.
 */
export function vectorRef(vector, index) {
  if (Array.isArray(vector) && Number.isInteger(index) && index >= 0 && index < vector.length) {
    return vector[index];
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
  if (Array.isArray(vector) && Number.isInteger(index) && index >= 0 && index < vector.length) {
    vector[index] = value;
    return null;
  }
  return primitiveCell('vector-set!').primitive(vector, index, value);
}

// ============================================================================
// Inline arithmetic and comparison
// ============================================================================

// Each is a binary arithmetic or comparison primitive for an inline
// expansion, whose guard has established that the name is still bound to the
// primitive, called where the inline code does not apply: two JavaScript
// numbers, or any operand held as a number or a Flonum -- an inexact integer,
// boxed (src/core/interpreter/number_representation.js) -- taken directly, the
// result of arithmetic with a box inexact and boxed if it is an integer. Each
// is small, so that V8 inlines it where it is called, and hands anything else
// -- a BigInt, a rational, a complex, a wrong type -- to `otherwise`, which is
// not inlined. Each is written out, rather than made by one function from the
// operation, so that each call of an operation is a call site of its own.

/**
 * @typedef {Object} SlowPath
 * @property {string} name - The primitive's name.
 * @property {function(*, *): *} op - The operation on two reals held as
 *   numbers, BigInts or Flonums, `undefined` where it does not apply.
 * @property {Function|null} primitive - The primitive, once found.
 */

/**
 * An operation's way for `otherwise`. The primitive is found by its name the
 * first time it is needed, and kept: found on every call, it cost
 * arithmetic on complex numbers (benchmarks/r7rs/src/mbrotZ.scm) an eighth
 * of its time.
 * @param {string} name - The primitive's name.
 * @param {function(*, *): *} op - The operation on two reals.
 * @returns {SlowPath}
 */
function slowPath(name, op) {
  return { name, op, primitive: null };
}

const ADD = slowPath('+', addReals);
const SUB = slowPath('-', subReals);
const MUL = slowPath('*', mulReals);
const LT = slowPath('<', lessReals);
const GT = slowPath('>', (a, b) => lessReals(b, a));
const LE = slowPath('<=', lessEqualReals);
const GE = slowPath('>=', (a, b) => lessEqualReals(b, a));
const NUM_EQ = slowPath('=', equalReals);

/**
 * An operation on two operands that are not both held as numbers or Flonums:
 * a real held as a BigInt (`addReals` and the rest), and otherwise the
 * primitive, so that every error is its own.
 * @param {SlowPath} slow - The operation.
 * @param {*} a - One operand.
 * @param {*} b - The other.
 * @returns {*}
 */
function otherwise(slow, a, b) {
  const r = slow.op(a, b);
  if (r !== undefined) return r;
  return (slow.primitive ?? (slow.primitive = primitiveCell(slow.name).primitive))(a, b);
}

/**
 * `+` of two operands.
 * @param {*} a - One.
 * @param {*} b - The other.
 * @returns {*}
 */
export function add(a, b) {
  if (typeof a === 'number' && typeof b === 'number') return addNumbers(a, b);
  const x = heldDouble(a), y = heldDouble(b);
  if (x !== undefined && y !== undefined) return inexactReal(x + y);
  return otherwise(ADD, a, b);
}

/**
 * `-` of two operands.
 * @param {*} a - The minuend.
 * @param {*} b - The subtrahend.
 * @returns {*}
 */
export function sub(a, b) {
  if (typeof a === 'number' && typeof b === 'number') return subNumbers(a, b);
  const x = heldDouble(a), y = heldDouble(b);
  if (x !== undefined && y !== undefined) return inexactReal(x - y);
  return otherwise(SUB, a, b);
}

/**
 * `*` of two operands.
 * @param {*} a - One.
 * @param {*} b - The other.
 * @returns {*}
 */
export function mul(a, b) {
  if (typeof a === 'number' && typeof b === 'number') return mulNumbers(a, b);
  const x = heldDouble(a), y = heldDouble(b);
  if (x !== undefined && y !== undefined) return inexactReal(x * y);
  return otherwise(MUL, a, b);
}

/**
 * `<` of two operands.
 * @param {*} a - One.
 * @param {*} b - The other.
 * @returns {*}
 */
export function lt(a, b) {
  const x = heldDouble(a), y = heldDouble(b);
  if (x !== undefined && y !== undefined) return x < y;
  return otherwise(LT, a, b);
}

/**
 * `>` of two operands.
 * @param {*} a - One.
 * @param {*} b - The other.
 * @returns {*}
 */
export function gt(a, b) {
  const x = heldDouble(a), y = heldDouble(b);
  if (x !== undefined && y !== undefined) return x > y;
  return otherwise(GT, a, b);
}

/**
 * `<=` of two operands.
 * @param {*} a - One.
 * @param {*} b - The other.
 * @returns {*}
 */
export function le(a, b) {
  const x = heldDouble(a), y = heldDouble(b);
  if (x !== undefined && y !== undefined) return x <= y;
  return otherwise(LE, a, b);
}

/**
 * `>=` of two operands.
 * @param {*} a - One.
 * @param {*} b - The other.
 * @returns {*}
 */
export function ge(a, b) {
  const x = heldDouble(a), y = heldDouble(b);
  if (x !== undefined && y !== undefined) return x >= y;
  return otherwise(GE, a, b);
}

/**
 * `=` of two operands.
 * @param {*} a - One.
 * @param {*} b - The other.
 * @returns {*}
 */
export function numEq(a, b) {
  const x = heldDouble(a), y = heldDouble(b);
  if (x !== undefined && y !== undefined) return x === y;
  return otherwise(NUM_EQ, a, b);
}

/**
 * A tail call a call site does not make directly (see `stack`), returned
 * to the trampoline.
 *
 * A callee that is not a function is not a procedure, and is reported here as
 * the interpreter reports it. Returned as a `TailCall`, it would reach a
 * trampoline that takes a `TailCall` of something other than a function for an
 * expression to evaluate, and fail with a message about the interpreter's
 * internals -- "ctl.step is not a function".
 *
 * @param {*} callee - What is called.
 * @param {Array<*>} args - Scheme values.
 * @returns {TailCall} The pending call.
 * @throws {SchemeApplicationError} If the callee is not a procedure.
 */
export function tailCall(callee, args) {
  if (typeof callee !== 'function') notAProcedure(callee);
  return new TailCall(callee, args);
}

/**
 * Reports a call to a value that is not a procedure, as the interpreter does.
 *
 * Where generated code wants a call's value it tests the callee first,
 * because calling what is not a function makes JavaScript report the
 * temporary that held it -- "$t0 is not a function" -- and calling the empty
 * list, which is `null`, fails before that, reading its raw entry.
 *
 * @param {*} callee - What was called.
 * @returns {never}
 * @throws {SchemeApplicationError} Always.
 */
export function notAProcedure(callee) {
  throw new SchemeApplicationError(callee);
}

/**
 * Reports a compiled procedure called with the wrong number of arguments, as
 * the interpreter reports an interpreted one. Generated code tests the count
 * on entry to a procedure's fast form and calls this when it is wrong.
 *
 * @param {string} name - The procedure's name.
 * @param {number} required - How many parameters it has, besides a rest one.
 * @param {boolean} rest - Whether it has a rest parameter.
 * @param {number} given - How many arguments it was called with.
 * @returns {never}
 * @throws {SchemeArityError} Always.
 */
export function wrongArity(name, required, rest, given) {
  throw new SchemeArityError(name, required, rest ? Infinity : required, given);
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
 * A primitive is a plain function that takes Scheme values, so it is called
 * directly. An interpreted closure or a compiled procedure is also a plain
 * function, but calling it that way means entering Scheme from JavaScript, and
 * it converts accordingly -- exact integers become doubles, bignums beyond 2^53
 * throw. Compiled code is not a JavaScript caller, so it uses the procedure's
 * raw entry instead: for a compiled procedure, its code.
 *
 * A function with no raw entry is a primitive, or JavaScript's own, which gets
 * its arguments as JavaScript values, as the interpreter gives them
 * (`callForeign` in `src/core/interpreter/values.js`).
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
  return callWithSchemeValues(fn, args);
}

/**
 * How much more stack compiled frames above the nearest interpreter frame may
 * take, in slots, and whether they may move to the heap when it runs out:
 * `compiledStack` in `src/core/interpreter/unwind.js`, which owns it because
 * the interpreter opens and closes the segments it measures. Generated code
 * takes its frame from `room` on entry, stores what is left before each call,
 * makes a tail call directly only while room is left, and with none, where
 * `flushable` allows, moves its frames to the heap with `flush`.
 *
 * @type {{room: number, limit: number, flushable: boolean}}
 */
export const stack = compiledStack;

/**
 * Moves the compiled frames on the JavaScript stack to the interpreter's heap
 * stack, from a procedure entered too deep to run. Every compiled frame
 * beneath it saves itself on seeing the unwind sentinel, as it would for a
 * continuation capture, and the interpreter then calls the procedure again
 * with the JavaScript stack empty.
 *
 * @param {Function} procedure - The procedure that was entered.
 * @param {Array<*>} args - Its arguments.
 * @returns {symbol} `UNWIND`, always.
 */
export function flush(procedure, args) {
  beginFlush(procedure, args);
  return UNWIND;
}


/**
 * Performs one step of a pending tail call.
 *
 * Compiled procedures signal a tail call they do not make directly (see
 * `stack`) by returning the interpreter's own `TailCall`, which is why an
 * interpreted procedure and a compiled one are interchangeable at a call site
 * in either direction.
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
 * Makes a compiled procedure of its generated code.
 *
 * The code is the procedure's raw entry, which takes and returns Scheme values
 * and is what compiled code calls; the procedure itself, which this returns
 * and Scheme holds, faces JavaScript as an interpreted closure does
 * (`createCompiledProcedure` in `src/core/interpreter/values.js`). The raw
 * entry is marked `SCHEME_PRIMITIVE`, a function taking Scheme values, which
 * is what a direct tail call tests for (`tail` in emit.scm).
 *
 * Whether the procedure has a rest parameter is recorded, since a call from
 * JavaScript is fitted to its parameters, and the code's `length` counts only
 * the others.
 *
 * @param {Function} code - The generated function.
 * @param {string} name - The Scheme procedure's name, for stack traces.
 * @param {Object} env - The environment the procedure closes over.
 * @param {boolean} [rest=false] - Whether it has a rest parameter.
 * @returns {Function} The procedure.
 */
export function markProcedure(code, name, env, rest = false) {
  code[SCHEME_PRIMITIVE] = true;
  const procedure = createCompiledProcedure(code, env);
  procedure.$compiled = true;
  procedure.$rest = rest;
  procedure.schemeName = name;
  // One function for every procedure, rather than one made for each: a
  // closure made in a loop makes a procedure each time round.
  procedure.toString = compiledProcedureText;
  return procedure;
}

/**
 * How a compiled procedure shows itself to JavaScript, as its `toString`.
 * @this {Function} The procedure.
 * @returns {string} Its text.
 */
function compiledProcedureText() {
  const name = this.schemeName;
  return `#<compiled-procedure${name && name !== 'anonymous' ? ' ' + name : ''}>`;
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

// =============================================================================
// Running with no interpreter
// =============================================================================

/**
 * What a driver with no interpreter beneath it needs (`runAhead` in
 * unwind.js): how compiled code calls a procedure, how it tells a pending
 * tail call, and the continuations it makes.
 * @type {Object}
 */
const AHEAD_HOOKS = {
  call: callWithSchemeValues,
  callScheme: callSchemeProcedure,
  isTailCall: (x) => x instanceof TailCall,
  nativeContinuation: createNativeContinuation
};

/**
 * Calls a procedure with Scheme values and runs the call to its end with no
 * interpreter at all, as a program compiled ahead of time runs: the
 * continuations it captures and the frames it moves to the heap finished by
 * a driver of the runtime's own.
 * @param {Function} procedure - The procedure: compiled, or a primitive.
 * @param {Array<*>} args - Its arguments, Scheme values.
 * @returns {*} Its value.
 */
export function runAhead(procedure, args) {
  return runAheadWith(procedure, args, AHEAD_HOOKS);
}

/**
 * What stands for the interpreter of an environment a program compiled ahead
 * of time runs in, which has none: JavaScript calling one of its procedures
 * (`createCompiledProcedure` in values.js) runs the call with `runAhead`, and
 * gets its value as it would from the interpreter, the first of several and
 * converted for JavaScript.
 * @type {{callCompiledEntry: function(Function, Array<*>, *): *}}
 */
export const aheadRunner = {
  callCompiledEntry(raw, args) {
    // The raw entry as a procedure whose raw entry it is, for `callWithSchemeValues`.
    let result = runAhead({ [SCHEME_RAW_CALL]: raw }, args);
    if (result instanceof Values) result = result.first();
    return schemeToJsDeep(result);
  }
};
