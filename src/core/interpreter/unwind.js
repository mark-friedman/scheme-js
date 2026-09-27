/**
 * @fileoverview Capturing a continuation across compiled code.
 *
 * ## The problem this solves
 *
 * A continuation in this interpreter is its frame stack. A *compiled* procedure
 * does not appear there: it runs in a JavaScript stack frame, and JavaScript
 * offers no way to read one. So when compiled code calls interpreted code and
 * the interpreted code captures a continuation, everything the compiled caller
 * had left to do would be missing from it -- the program then resumes into a
 * continuation with a hole in it and returns a plausible wrong answer.
 *
 * ## How it is solved
 *
 * The compiled frames put themselves into the continuation, cooperatively, by
 * unwinding:
 *
 * 1. `call/cc` notices that compiled code called the run it is in. Instead of
 *    building a continuation from a stack it knows to be incomplete, it
 *    records what it needs to finish the job later, with the frames of its
 *    own run, and abandons that run by throwing `CaptureUnwind`.
 * 2. That run returns the `UNWIND` sentinel to whoever called it -- compiled
 *    code.
 * 3. Generated code checks for `UNWIND` after every non-tail call. On seeing
 *    it, a procedure spills its locals and resume point with `reify` and
 *    returns `UNWIND` itself, so its own caller does the same.
 * 4. The outermost compiled procedure returns `UNWIND` to the interpreter. If
 *    that run was itself called by compiled code, it adds its own frames to
 *    the capture and returns `UNWIND` in turn (`Interpreter.unwindsOut`), so
 *    the unwind carries on through as many alternations of compiled and
 *    interpreted code as there are. The first run that cannot pass it on puts
 *    the frames on its own stack, in order, and finishes the capture.
 *
 * Signalling with a returned value rather than a thrown one is deliberate: on a
 * high-level virtual machine a throw costs orders of magnitude more than a
 * return, and every non-tail call in compiled code pays for this check. A throw
 * is used only in step 1, which happens once per capture.
 *
 * Reinstating such a continuation runs the frames through `CompiledFrame` in
 * `frames.js`, which calls the procedure's resumable form. Frames are *copied*
 * on the way in, so invoking the same continuation twice does not let the
 * second invocation see state left by the first -- which is what makes a
 * continuation multi-shot rather than one-shot.
 *
 * ## What this does not cover
 *
 * JavaScript that is not compiled code -- a primitive calling a procedure back,
 * host code calling a callback -- cannot save itself, so an unwind stops at the
 * run such a caller started. A continuation captured above one leaves out the
 * JavaScript caller and whatever is beneath it that the interpreter did not
 * run, exactly as it does with no compiled code anywhere: it works as an
 * escape, and resumed after those frames have returned it resumes without
 * them. A capture beneath a redefined inlined primitive is refused: an inline
 * expansion is not a call site the resumable form splits at, so there is no
 * point to resume from.
 */

import { CTL, ENV, FSTACK } from './stepables_base.js';

/**
 * Returned by compiled code whose callee began capturing a continuation.
 *
 * A dedicated symbol, so that no Scheme value can be mistaken for it.
 */
export const UNWIND = Symbol('scheme.unwind');

/**
 * Thrown by `call/cc` to abandon a nested run that sits above compiled frames.
 *
 * Not an error: a control-flow signal, in the same family as the sentinel that
 * ends a nested run normally. It carries nothing, because everything the
 * capture needs is in `unwinding`.
 */
export class CaptureUnwind {
  constructor() {
    this.name = 'CaptureUnwind';
  }
}

/**
 * State of a capture that is unwinding the JavaScript stack.
 *
 * Module-level rather than passed along, because it has to travel through
 * generated code that knows nothing about it. A compiled procedure says "I
 * suspended" by returning `UNWIND` and leaves its frame here on the way out.
 *
 * @property {Array<Object>} frames - What the unwind has collected, innermost
 *   first: suspended compiled frames, `{twin, pc, slots}`, and the frames of
 *   each run of the interpreter it passed through, `{segment}`.
 * @property {Object|null} pending - What `call/cc` needs to finish the capture.
 */
export const unwinding = { frames: [], pending: null };

/**
 * Records a suspended compiled frame. Called by generated code.
 *
 * @param {Function} twin - The procedure's resumable form.
 * @param {number} pc - The block to continue at.
 * @param {Object} slots - Spilled local variables.
 * @returns {void}
 */
export function reify(twin, pc, slots) {
  unwinding.frames.push({ twin, pc, slots });
}

/**
 * Begins a capture that has to cross compiled frames.
 *
 * @param {Object} pending - `{ lambdaExpr, env, segment }`: the receiver still
 *   to be applied, the environment to apply it in, and the frames of the run
 *   `call/cc` was in, above the sentinel it started on -- the innermost part
 *   of the continuation.
 * @returns {void}
 */
export function beginCapture(pending) {
  unwinding.frames = [];
  unwinding.pending = pending;
}

/**
 * How much more stack the compiled frames above the nearest interpreter frame
 * may take, and whether they may move to the heap when it runs out.
 *
 * Compiled frames live on the JavaScript stack, which holds a few thousand of
 * them; the interpreter's live on the heap and can go as deep as memory
 * allows. So a compiled procedure entered with no room left does not run: it
 * asks, with `beginFlush`, to be called again from the interpreter, and
 * unwinds. Every compiled frame beneath it saves itself on the way out, as for
 * a capture, and `completeCapture` puts them on the interpreter's frame stack
 * and makes the call, with the JavaScript stack empty again.
 *
 * `room` is in slots, a slot being one local of a frame. Compiled code stores
 * what room it leaves before each call and takes its own frame from what it
 * finds on entry (see `depth-entry` in `src/compiler/emit.scm`). Room rather
 * than depth, so that on entry, and at a tail call, it is compared with zero
 * rather than with a second field: reading one on entry to every procedure
 * that calls cost `tak` 3%. A tail call made directly needs room as well,
 * since the unwind cannot move its caller's frame: without it the call goes to
 * a trampoline instead.
 *
 * `limit`, the room at the bottom of a segment, is half of V8's default stack,
 * about 984 KB in Node and Chrome, in 8-byte slots; the size compiled code
 * gives its frames overestimates them, so the stack those frames really hold
 * is less. It is no lower because moving frames is not free: each frame moved
 * finishes in its procedure's resumable form, which is slower, and at a
 * quarter of the stack `earley`, which never came near overflowing, moved its
 * outer loop there and ran a fifth slower.
 *
 * `flushable` is true only while the unwind can reach an interpreter frame that
 * will finish it: in compiled code the interpreter called. Anywhere else,
 * compiled code runs out of room without moving its frames, so it never does
 * so beneath a JavaScript caller -- a primitive calling a procedure back, host
 * code calling a callback, or code outside any run of the interpreter -- which
 * would take the unwind sentinel for a value. The interpreter opens a segment
 * with `openCompiledSegment` whenever it calls compiled code, and every
 * JavaScript that calls a Scheme procedure closes one with `suspendFlush`; both
 * give back what they replaced with `restoreFlush`.
 *
 * A run of the interpreter that compiled code called, and that passes unwinds
 * on (`Interpreter.unwindsOut`), is not the bottom of a segment: it sits on the
 * JavaScript stack above the compiled code that called it, so the compiled code
 * it calls in turn continues that segment's room, less what the run itself
 * takes (`NESTED_RUN_ROOM`), and a move started there carries on through it to
 * the run that finishes it. That is what lets recursion alternating between
 * compiled and interpreted code go as deep as either alone.
 *
 * One way remains to reach compiled code beneath a JavaScript caller while it
 * is flushable: compiled code calling a plain JavaScript function directly,
 * which calls a compiled procedure back before it returns.
 *
 * `refusesCapture` is true while an inline expansion calls what its primitive's
 * name has been redefined to (`R.callBinding`). Frames may not move there
 * either, but unlike a JavaScript caller, which a continuation simply leaves
 * out as it does with no compiled code anywhere, the expansion's frame is
 * compiled code with no point to resume from, so a capture beneath it is
 * refused.
 *
 * @type {{room: number, limit: number, flushable: boolean, refusesCapture: boolean}}
 */
export const compiledStack = { room: 65536, limit: 65536, flushable: false, refusesCapture: false };

/**
 * Why a capture beneath a redefined primitive is refused, for `call/cc` in
 * interpreted code and for compiled code's own check alike.
 * @type {string}
 */
export const CAPTURE_UNDER_PRIMITIVE =
  'call/cc: a continuation was captured beneath a redefined primitive, which '
  + 'cannot be resumed. Run this program with its code interpreted '
  + '(--no-compile at the command line, setUserCodeCompilation(false) in a page).';

/**
 * The state `restoreFlush` gives back: `flushable` and `refusesCapture`
 * together, as the bits of one number.
 * @returns {number} The state.
 */
export function flushState() {
  return (compiledStack.flushable ? 1 : 0) | (compiledStack.refusesCapture ? 2 : 0);
}

/**
 * The room a nested run of the interpreter takes from the segment it
 * continues: the JavaScript frames between compiled code and the compiled code
 * the run calls -- the procedure's raw entry, `runWithSentinel`, `run`, `step`,
 * the application's step, `continueApplication`. Measured on V8, recursion
 * alternating between a small compiled procedure and an interpreted one used
 * about 214 slots of real stack a level, nearly all of it the run; this is
 * rounded up, so that a move comes before the stack runs out.
 * @type {number}
 */
export const NESTED_RUN_ROOM = 256;

/**
 * Starts a segment of compiled frames above the interpreter, or continues one
 * from a nested run that passes unwinds on.
 * @param {boolean} [continues=false] - Whether the run calling compiled code
 *   passes unwinds on, and so continues the segment of the compiled code that
 *   called it rather than starting one of its own.
 * @returns {number} The state to restore afterwards.
 */
export function openCompiledSegment(continues = false) {
  const saved = flushState();
  if (continues) {
    // Still flushable: the run passes unwinds on only when its caller could.
    compiledStack.room -= NESTED_RUN_ROOM;
  } else {
    compiledStack.room = compiledStack.limit;
    compiledStack.flushable = true;
    compiledStack.refusesCapture = false;
  }
  return saved;
}

/**
 * Stops compiled frames moving to the heap while JavaScript that is not the
 * interpreter calls a Scheme procedure, since it would receive the unwind.
 * @returns {number} The state to restore afterwards.
 */
export function suspendFlush() {
  const saved = flushState();
  compiledStack.flushable = false;
  compiledStack.refusesCapture = false;
  return saved;
}

/**
 * Stops compiled frames moving to the heap, and refuses a capture, while an
 * inline expansion calls what its primitive's name was redefined to.
 * @returns {number} The state to restore afterwards.
 */
export function suspendForPrimitive() {
  const saved = flushState();
  compiledStack.flushable = false;
  compiledStack.refusesCapture = true;
  return saved;
}

/**
 * Gives back what `openCompiledSegment`, `suspendFlush` or
 * `suspendForPrimitive` replaced.
 * @param {number} saved - What it returned.
 * @returns {void}
 */
export function restoreFlush(saved) {
  compiledStack.flushable = (saved & 1) !== 0;
  compiledStack.refusesCapture = (saved & 2) !== 0;
}

/**
 * Begins moving the compiled frames on the JavaScript stack to the heap, from
 * a compiled procedure entered too deep to run.
 *
 * @param {Function} procedure - The procedure, to be called again from the
 *   interpreter once the frames beneath it are on its heap stack.
 * @param {Array<*>} args - What it was called with.
 * @returns {void}
 */
export function beginFlush(procedure, args) {
  unwinding.frames = [];
  unwinding.pending = { call: procedure, args };
}

/**
 * Begins a capture made *by* compiled code rather than beneath it.
 *
 * `call/cc` reached from a compiled procedure has no interpreter frame stack to
 * snapshot and no boundary marker to splice at: the capture starts in compiled
 * code, and the interpreter frames that make up the rest of the continuation
 * are simply whatever is live when the unwind reaches the interpreter. So
 * nothing is recorded about them here, and `completeCapture` reads them from
 * the registers instead.
 *
 * @param {*} receiver - The procedure to apply to the continuation.
 * @returns {void}
 */
export function beginCompiledCapture(receiver) {
  unwinding.frames = [];
  unwinding.pending = { lambdaExpr: receiver, env: null, segment: [] };
}

/**
 * What finishing a capture needs from the interpreter.
 *
 * This module owns the protocol but not the interpreter's own vocabulary --
 * what a frame is, what a continuation is, how a procedure is applied to one --
 * so it asks for those rather than importing them. That is what keeps the
 * dependency pointing one way: the interpreter knows about compiled code only
 * through this file, and this file knows nothing about the compiler at all.
 *
 * @typedef {Object} CaptureHooks
 * @property {function(Function, number, Object): Object} frameFor - Builds a
 *   frame that resumes a suspended compiled procedure.
 * @property {function(Array, Object): Function} makeContinuation - Builds a
 *   continuation from a frame stack.
 * @property {function(Object, *): Object} applyReceiver - Builds the expression
 *   that applies the receiver to the continuation.
 * @property {function(Function, Array): Object} applyCall - Builds the
 *   expression that makes a pending call, for frames moved to the heap.
 * @property {function(Array, Array): void} pushMoved - Puts frames moved to the
 *   heap, outermost first, on a frame stack.
 * @property {function(Array): Array} segmentOf - The frames of the run a frame
 *   stack belongs to: those above the sentinel it started on, in order.
 */

/**
 * Finishes an unwind that has reached the interpreter, or passes it on.
 *
 * A run that passes unwinds on (`Interpreter.unwindsOut`) adds its own frames
 * and abandons itself, returning the unwind sentinel to the compiled code that
 * called it. Any other run finishes the unwind: what it collected goes on the
 * run's own frame stack, outermost first -- compiled frames, then the frames of
 * the run they called, then the compiled frames that run called, and so on
 * inwards. A capture then applies its receiver to a continuation of that
 * stack, with the frames of the run `call/cc` was in innermost; a move to the
 * heap makes the call that was too deep to make.
 *
 * @param {Array} registers - The interpreter registers.
 * @param {Object} interpreter - The interpreter.
 * @param {CaptureHooks} hooks - What this needs from the interpreter.
 * @returns {boolean} True, to continue the trampoline.
 * @throws {CaptureUnwind} In a run that passes the unwind on.
 */
export function completeCapture(registers, interpreter, hooks) {
  if (unwinding.pending === null) {
    throw new Error(
      'compiled code reported a continuation capture, but none was in progress');
  }
  if (interpreter.unwindsOut) {
    unwinding.frames.push({ segment: hooks.segmentOf(registers[FSTACK]) });
    throw new CaptureUnwind();
  }
  const { lambdaExpr, env, segment, call, args } = unwinding.pending;

  // `unwinding.frames` is innermost first, because the innermost procedure
  // reifies first as the unwind travels outward. A frame stack is innermost
  // *last*, since the interpreter pops from the end.
  const collected = [];
  for (let i = unwinding.frames.length - 1; i >= 0; i--) {
    const piece = unwinding.frames[i];
    if (piece.segment !== undefined) collected.push(...piece.segment);
    else collected.push(hooks.frameFor(piece.twin, piece.pc, piece.slots));
  }

  unwinding.frames = [];
  unwinding.pending = null;

  // Frames moved to the heap because the stack was deep go on the stack as
  // they are, as the interpreter pushes any frame, since no continuation is
  // taken -- and then the call that was too deep to make is made.
  if (call !== undefined) {
    hooks.pushMoved(registers[FSTACK], collected);
    registers[CTL] = hooks.applyCall(call, args);
    return true;
  }

  const stack = [...registers[FSTACK], ...collected, ...segment];
  registers[FSTACK] = stack;
  if (env !== null) registers[ENV] = env;
  registers[CTL] =
    hooks.applyReceiver(lambdaExpr, hooks.makeContinuation(stack, interpreter));
  return true;
}
