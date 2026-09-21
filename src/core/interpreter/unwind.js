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
 * 1. `call/cc` notices a compiled boundary beneath it. Instead of building a
 *    continuation from a stack it knows to be incomplete, it records what it
 *    needs to finish the job later and abandons the nested run by throwing
 *    `CaptureUnwind`.
 * 2. That run returns the `UNWIND` sentinel to whoever called it -- compiled
 *    code.
 * 3. Generated code checks for `UNWIND` after every non-tail call. On seeing
 *    it, a procedure spills its locals and resume point with `reify` and
 *    returns `UNWIND` itself, so its own caller does the same.
 * 4. The outermost compiled procedure returns `UNWIND` to the interpreter,
 *    which splices the reified frames into the stack where the boundary was
 *    and finishes the capture.
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
 * A capture crossing more than one boundary between compiled and interpreted
 * code. Each boundary would need its own group of frames spliced at its own
 * position, and getting that wrong gives a wrong answer rather than a failure,
 * so `call/cc` refuses outright instead. A capture beneath a redefined inlined
 * primitive is refused for the same reason: an inline expansion is not a call
 * site the resumable form splits at, so there is no point to resume from.
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
 * @property {Array<Object>} frames - Suspended compiled frames, innermost first.
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
 * @param {Object} pending - `{ lambdaExpr, fstack, env, boundary }`: the
 *   receiver still to be applied, the frame stack as `call/cc` saw it, the
 *   environment to apply the receiver in, and the index in that stack of the
 *   boundary marker the compiled frames belong at.
 * @returns {void}
 */
export function beginCapture(pending) {
  unwinding.frames = [];
  unwinding.pending = pending;
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
  unwinding.pending = { lambdaExpr: receiver, fstack: null, env: null, boundary: -1 };
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
 */

/**
 * Finishes a capture whose unwind has reached the interpreter.
 *
 * The reified frames are spliced in where the boundary marker sat, so that the
 * continuation reads, from outermost to innermost: the interpreter frames
 * outside all compiled code, the compiled frames, then the interpreter frames
 * the capture was made in.
 *
 * @param {Array} registers - The interpreter registers.
 * @param {Object} interpreter - The interpreter.
 * @param {CaptureHooks} hooks - What this needs from the interpreter.
 * @returns {boolean} True, to continue the trampoline.
 */
export function completeCapture(registers, interpreter, hooks) {
  if (unwinding.pending === null) {
    throw new Error(
      'compiled code reported a continuation capture, but none was in progress');
  }
  const { lambdaExpr, fstack, env, boundary } = unwinding.pending;

  // `unwinding.frames` is innermost first, because the innermost procedure
  // reifies first as the unwind travels outward. A frame stack is innermost
  // *last*, since the interpreter pops from the end.
  const compiled = unwinding.frames
    .map((f) => hooks.frameFor(f.twin, f.pc, f.slots))
    .reverse();

  // A capture made *by* compiled code records no stack and no boundary, because
  // there were none to record: the frames that make up the rest of the
  // continuation are the ones live right now, and the compiled frames belong
  // directly inside them -- which is the end of the stack, since the
  // interpreter pops from there.
  const spliced = boundary < 0
    ? [...registers[FSTACK], ...compiled]
    : [...fstack.slice(0, boundary), ...compiled, ...fstack.slice(boundary + 1)];

  unwinding.frames = [];
  unwinding.pending = null;

  registers[FSTACK] = spliced;
  if (env !== null) registers[ENV] = env;
  registers[CTL] =
    hooks.applyReceiver(lambdaExpr, hooks.makeContinuation(spliced, interpreter));
  return true;
}
