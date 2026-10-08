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

import { ANS, CTL, ENV, FSTACK } from './stepables_base.js';

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
  const counts = frameCounts.get(twin);
  if (counts === undefined) frameCounts.set(twin, { saved: 1, resumed: 0, ask: undefined });
  else counts.saved++;
}

// =============================================================================
// Procedures whose saved frames are re-entered
// =============================================================================
//
// A procedure whose frames continuations keep re-entering costs more compiled
// than interpreted, and is switched back to the interpreted closure it was
// compiled from. Which ones is the compiler's decision, made in Scheme
// (`note-resume` in `src/compiler/tier.scm`); what is here is what it decides
// from, kept where frames are saved and resumed: how many times each
// procedure's frames have been.

/**
 * Saves and resumes of each procedure's frames, by its resumable form, and the
 * resume at which to ask the policy next, once it has been asked.
 * @type {WeakMap<Function, {saved: number, resumed: number, ask: (number|undefined)}>}
 */
const frameCounts = new WeakMap();

/**
 * Asked, at the resumes it names, whether a procedure whose frame is being
 * resumed is re-entered, switching it back if it is: a Scheme procedure, the
 * compiler's (`note-resume` in `src/compiler/tier.scm`); null until the
 * compiler has started and registered it.
 * @type {Function|null}
 */
let reentryPolicy = null;

/**
 * The resume at which the policy is first asked about a procedure. A procedure
 * nested in another has a resumable form of its own for every closure made of
 * it, each resumed only a few times, so asking at the first resume would ask
 * about nearly every one.
 * @type {number}
 */
let firstAsk = Infinity;

/**
 * Registers what decides whether a procedure whose frames are being resumed
 * is re-entered, and switches it back.
 * @param {Function} policy - A Scheme procedure, given the procedure's
 *   resumable form and its frames' saves and resumes so far; answers #t if it
 *   judged the procedure re-entered, after which it is not asked about it
 *   again, and otherwise the resume at which to ask next.
 * @param {number} first - The resume at which to ask about a procedure first.
 */
export function setReentryPolicy(policy, first) {
  reentryPolicy = policy;
  firstAsk = first;
}

/**
 * Notes that a saved compiled frame is being resumed, and asks whether its
 * procedure is re-entered when the policy asked to be asked.
 * @param {Function} twin - The procedure's resumable form.
 * @param {function(Function, Array<*>): *} call - Calls the policy:
 *   `callSchemeProcedure`, as the evaluator calls the rest of the tier's
 *   Scheme. Passed in by the evaluator, since `values.js`, where it is, imports
 *   this module.
 */
export function noteResume(twin, call) {
  const counts = frameCounts.get(twin);
  if (counts === undefined) return;
  counts.resumed++;
  if (counts.resumed >= (counts.ask ?? firstAsk) && reentryPolicy !== null) {
    const next = call(reentryPolicy, [twin, counts.saved, counts.resumed]);
    counts.ask = next === true ? Infinity : Number(next);
  }
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
 * Begins moving the compiled frames beneath a run to the heap, so that a step
 * the run cannot take now is taken again by the run that finishes the move: a
 * pause at a breakpoint, which a run compiled code called -- beneath compiled
 * frames on the JavaScript stack -- could not wait at, as the run of the
 * asynchronous loop does. No continuation is taken; the frames are moved as
 * for a call too deep to make, then the run's own on top of them.
 *
 * @param {Array} fstack - The run's frame stack.
 * @param {Object} step - The step to take again.
 * @param {Object} env - Its environment.
 * @returns {boolean} Whether the run was called by compiled code, and so began
 *   the move: it then abandons itself by throwing `CaptureUnwind`. A run
 *   called beneath an inline expansion of a redefined primitive cannot move
 *   them, there being no point to resume the expansion from.
 */
export function beginStepAgain(fstack, step, env) {
  let start = fstack.length;
  while (start > 0 && fstack[start - 1].isSentinel !== true) start--;
  const sentinel = start > 0 ? fstack[start - 1] : null;
  if (sentinel === null || sentinel.compiledBoundary !== true || sentinel.refusesCapture === true) return false;
  unwinding.frames = [];
  unwinding.pending = { resume: step, env, segment: fstack.slice(start) };
  return true;
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
 * heap makes the call that was too deep to make, or takes again the step a
 * run could not take (`beginStepAgain`), with that run's frames on top.
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
  const { lambdaExpr, env, segment, call, args, resume } = unwinding.pending;

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
  if (resume !== undefined) {
    hooks.pushMoved(registers[FSTACK], collected);
    registers[FSTACK].push(...segment);
    registers[ENV] = env;
    registers[CTL] = resume;
    return true;
  }

  const stack = [...registers[FSTACK], ...collected, ...segment];
  registers[FSTACK] = stack;
  if (env !== null) registers[ENV] = env;
  registers[CTL] =
    hooks.applyReceiver(lambdaExpr, hooks.makeContinuation(stack, interpreter));
  return true;
}

// =============================================================================
// Finishing an unwind without the interpreter
// =============================================================================
//
// `completeCapture` puts the frames an unwind saved on the interpreter's
// frame stack, as interpreter frames, makes the continuation a copy of that
// stack, and resumes the frames one interpreter step at a time; a continuation
// invoked from compiled code goes through a run of the interpreter of its own
// and is thrown back to this one. For a capture made by compiled code, with
// only compiled frames between it and the run that finishes it, all of that
// is done here instead, by a driver: the saved frames
// kept as a list of their own, innermost first, which a continuation shares
// rather than copies; each resumed by calling its resumable form; and a
// continuation invoked from compiled code running in the driver taken by a
// throw the driver catches. That made compiled `ctak` 1.6 and `fibc` 2.4
// times faster.
//
// A continuation the driver makes is the interpreter's as well: the run's
// frame stack beneath the driver, copied at the driver's first capture, with
// the compiled frames on top, made only if something asks for it -- invoked
// from interpreted code, from inside a run of the interpreter, after the
// driver has returned. Those take the interpreter's way, which runs the
// `dynamic-wind` thunks a jump would skip, since a jump is taken only where no
// run lies between it and its driver: a run clears the driver it was called
// beneath (`enterRun`). Anything else an unwind collects -- the frames of a run
// it passed through, a capture made by interpreted code, a step to take again
// for the debugger -- is handed to `completeCapture` with the driver's frames
// beneath, as before; and so is everything while a debugger is on, whose
// stack is the interpreter's.

/**
 * How the unwinds that reached a run were finished: by the driver, and how
 * many continuations it took by a jump; or handed to the interpreter. For the
 * tests and measurements that need to know which way ran.
 * @type {{finished: number, jumps: number, handedDown: number}}
 */
export const nativeUnwinds = { finished: 0, jumps: 0, handedDown: 0 };

/**
 * The driver whose compiled code is running now with no run of the
 * interpreter between it and the driver, or null.
 * @type {Object|null}
 */
let currentDriver = null;

/**
 * Notes that a run of the interpreter starts: no driver beneath it is current
 * until it ends.
 * @returns {Object|null} What `leaveRun` gives back.
 */
export function enterRun() {
  const saved = currentDriver;
  currentDriver = null;
  return saved;
}

/**
 * Notes that a run of the interpreter ends.
 * @param {Object|null} saved - What `enterRun` returned.
 * @returns {void}
 */
export function leaveRun(saved) {
  currentDriver = saved;
}

/**
 * Thrown to take a continuation a driver made, back to that driver. A control
 * signal, not an Error, which would take a stack trace every time.
 */
class NativeJump {
  /**
   * @param {Object} driver - The driver.
   * @param {Object|null} stack - The continuation's frames, innermost first.
   * @param {*} value - What the continuation was invoked with.
   */
  constructor(driver, stack, value) {
    this.driver = driver;
    this.stack = stack;
    this.value = value;
  }
}

/**
 * Finishes an unwind that has reached a run of the interpreter: in a driver,
 * where it can be (above), and otherwise as `completeCapture` does.
 * @param {Array} registers - The run's registers.
 * @param {Object} interpreter - The interpreter.
 * @param {CaptureHooks & NativeHooks} hooks - What this needs from the interpreter.
 * @returns {boolean} Whether the run's trampoline continues.
 */
export function finishUnwind(registers, interpreter, hooks) {
  if (interpreter.unwindsOut || interpreter.debugRuntime?.enabled || !nativelyFinishable()) {
    return completeCapture(registers, interpreter, hooks);
  }
  return drive(registers, interpreter, hooks);
}

/**
 * Whether the unwind in progress can be finished in a driver: a capture made
 * by compiled code, every frame it saved compiled.
 *
 * Not a move to the heap, which the interpreter finishes (`MovedFrames` in
 * frames.js): after one, everything the frames moved go on to do -- in
 * `earley` the rest of the program -- would run inside the driver, and there
 * it took a fifth longer, the time all in garbage collection, as it did when
 * a moved frame was resumed from inside the frame holding it.
 * @returns {boolean}
 */
function nativelyFinishable() {
  const pending = unwinding.pending;
  if (pending === null || pending.lambdaExpr === undefined) return false;
  return pending.env === null && pending.segment.length === 0
    && unwinding.frames.every((piece) => piece.twin !== undefined);
}

/**
 * What a driver needs from the interpreter, beside `CaptureHooks`.
 * @typedef {Object} NativeHooks
 * @property {function(Function, Array): *} call - Calls a procedure with
 *   Scheme values, as compiled code does (`callWithSchemeValues`).
 * @property {function(Function, Array): *} callScheme - Calls a Scheme
 *   procedure from JavaScript (`callSchemeProcedure`), for the policy that
 *   switches re-entered procedures back.
 * @property {function(TailCall, Array): boolean} takeTailCall - Makes a tail
 *   call the run's next step.
 * @property {function(Object, Object, Object): Function} nativeContinuation -
 *   A continuation of a driver's frames (`createNativeContinuation`).
 * @property {function(*): boolean} isTailCall - Whether a value is a pending
 *   tail call.
 */

/**
 * Finishes the unwind in progress, and runs what follows it, in a driver:
 * until the frames the driver keeps are all resumed, or until an unwind
 * reaches it that it cannot finish, which is handed to `completeCapture` with
 * those frames beneath it.
 * @param {Array} registers - The run's registers.
 * @param {Object} interpreter - The interpreter.
 * @param {CaptureHooks & NativeHooks} hooks - What this needs.
 * @returns {boolean} Whether the run's trampoline continues.
 */
function drive(registers, interpreter, hooks) {
  // What a continuation holds of the driver: whether it is running, the run's
  // frame stack beneath it -- read until its first capture copies it -- and
  // the hooks. Nothing more, since a program may keep its continuations.
  const driver = { live: true, base: null, beneath: registers[FSTACK], hooks };
  // The frames the driver holds, innermost first, as `{frame, next}`.
  let stack = null;
  // What to do next: resume `frame` with `value`, or call `callee` -- a
  // capture's receiver -- with `args`, so that a step allocates nothing but
  // its frame's copy.
  let frame = null;
  let value;
  let callee = null;
  let args = null;
  let result = UNWIND;
  let handDown = false;
  const savedDriver = currentDriver;
  const flush = flushState();
  currentDriver = driver;
  compiledStack.flushable = true;
  compiledStack.refusesCapture = false;
  // What compiled code calls back into Scheme starts from the run's stack, as
  // where the interpreter calls compiled code.
  interpreter.pushJsContext(registers[FSTACK]);
  try {
    for (;;) {
      if (frame !== null || callee !== null) {
        compiledStack.room = compiledStack.limit;
        try {
          if (frame !== null) {
            const { twin, pc, slots } = frame;
            frame = null;
            noteResume(twin, hooks.callScheme);
            // Copied, never shared, so the continuation stays multi-shot.
            result = twin(pc, { ...slots, $r: value });
          } else {
            const f = callee;
            const a = args;
            callee = null;
            args = null;
            result = hooks.call(f, a);
          }
          value = undefined;
          // A tail call with frames still to return to is made here, and so
          // is one to a continuation of this driver's, which is a jump; any
          // other, with none, is the run's to make, as the call's own would be.
          while (hooks.isTailCall(result) && (stack !== null || result.func.driver === driver)) {
            result = hooks.call(result.func, result.args);
          }
        } catch (e) {
          if (!(e instanceof NativeJump) || e.driver !== driver) throw e;
          nativeUnwinds.jumps++;
          stack = e.stack;
          result = e.value;
        }
      }
      if (result === UNWIND) {
        if (!nativelyFinishable()) {
          for (let node = stack; node !== null; node = node.next) unwinding.frames.push(node.frame);
          handDown = true;
          break;
        }
        // Saved innermost first, as the unwind travelled outward.
        for (let i = unwinding.frames.length - 1; i >= 0; i--) stack = { frame: unwinding.frames[i], next: stack };
        callee = unwinding.pending.lambdaExpr;
        unwinding.frames = [];
        unwinding.pending = null;
        nativeUnwinds.finished++;
        args = [continuationOf(driver, stack, interpreter)];
        continue;
      }
      if (stack === null) break;
      frame = stack.frame;
      stack = stack.next;
      value = result;
      result = undefined;
    }
  } finally {
    interpreter.popJsContext();
    restoreFlush(flush);
    currentDriver = savedDriver;
    driver.live = false;
    driver.beneath = null;
  }
  if (handDown) {
    nativeUnwinds.handedDown++;
    return completeCapture(registers, interpreter, hooks);
  }
  if (hooks.isTailCall(result)) return hooks.takeTailCall(result, registers);
  registers[ANS] = result;
  return false;
}

// =============================================================================
// Running with no interpreter at all
// =============================================================================
//
// A program compiled ahead of time runs with no interpreter: nothing beneath
// its compiled code to finish an unwind, to make a tail call it hands back,
// or to invoke a continuation the interpreter's way. A driver with nothing
// beneath does all of it: a capture as `drive` does, a move to the heap the
// same way -- the frames on its list, the call made again from the driver --
// and every tail call. A continuation one makes is taken by a jump while its
// driver runs it; invoked otherwise -- after its driver has returned, from
// JavaScript, from a driver started since -- it re-enters through a driver of
// its own, which resumes its frames and returns what the last of them does.

/**
 * Runs a call to the end with no interpreter at all.
 * @param {Function} callee - The procedure.
 * @param {Array<*>} args - Its arguments, Scheme values.
 * @param {NativeHooks & {frameFor: Function}} hooks - What the driver needs.
 * @returns {*} Its value.
 */
export function runAhead(callee, args, hooks) {
  return ahead(hooks, null, undefined, callee, args);
}

/**
 * Invokes a continuation a driver with no interpreter made, where it cannot
 * be taken by a jump: its frames resumed by a driver of their own.
 * @param {Function} continuation - The continuation.
 * @param {*} value - What it is invoked with.
 * @returns {*} What the last of its frames returns.
 */
function reenterAhead(continuation, value) {
  return ahead(continuation.driver.hooks, continuation.frames, value, null, null);
}

/**
 * The loop of a driver with no interpreter beneath it: a call to make, or
 * frames to resume with a value.
 * @param {Object} hooks - What the driver needs.
 * @param {Object|null} frames - Frames to resume, innermost first, or null.
 * @param {*} value - What to resume them with.
 * @param {Function|null} first - A procedure to call first, or null.
 * @param {Array<*>|null} firstArgs - Its arguments.
 * @returns {*} The value the call or the frames end with.
 */
function ahead(hooks, frames, value, first, firstArgs) {
  const driver = { live: true, base: null, beneath: null, hooks, reenter: reenterAhead };
  let stack = frames;
  let frame = null;
  let callee = first;
  let args = firstArgs;
  let result = value;
  const savedDriver = currentDriver;
  const flush = flushState();
  currentDriver = driver;
  compiledStack.flushable = true;
  compiledStack.refusesCapture = false;
  try {
    for (;;) {
      if (frame !== null || callee !== null) {
        compiledStack.room = compiledStack.limit;
        try {
          if (frame !== null) {
            const { twin, pc, slots } = frame;
            frame = null;
            noteResume(twin, hooks.callScheme);
            result = twin(pc, { ...slots, $r: value });
          } else {
            const f = callee;
            const a = args;
            callee = null;
            args = null;
            result = hooks.call(f, a);
          }
          value = undefined;
          while (hooks.isTailCall(result)) result = hooks.call(result.func, result.args);
        } catch (e) {
          if (!(e instanceof NativeJump) || e.driver !== driver) throw e;
          nativeUnwinds.jumps++;
          stack = e.stack;
          result = e.value;
        }
      }
      if (result === UNWIND) {
        const pending = unwinding.pending;
        if (pending === null || !unwinding.frames.every((piece) => piece.twin !== undefined)
            || (pending.call === undefined && (pending.env !== null || pending.segment.length !== 0))) {
          throw new Error('an unwind reached compiled code running with no interpreter, which only an interpreter could finish');
        }
        for (let i = unwinding.frames.length - 1; i >= 0; i--) stack = { frame: unwinding.frames[i], next: stack };
        unwinding.frames = [];
        unwinding.pending = null;
        nativeUnwinds.finished++;
        if (pending.call !== undefined) {
          callee = pending.call;
          args = pending.args;
        } else {
          callee = pending.lambdaExpr;
          args = [hooks.nativeContinuation(driver, stack, null)];
        }
        continue;
      }
      if (stack === null) return result;
      frame = stack.frame;
      stack = stack.next;
      value = result;
      result = undefined;
    }
  } finally {
    restoreFlush(flush);
    currentDriver = savedDriver;
    driver.live = false;
  }
}

/**
 * The continuation of a capture a driver finishes, holding the driver and its
 * frames. Beneath them is the run's frame stack, copied once, at the driver's
 * first capture, while the run waits on the driver and so cannot have changed
 * it.
 * @param {Object} driver - The driver.
 * @param {Object|null} stack - The frames, innermost first.
 * @returns {Function} The continuation.
 */
function continuationOf(driver, stack, interpreter) {
  if (driver.base === null) driver.base = [...driver.beneath];
  return driver.hooks.nativeContinuation(driver, stack, interpreter);
}

/**
 * Takes a continuation a driver made by a jump to its driver, if compiled
 * code is running in that driver now with no run between; otherwise returns,
 * and the continuation is invoked the interpreter's way.
 * @param {Function} continuation - The continuation.
 * @param {*} value - What it is invoked with.
 * @returns {void}
 * @throws {NativeJump} Where it can be taken by a jump.
 */
export function jumpIfDriving(continuation, value) {
  const driver = continuation.driver;
  if (driver.live && currentDriver === driver) throw new NativeJump(driver, continuation.frames, value);
}

/**
 * The frame stack of a continuation a driver made, as the interpreter holds
 * one: the run's stack beneath the driver, then its compiled frames, moved to
 * the heap as one frame.
 * @param {Function} continuation - The continuation.
 * @returns {Array} The frame stack.
 */
export function nativeFrameStack(continuation) {
  const { base, hooks } = continuation.driver;
  const frames = [];
  for (let node = continuation.frames; node !== null; node = node.next) {
    frames.push(hooks.frameFor(node.frame.twin, node.frame.pc, node.frame.slots));
  }
  const fstack = [...base];
  hooks.pushMoved(fstack, frames.reverse());
  return fstack;
}
