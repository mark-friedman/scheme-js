/**
 * The explicit stack and trampoline used by convention A.
 *
 * Continuation frames are records in a JavaScript array. A compiled procedure
 * never calls another compiled procedure directly: it returns a control
 * instruction to the trampoline, which is what keeps the JavaScript stack one
 * frame deep regardless of Scheme recursion depth.
 *
 * `call/cc` is therefore nearly free to implement -- capturing a continuation
 * is copying the array -- which is this convention's main attraction.
 */

/** Marks the `call/cc` primitive so the trampoline can recognise it. */
export const CALLCC = { $callcc: true, $arity: 1 };

/** Control instruction kinds, as small integers to keep the trampoline cheap. */
const CALL = 0;
const JUMP = 1;
const RET = 2;

/** The single instruction record, reused to avoid allocating per transfer. */
const instr = { kind: 0, fn: null, args: null, value: undefined, resume: 0, frame: null, owner: null };

/** The continuation frame stack. */
export let stack = [];

/**
 * Issues a non-tail call: pushes a continuation frame and transfers control.
 * @param {Function} fn - The procedure to call.
 * @param {Array<*>} args - Its arguments.
 * @param {Function} owner - The calling procedure, to resume into.
 * @param {number} resume - Block index to resume at.
 * @param {Object} frame - The caller's spilled locals.
 * @returns {Object} The control instruction.
 */
export function call(fn, args, owner, resume, frame) {
  stack.push({ owner, resume, frame });
  instr.kind = CALL;
  instr.fn = fn;
  instr.args = args;
  return instr;
}

/**
 * Issues a tail call: transfers control without pushing a frame.
 * @param {Function} fn - The procedure to call.
 * @param {Array<*>} args - Its arguments.
 * @returns {Object} The control instruction.
 */
export function jump(fn, args) {
  instr.kind = JUMP;
  instr.fn = fn;
  instr.args = args;
  return instr;
}

/**
 * Returns a value to the caller.
 * @param {*} value - The value.
 * @returns {Object} The control instruction.
 */
export function ret(value) {
  instr.kind = RET;
  instr.value = value;
  return instr;
}

/**
 * Runs a compiled procedure to completion.
 * @param {Function} fn - The procedure.
 * @param {Array<*>} args - Its arguments.
 * @returns {*} The final value.
 */
export function run(fn, args = []) {
  const base = stack.length;
  let next = fn;
  let nextArgs = args;
  let pc = 0;
  let frame = null;

  for (;;) {
    const result = next(pc, frame, ...nextArgs);

    if (result !== instr) {
      // A primitive, or any plain JavaScript function, returned directly.
      if (stack.length === base) return result;
      const k = stack.pop();
      next = k.owner;
      pc = k.resume;
      frame = k.frame;
      frame.$r = result;
      nextArgs = [];
      continue;
    }

    if (instr.kind === RET) {
      const value = instr.value;
      if (stack.length === base) return value;
      const k = stack.pop();
      next = k.owner;
      pc = k.resume;
      frame = k.frame;
      frame.$r = value;
      nextArgs = [];
      continue;
    }

    // CALL and JUMP differ only in whether a frame was pushed, which `call`
    // has already done by the time we get here.
    const target = instr.fn;
    const targetArgs = instr.args;

    if (target === CALLCC) {
      // Capturing is copying the frame array. This is the whole attraction of
      // this convention: the continuation is already a first-class object in
      // the representation, so no reification pass is needed.
      const captured = stack.slice();
      const k = makeContinuation(captured, base);
      next = targetArgs[0];
      pc = 0;
      frame = null;
      nextArgs = [k];
      continue;
    }

    if (target !== null && typeof target === 'object' && target.$continuation) {
      // Reinstating a continuation replaces the stack wholesale. The captured
      // array is copied rather than adopted, so the same continuation can be
      // invoked again and still see its original frames -- multi-shot.
      stack.length = 0;
      for (const f of target.captured) stack.push(f);
      const value = targetArgs.length === 0 ? undefined : targetArgs[0];
      if (stack.length === base) return value;
      const k = stack.pop();
      next = k.owner;
      pc = k.resume;
      frame = k.frame;
      frame.$r = value;
      nextArgs = [];
      continue;
    }

    if (typeof target === 'function' && target.$arity !== undefined) {
      next = target;
      pc = 0;
      frame = null;
      nextArgs = targetArgs;
    } else {
      // A primitive: call it directly and deliver the value as a return.
      const value = target(...targetArgs);
      if (stack.length === base) return value;
      const k = stack.pop();
      next = k.owner;
      pc = k.resume;
      frame = k.frame;
      frame.$r = value;
      nextArgs = [];
    }
  }
}

/**
 * Builds a first-class continuation from a captured frame stack.
 * @param {Array<Object>} captured - The captured frames.
 * @param {number} base - Stack base of the run that captured it.
 * @returns {Object} A continuation object the trampoline recognises.
 */
function makeContinuation(captured, base) {
  return { $continuation: true, $arity: 1, captured, base };
}

/**
 * Captures the current continuation as a copy of the frame stack.
 *
 * Copying rather than sharing is what makes the captured continuation
 * multi-shot: the running computation goes on mutating its own frames.
 *
 * @returns {Array<Object>} The captured stack.
 */
export function capture() {
  return stack.slice();
}

/**
 * Reinstates a captured continuation.
 * @param {Array<Object>} captured - A stack from `capture`.
 * @returns {void}
 */
export function reinstate(captured) {
  stack = captured.slice();
}

/** Resets the machine between benchmark runs. */
export function reset() {
  stack = [];
}
