/**
 * Shared runtime for the Stage 2a prototypes.
 *
 * Both backends use the same value representation and the same primitives, so
 * that neither is flattered by cheaper arithmetic or cheaper pairs. Exact
 * integers are BigInt, matching the real implementation, which keeps these
 * numbers comparable to the interpreter baseline rather than to an idealized
 * fixnum-only compiler.
 */

/** A Scheme pair. */
export class Pair {
  /**
   * @param {*} car - The first component.
   * @param {*} cdr - The second component.
   */
  constructor(car, cdr) {
    this.car = car;
    this.cdr = cdr;
  }
}

/** An interned Scheme symbol. */
export class Sym {
  /** @param {string} name - The symbol's name. */
  constructor(name) { this.name = name; }
}

const symbols = new Map();

/**
 * Interns a symbol, so `eq?` on symbols is identity comparison.
 * @param {string} name - The symbol's name.
 * @returns {Sym} The interned symbol.
 */
export function sym(name) {
  let found = symbols.get(name);
  if (found === undefined) { found = new Sym(name); symbols.set(name, found); }
  return found;
}

/** Sentinel returned by a tail call; see the backends for the protocol. */
export const TAIL_CALL = Symbol('tail-call');

/** Sentinel returned while a continuation capture unwinds the stack (convention B). */
export const UNWIND = Symbol('unwind');

/**
 * Mutable registers used to pass a pending tail call back to a trampoline
 * without allocating. A tail call sets these and returns TAIL_CALL.
 */
export const tail = { fn: null, args: null };

/**
 * State for an in-progress continuation capture or invocation under
 * convention B. `frames` accumulates reified frames as the unwind propagates
 * outward, innermost first.
 */
export const unwinding = { frames: null, receiver: null, target: null, value: undefined };

/**
 * A reified continuation under convention B: the list of frames that were on
 * the JavaScript stack when it was captured.
 */
class ContinuationB {
  /** @param {Array<Object>} frames - Reified frames, innermost first. */
  constructor(frames) {
    this.frames = frames;
    this.$continuationB = true;
  }
}

/**
 * Begins a continuation capture. Returns the unwind sentinel so that every
 * frame between here and the driver reifies itself on the way out.
 *
 * Marshall's point is that this is a *return*, not a throw: on a high-level VM
 * a throw costs thousands of times more than returning a distinguished value,
 * which is what made the original generalized-stack-inspection technique
 * perform badly.
 *
 * @param {Function} receiver - The procedure `call/cc` was given.
 * @returns {symbol} The unwind sentinel.
 */
export function callcc(receiver) {
  unwinding.frames = [];
  unwinding.receiver = receiver;
  unwinding.target = null;
  return UNWIND;
}

/**
 * Invokes a captured continuation, abandoning the current one.
 *
 * The current JavaScript stack has to be discarded before the captured frames
 * can be reinstated, so this also unwinds -- the frames it reifies on the way
 * out are thrown away by the driver.
 *
 * @param {ContinuationB} k - The continuation.
 * @param {*} value - The value to deliver to it.
 * @returns {symbol} The unwind sentinel.
 */
export function invokeK(k, value) {
  unwinding.frames = [];
  unwinding.receiver = null;
  unwinding.target = k;
  unwinding.value = value;
  return UNWIND;
}

/**
 * Reinstates a captured continuation by resuming each frame in turn.
 *
 * Frames are copied rather than resumed in place, which is what makes a
 * continuation multi-shot: invoking the same one twice must not have the second
 * invocation see state left by the first.
 *
 * @param {Array<Object>} frames - Reified frames, innermost first.
 * @param {*} value - The value to deliver to the innermost frame.
 * @returns {*} The result, or UNWIND if a further capture occurred.
 */
export function rewind(frames, value) {
  for (let i = 0; i < frames.length; i++) {
    const record = frames[i];
    const frame = Object.assign({}, record.frame, { $r: value });
    value = settle(record.proc(record.pc, frame));
    if (value === UNWIND) {
      // A capture happened while reinstating. Everything we had not yet
      // resumed is part of the new continuation.
      for (let j = i + 1; j < frames.length; j++) unwinding.frames.push(frames[j]);
      return UNWIND;
    }
  }
  return value;
}

/**
 * Drives a convention B program, servicing captures and invocations.
 * @param {function(): *} entry - The compiled entry point.
 * @returns {*} The program's value.
 */
export function driveB(entry) {
  let value = settle(entry());
  for (;;) {
    if (value !== UNWIND) return value;

    const frames = unwinding.frames;
    const receiver = unwinding.receiver;
    const target = unwinding.target;
    const pending = unwinding.value;
    unwinding.frames = null;
    unwinding.receiver = null;
    unwinding.target = null;

    if (target !== null) {
      // A continuation was invoked: the frames just reified belong to the
      // context being abandoned, so they are discarded.
      value = rewind(target.frames, pending);
      continue;
    }

    const k = new ContinuationB(frames);
    const wrapped = (v) => invokeK(k, v);
    wrapped.$isContinuation = true;
    value = settle(receiver(wrapped));
    // If the receiver returned normally, its value is the value of the
    // `call/cc` expression, so the captured frames resume with it.
    if (value !== UNWIND) value = rewind(frames, value);
  }
}

/**
 * Drives a call to completion, resolving any tail calls it returns.
 * @param {*} value - The value a call returned.
 * @returns {*} The final value.
 */
export function settle(value) {
  while (value === TAIL_CALL) {
    const fn = tail.fn;
    const args = tail.args;
    tail.fn = null;
    tail.args = null;
    value = fn(...args);
  }
  return value;
}

// =============================================================================
// Primitives
// =============================================================================

/** @returns {boolean} Scheme truthiness: everything except #f is true. */
export const truthy = (v) => v !== false;

const asFraction = (x) => (typeof x === 'bigint' ? x : null);

/**
 * Compares two numbers, mirroring the real implementation's rule that exact
 * operands are compared exactly.
 * @param {*} a - Left operand.
 * @param {*} b - Right operand.
 * @returns {number} -1, 0 or 1.
 */
function compare(a, b) {
  const fa = asFraction(a);
  const fb = asFraction(b);
  if (fa !== null && fb !== null) return fa < fb ? -1 : (fa > fb ? 1 : 0);
  const na = Number(a);
  const nb = Number(b);
  return na < nb ? -1 : (na > nb ? 1 : 0);
}

const add = (a, b) => (typeof a === 'bigint' && typeof b === 'bigint' ? a + b : Number(a) + Number(b));
const sub = (a, b) => (typeof a === 'bigint' && typeof b === 'bigint' ? a - b : Number(a) - Number(b));
const mul = (a, b) => (typeof a === 'bigint' && typeof b === 'bigint' ? a * b : Number(a) * Number(b));

/**
 * The primitive environment. Names match the Scheme names used by the
 * benchmark programs.
 */
export const primitives = {
  '+': (...a) => a.length === 2 ? add(a[0], a[1]) : a.reduce(add, 0n),
  '-': (...a) => a.length === 1 ? sub(0n, a[0]) : a.reduce(sub),
  '*': (...a) => a.length === 2 ? mul(a[0], a[1]) : a.reduce(mul, 1n),
  '<': (...a) => pairwise(a, (x, y) => compare(x, y) < 0),
  '>': (...a) => pairwise(a, (x, y) => compare(x, y) > 0),
  '<=': (...a) => pairwise(a, (x, y) => compare(x, y) <= 0),
  '>=': (...a) => pairwise(a, (x, y) => compare(x, y) >= 0),
  '=': (...a) => pairwise(a, (x, y) => compare(x, y) === 0),
  'quotient': (a, b) => a / b,
  'remainder': (a, b) => a % b,
  'not': (a) => a === false,
  'cons': (a, b) => new Pair(a, b),
  'car': (p) => p.car,
  'cdr': (p) => p.cdr,
  'pair?': (p) => p instanceof Pair,
  'null?': (p) => p === null,
  'list': (...a) => { let l = null; for (let i = a.length - 1; i >= 0; i--) l = new Pair(a[i], l); return l; },
  'length': (p) => { let n = 0n; while (p !== null) { n++; p = p.cdr; } return n; },
  'append': (a, b) => { const items = []; while (a !== null) { items.push(a.car); a = a.cdr; }
                        let l = b === undefined ? null : b;
                        for (let i = items.length - 1; i >= 0; i--) l = new Pair(items[i], l);
                        return l; },
  'eq?': (a, b) => a === b,
  // Reports the JavaScript stack at the point of call. A debugger's call-stack
  // panel is rendered from exactly this information, so what shows up here is
  // what a developer would see when paused.
  'js-stack': () => {
    // Node truncates stack traces at ten frames by default, which would make a
    // deep Scheme stack look shallow for reasons that have nothing to do with
    // the calling convention.
    const previous = Error.stackTraceLimit;
    Error.stackTraceLimit = 500;
    const stack = new Error().stack;
    Error.stackTraceLimit = previous;
    return stack;
  },
  'display': (v) => { process.stdout.write(render(v)); return undefined; },
  'newline': () => { process.stdout.write('\n'); return undefined; }
};

/**
 * Applies a binary predicate pairwise across a variadic argument list.
 * @param {Array<*>} args - The arguments.
 * @param {function(*, *): boolean} test - The binary predicate.
 * @returns {boolean} True if the predicate holds for every adjacent pair.
 */
function pairwise(args, test) {
  for (let i = 0; i < args.length - 1; i++) {
    if (!test(args[i], args[i + 1])) return false;
  }
  return true;
}

/**
 * Renders a Scheme value the way the benchmark harness compares them.
 * @param {*} v - A value.
 * @returns {string} Its external representation.
 */
export function render(v) {
  if (v === null) return '()';
  if (v === true) return '#t';
  if (v === false) return '#f';
  if (typeof v === 'bigint') return v.toString();
  if (v instanceof Sym) return v.name;
  if (v instanceof Pair) {
    // Benchmarks only ever print an improper pair or a proper list.
    if (!(v.cdr instanceof Pair) && v.cdr !== null) {
      return `(${render(v.car)} . ${render(v.cdr)})`;
    }
    const parts = [];
    let cursor = v;
    while (cursor instanceof Pair) { parts.push(render(cursor.car)); cursor = cursor.cdr; }
    return `(${parts.join(' ')})`;
  }
  return String(v);
}
