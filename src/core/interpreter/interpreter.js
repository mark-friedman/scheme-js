import { Values, isSchemeClosure, callSchemeProcedure, registerGlobalEnvironment, methodReceiver } from './values.js';
import { LiteralNode, TailAppNode, ANS, CTL, ENV, FSTACK, ExceptionHandlerFrame, RaiseNode } from './ast.js';
import { SchemeError } from './errors.js';
import { CaptureUnwind, UNWIND, completeCapture, unwinding, compiledStack, flushState, restoreFlush, beginStepAgain, openCompiledSegment, enterRun, leaveRun } from './unwind.js';
import { CompiledEntryRemainder } from './frames.js';
import { TailCall } from './values.js';
import { takeCompiledRaise } from './ast_nodes.js';
import { interpretCompiledOver } from './library_registry.js';
import { globalContext } from './context.js';

/**
 * Finds the nearest ExceptionHandlerFrame on the stack.
 * @param {Array} fstack - The frame stack
 * @returns {number} Index of handler or -1 if not found
 */
function findExceptionHandler(fstack) {
  for (let i = fstack.length - 1; i >= 0; i--) {
    if (fstack[i] instanceof ExceptionHandlerFrame) {
      return i;
    }
  }
  return -1;
}

/**
 * Wraps a JS Error as a SchemeError if not already one.
 * @param {Error} e - The error to wrap
 * @returns {SchemeError} A SchemeError instance
 */
function wrapJsError(e) {
  if (e instanceof SchemeError) {
    return e;
  }
  // Wrap generic JS errors
  return new SchemeError(e.message, [], e.name);
}

import { schemeToJs, schemeToJsDeep } from './js_interop.js';

/**
 * Unpacks a Values object to its first value for JS interop.
 * Also performs Scheme->JS number and char conversion.
 *
 * By default, uses deep conversion (schemeToJsDeep) which recursively
 * converts within vectors, records and objects. Only the code that started
 * the run chooses otherwise, through its `jsAutoConvert` option -- `'raw'`
 * when it holds Scheme values -- and nothing the program does, so that a
 * JavaScript caller of a Scheme procedure gets what the procedure's plain
 * call promises.
 *
 * @param {*} result - The result to unpack
 * @param {Object} [options={}] - Conversion options (passed to schemeToJsDeep)
 * @returns {*} Converted value
 */
function unpackForJs(result, options = {}) {
  const mode = options.jsAutoConvert ?? 'deep';

  if (mode === 'raw') {
    // Deliberately *not* unpacked here. Collapsing several values to the first
    // one is a JavaScript-interop behaviour -- a JavaScript caller can only
    // receive one value -- and `raw` means the caller is not one. Compiled code
    // reaches an interpreted procedure through this path, so unpacking here
    // silently dropped every value but the first on the way out, which is how
    // `(call-with-values p +)` with an interpreted `p` returning two values
    // came back as the first of them.
    return result;
  }

  if (result instanceof Values) {
    result = result.first();
  }
  if (mode === 'deep' || mode === true) {
    return schemeToJsDeep(result, options);
  }
  if (mode === 'shallow' || mode === false) {
    return schemeToJs(result);
  }

  // Fallback to deep for any unknown mode
  return schemeToJsDeep(result, options);
}

/**
 * SentinelFrame - Boundary Marker for JavaScript ↔ Scheme Transitions
 *
 * ## Purpose
 * SentinelFrame is pushed onto the frame stack when JavaScript code calls back
 * into Scheme (e.g., when JS invokes a Scheme closure that was passed as a callback).
 * It serves as a "stop marker" that tells the interpreter when to exit the nested
 * `run()` call and return control to JavaScript.
 *
 * ## The Problem It Solves
 * Consider this scenario:
 * ```scheme
 * (js-call "array.map" my-scheme-function)
 * ```
 * Here, Scheme calls JS, which then calls back into Scheme for each element.
 * Without SentinelFrame, when `my-scheme-function` completes, the interpreter
 * would keep running frames from the *outer* Scheme computation, which is wrong.
 *
 * ## How It Works
 * 1. When JS calls a Scheme closure via `runWithSentinel()`:
 *    - A new SentinelFrame is pushed onto the stack
 *    - The inner `run()` loop starts executing
 *
 * 2. When the closure's body completes:
 *    - The interpreter pops frames until it reaches SentinelFrame
 *    - SentinelFrame's `step()` throws `SentinelResult` with the answer
 *
 * 3. The outer `run()` catches `SentinelResult`:
 *    - Returns the wrapped value to JavaScript
 *    - Execution continues in JS land
 *
 * ## Relationship with jsContextStack
 * SentinelFrame works together with `jsContextStack` for proper `dynamic-wind`
 * handling. See `pushJsContext()` and `getParentContext()`.
 *
 * @see runWithSentinel - Creates the stack with a SentinelFrame
 * @see SentinelResult - The exception thrown to terminate the nested run
 * @see frames.js filterSentinelFrames - Removes SentinelFrames from continuation copies
 */
class SentinelFrame {
  /**
   * @param {boolean} [compiledBoundary=false] - True when this marks a call
   *   from *compiled* code into the interpreter that an unwind can cross.
   *   Compiled procedures run in JavaScript stack frames that `FSTACK` does
   *   not represent, so a continuation captured above this marker would
   *   silently omit everything the compiled caller had left to do. Recording
   *   it is what lets `CallCCNode` bring those frames in by unwinding, and the
   *   run it starts pass the unwind on (`unwindsOut`).
   * @param {boolean} [refusesCapture=false] - True when the compiled caller is
   *   an inline expansion calling what its primitive was redefined to, which
   *   has no point to resume from, so that a capture above is refused.
   */
  constructor(compiledBoundary = false, refusesCapture = false) {
    /** Identifies every sentinel, including subclasses, for stack filtering. */
    this.isSentinel = true;
    this.compiledBoundary = compiledBoundary;
    this.refusesCapture = refusesCapture;
  }

  /**
   * Executes when the interpreter reaches this frame.
   * This means the nested Scheme computation has completed.
   * We throw SentinelResult to break out of the inner run() loop.
   *
   * @param {Array} registers - The interpreter registers [ans, ctl, env, fstack, this]
   * @param {Interpreter} interpreter - The interpreter instance
   * @throws {SentinelResult} Always throws to signal completion
   */
  step(registers, interpreter) {
    // We have reached the bottom of the inner run's stack.
    // The result is in registers[ANS].
    // We throw a special signal to break out of interpreter.run immediately.
    throw new SentinelResult(registers[ANS]);
  }
}

/**
 * SentinelResult - Control Flow Exception for Nested Run Termination
 *
 * This is thrown by SentinelFrame to signal that a nested `run()` call
 * has completed successfully. It's caught in the trampoline loop (line ~225)
 * and causes the run to return the wrapped value to its JavaScript caller.
 *
 * Note: This is NOT an error. It's a control flow mechanism similar to
 * how some systems use exceptions for non-local returns.
 *
 * @see SentinelFrame - The frame that throws this
 */
class SentinelResult {
  /**
   * @param {*} value - The result value from the nested computation
   */
  constructor(value) {
    this.value = value;
  }
}

/**
 * The core Scheme interpreter.
 * Manages the top-level trampoline loop and register state.
 */
export class Interpreter {
  /**
   * Creates a new interpreter instance.
   * @param {InterpreterContext} [context] - Optional context for state isolation.
   *   If not provided, uses the global shared context.
   */
  constructor(context = null) {
    /**
     * The interpreter context containing all mutable state.
     * @type {InterpreterContext}
     */
    this.context = context || globalContext;

    /**
     * The global environment for the interpreter.
     * @type {Environment | null}
     */
    this.globalEnv = null;
    this.depth = 0;

    /**
     * Whether the run in progress passes an unwind it receives on to its
     * caller, rather than finishing it. True in a run compiled code called,
     * while that compiled code could hand the unwind on in turn: the run adds
     * its own frames to the unwind and returns the unwind sentinel, and the
     * compiled caller saves itself as any compiled frame does. So a capture,
     * or a move of frames to the heap, passes through as many nested runs as
     * compiled and interpreted code alternate, and the first run that cannot
     * pass it on finishes it. Set by `run` from the sentinel it starts on.
     * @type {boolean}
     */
    this.unwindsOut = false;

    /**
     * Stack of frame stacks representing the Scheme context at JS boundary crossings.
     * When Scheme calls a JS function, we push the current fstack here.
     * When JS calls back into Scheme (via a callable closure/continuation),
     * we use the top of this stack as the parent context.
     * @type {Array<Array>}
     */
    this.jsContextStack = [];

    /**
     * Optional debug runtime for debugging support.
     * When set, the interpreter will check for breakpoints and stepping before each step.
     * @type {import('../../debug/scheme_debug_runtime.js').SchemeDebugRuntime|null}
     */
    this.debugRuntime = null;

    /**
     * The compiler tier, which compiles the program's own top-level
     * procedures as it runs, or null when nothing does
     * (`src/compiler/tiering.js`, `attachTier`): the tier's record from
     * `src/compiler/tier.scm`, holding a Scheme procedure for each thing the
     * interpreter tells or asks it -- `bound`, given a top-level name, the
     * closure bound to it and the environment binding it; `due`, given a
     * closure whose countdown has run out; and `form`, given a top-level form
     * and its environment, which answers the procedure to run the form as, or
     * #f. The interpreter calls them with `callSchemeProcedure`, and knows
     * nothing else about the compiler, which a browser page loads after it.
     * @type {Object|null}
     */
    this.tier = null;

    /**
     * Whether the program is being debugged: a breakpoint set, a step in
     * progress, or paused, as the debug runtime last said
     * (`interpretForDebugger`). The tier compiles nothing meanwhile.
     * @type {boolean}
     */
    this.debugging = false;
  }


  /**
   * Pushes the current Scheme context before calling into JS.
   * @param {Array} fstack - The current frame stack.
   */
  pushJsContext(fstack) {
    // The stack is recorded by reference plus its current depth, and only
    // copied if something actually asks for it. Every primitive application
    // goes through here, and the overwhelming majority of primitives never
    // re-enter Scheme, so eagerly copying the whole frame stack each time was
    // an O(depth) cost paid almost entirely for nothing.
    //
    // Recording the depth rather than copying is safe because the frames below
    // it cannot change while this entry is live: the interpreter is suspended
    // inside the JS call for exactly that window, so nothing is pushing or
    // popping beneath it. A continuation invocation replaces `registers[FSTACK]`
    // with a different array entirely, which leaves this entry pointing at the
    // same stack the eager copy would have captured.
    this.jsContextStack.push({ stack: fstack, depth: fstack.length });
  }

  /**
   * Pops the Scheme context after returning from JS.
   */
  popJsContext() {
    this.jsContextStack.pop();
  }

  /**
   * Gets the current parent context (if any) for re-entering Scheme from JS.
   * @returns {Array} The parent frame stack, or empty array if none.
   */
  getParentContext() {
    if (this.jsContextStack.length > 0) {
      const entry = this.jsContextStack[this.jsContextStack.length - 1];
      return entry.stack.slice(0, entry.depth);
    }
    return [];
  }

  /**
   * Sets the global environment. Required before running.
   * @param {Environment} env The global environment, pre-filled with primitives.
   */
  setGlobalEnv(env) {
    this.globalEnv = env;
    // Compiled code called from JavaScript runs on the interpreter its
    // environment belongs to (`createCompiledProcedure`).
    registerGlobalEnvironment(env, this);
  }

  /**
   * Runs a piece of Scheme code (as an AST).
   * @param {Executable} ast - The AST node to execute.
   * @param {Environment} [env] - The environment to run in. Defaults to globalEnv.
   * @param {Array} [initialStack] - Initial frame stack.
   * @param {*} [thisContext] - The JavaScript 'this' context.
   * @param {Object} [options={}] - Options for unpacking the result (passed to unpackForJs).
   * @returns {*} The final result of the computation.
   */
  run(ast, env = this.globalEnv, initialStack = [], thisContext = undefined, options = {}) {
    if (!this.globalEnv) {
      throw new SchemeError("Interpreter global environment is not set. Call setGlobalEnv() first.");
    }

    // The "CPU registers" - see stepables.js for constant definitions
    // ANS (0): answer - holds result of last computation
    // CTL (1): control - holds next AST node or Frame to execute
    // ENV (2): environment - holds current lexical environment
    // FSTACK (3): frame stack - holds continuation frames
    // THIS (4): this context - holds current JS 'this' context
    // We use a COPY of the initialStack to avoid mutating the parent's record of it,
    // although frames themselves are shared.
    const registers = [null, ast, env, [...initialStack], thisContext];
    // The sentinel this run started on, if JavaScript started it: what marks a
    // continuation captured in it.
    const bottom = initialStack[initialStack.length - 1];
    const ownSentinel = bottom !== undefined && bottom.isSentinel === true ? bottom : null;

    // Track recursion depth
    this.depth++;
    // Whether compiled code may move its frames to the heap belongs to whoever
    // called this run, and is theirs again however it ends.
    const flush = flushState();
    const unwindsOut = this.unwindsOut;
    this.unwindsOut = initialStack.length > 0
      && initialStack[initialStack.length - 1].compiledBoundary === true;
    // No driver beneath this run takes a continuation by a jump through it.
    const driver = enterRun();
    // Compiled code reads `this` as this run's receiver.
    const receiver = methodReceiver.v;
    methodReceiver.v = thisContext;

    // The Top-Level Trampoline
    try {
      while (true) {
        try {
          // The `step` method returns `true` to continue the trampoline
          // (a tail call) or `false` to halt (a value return).
          if (this.step(registers)) {
            continue;
          }

          // --- `step` returned false ---
          // This means a value is in `ans` and `ctl` is "done".
          // We must now check the frame stack.
          const fstack = registers[FSTACK];

          if (fstack.length === 0) {
            // --- Fate #1: Normal Termination ---
            // Stack is empty, computation is done.
            // Closures are now callable functions, no wrapping needed.
            return unpackForJs(registers[ANS], options);
          }

          // --- Fate #2: Restore a Frame ---
          // The stack is not empty. Pop the next frame.
          const frame = fstack.pop();

          // Set the frame as the new 'ctl'
          registers[CTL] = frame;

          // The 'ans' register already contains the value this frame was waiting for.
          // The 'env' register is restored by the frame itself in its `step` method.

          // Loop again to execute the frame's `step`
          continue;

        } catch (e) {
          // A capture that has to cross compiled frames abandons this run so
          // that the compiled frames below it can record themselves. The
          // sentinel goes back to whoever called in -- compiled code -- rather
          // than through the usual conversion for a JavaScript caller.
          if (e instanceof CaptureUnwind) return UNWIND;

          // Check for Continuation Unwind
          // We check the constructor name to avoid circular dependency imports if possible.
          if (e.constructor.name === 'ContinuationUnwind') {
            // A continuation captured in this run, invoked from a run nested
            // in it -- by compiled code, which calls a continuation through a
            // run of its own -- is this run's to take: its stack holds this
            // run's sentinel. Invoked again from here, it stays in this run,
            // as it would had this run invoked it. Thrown on past, it would
            // reach the JavaScript that started this run as an exception.
            if (this.depth > 1 && ownSentinel !== null && e.continuation !== null && e.target.includes(ownSentinel)) {
              registers[CTL] = new TailAppNode(new LiteralNode(e.continuation),
                e.args.map((arg) => new LiteralNode(arg)));
              continue;
            }
            // If we are nested (depth > 1), strictly propagate up to the top level
            if (this.depth > 1) {
              throw e;
            }

            // Top Level (depth == 1): Adopt the hijacked state
            const newRegisters = e.registers;

            registers[ANS] = newRegisters[ANS];
            // CTL is only meaningful if !isReturn.
            registers[ENV] = newRegisters[ENV];
            registers[FSTACK] = newRegisters[FSTACK];

            if (e.isReturn) {
              // Mimic "step returned false" (Value Return)
              // We must check if stack is empty, or pop the next frame.

              const fstack = registers[FSTACK];
              if (fstack.length === 0) {
                // Done - closures are callable, no wrapping needed
                return unpackForJs(registers[ANS], options);
              }

              // Pop next frame and continue
              const frame = fstack.pop();
              registers[CTL] = frame;
              continue;
            } else {
              // Mimic "step returned true" (Tail Call)
              // ctl must be valid.
              registers[CTL] = newRegisters[CTL];
              continue;
            }
          }

          // Check for SentinelResult (Control Flow for JS Interop)
          if (e instanceof SentinelResult) {
            return unpackForJs(e.value, options);
          }

          // Compiled code raised, and threw the raise here for this run to
          // perform from where it called compiled code, with the handlers on
          // this frame stack; see `raiseFromCompiledCode`.
          const raise = takeCompiledRaise(e);
          if (raise !== null) {
            registers[CTL] = raise;
            continue;
          }

          // Check if there's an ExceptionHandlerFrame on the stack
          // If so, route the JS error through Scheme's exception system
          const handlerIndex = findExceptionHandler(registers[FSTACK]);
          if (handlerIndex !== -1) {
            // Wrap JS error as SchemeError if needed
            const schemeError = wrapJsError(e);
            // Use RaiseNode to properly unwind and invoke handler
            // This ensures dynamic-wind 'after' thunks are called
            registers[CTL] = new RaiseNode(schemeError, false);
            continue;
          }

          // No handler found - propagate to JS caller
          if (!(e instanceof SchemeError)) {
            console.error("Native JavaScript error caught in interpreter:", e);
          }
          throw e;
        }
      }
    } finally {
      this.depth--;
      restoreFlush(flush);
      this.unwindsOut = unwindsOut;
      leaveRun(driver);
      methodReceiver.v = receiver;
    }
  }

  /**
   * Runs an AST with a sentinel frame on the stack.
   * Used when JavaScript code calls a Scheme closure.
   * The sentinel ensures the nested run terminates properly.
   * Uses the parent context from jsContextStack for proper dynamic-wind handling.
   *
   * @param {Executable} ast - The AST to execute.
   * @param {*} [thisContext] - The value for the 'this' register.
   * @returns {*} The result of the computation.
   */
  /**
   * Runs an AST with a sentinel frame on the stack.
   * @param {Executable} ast - The AST to execute.
   * @param {*} [thisContext] - The value for the 'this' register.
   * @param {Object} [options={}] - Options for unpacking the result.
   * @returns {*} The result of the computation.
   */
  runWithSentinel(ast, thisContext = undefined, options = {}) {
    // Get the parent context (the Scheme stack at the point where we entered JS)
    const parentContext = this.getParentContext();
    // Marked as a boundary an unwind can cross only if the compiled caller can
    // hand the unwind on to an interpreter: `flushable` says no JavaScript
    // caller -- a primitive calling a procedure back -- sits beneath it.
    // Otherwise the run finishes what reaches it, as a run JavaScript called
    // does.
    const stackWithSentinel = [
      ...parentContext,
      new SentinelFrame(options.compiledBoundary === true && compiledStack.flushable,
        options.compiledBoundary === true && compiledStack.refusesCapture)
    ];
    return this.run(ast, this.globalEnv, stackWithSentinel, thisContext, options);
  }

  /**
   * Calls a compiled procedure for JavaScript, its arguments already
   * converted into Scheme: its compiled code called directly, as a run calls
   * it, and its value converted out, as a run's is.
   *
   * A run of the interpreter is what finishes, before the procedure returns to
   * JavaScript, what its code cannot on the JavaScript stack: frames moved to
   * the heap when its recursion goes deep, a continuation captured beneath it,
   * a tail call to a procedure that is not its own. Every call used to start
   * one, which cost JavaScript half a microsecond a call whichever tier made
   * the procedure. Now one is started only when the code returns one of those,
   * or throws, and its first step takes up what the code left
   * (`CompiledEntryRemainder`), with the stack the run started with -- the
   * frames beneath the JavaScript caller and the sentinel -- as it would have
   * been at that point had the run made the call. While the code runs, the
   * run's depth is counted as the run's would be, so that a run nested in it
   * passes a continuation's unwinding on as it did.
   *
   * @param {Function} raw - The procedure's raw entry.
   * @param {Array<*>} args - Its arguments, Scheme values.
   * @param {*} [thisContext] - The JavaScript `this` it was called with.
   * @returns {*} Its value, converted for JavaScript.
   */
  callCompiledEntry(raw, args, thisContext = undefined) {
    // What the code reads as `this` is the receiver it was called with.
    const receiver = methodReceiver.v;
    methodReceiver.v = thisContext;
    try {
      const flush = openCompiledSegment(false);
      let result;
      this.depth++;
      try {
        result = raw(...args);
      } catch (e) {
        restoreFlush(flush);
        this.depth--;
        return this.runWithSentinel(new CompiledEntryRemainder('throw', e), thisContext);
      }
      restoreFlush(flush);
      this.depth--;
      if (result === UNWIND) return this.runWithSentinel(new CompiledEntryRemainder('unwind', null), thisContext);
      if (result instanceof TailCall) return this.runWithSentinel(new CompiledEntryRemainder('tail', result), thisContext);
      return unpackForJs(result);
    } finally {
      methodReceiver.v = receiver;
    }
  }

  /**
   * Invokes a captured continuation from JavaScript.
   * This is called when JS code invokes a callable continuation.
   *
   * @param {Function} continuation - The callable continuation (with fstack attached).
   * @param {*} value - The value to pass to the continuation.
   * @param {*} [thisContext] - The value for the 'this' register.
   * @param {Object} [options] - As for `runWithSentinel`: `jsAutoConvert: 'raw'`
   *   for a caller holding Scheme values.
   * @returns {*} The result of invoking the continuation.
   */
  invokeContinuation(continuation, value, thisContext = undefined, options = {}) {
    // Build an AST that invokes the continuation
    const ast = new TailAppNode(
      new LiteralNode(continuation),
      [new LiteralNode(value)]
    );

    // Run with sentinel and parent context
    return this.runWithSentinel(ast, thisContext, options);
  }



  /**
   * Executes a single step of the computation.
   * This polymorphically calls the `step` method on the `ctl` object.
   * @param {Array} registers - The [ans, ctl, env, fstack] registers array.
   * @returns {boolean} `true` to continue the trampoline, `false` to halt.
   */
  step(registers) {
    const ctl = registers[CTL];

    // Debug hook: check if we should pause before this step. Only while the
    // program is being debugged -- a breakpoint set, a step in progress --
    // does the debugger have anything to check.
    if (this.debugRuntime?.debugging && ctl.source) {
      if (this.debugRuntime.shouldPause(ctl.source, registers[ENV])) {
        // A run compiled code called is beneath the compiled frames, on the
        // JavaScript stack, and cannot wait there; so it moves them to the
        // heap and the step is taken again, and paused at, by the run that
        // finishes the move.
        if (beginStepAgain(registers[FSTACK], ctl, registers[ENV])) throw new CaptureUnwind();
        this.debugRuntime.pause(ctl.source, registers[ENV]);
      }
    }

    return ctl.step(registers, this);
  }

  /**
   * Sets the debug runtime for this interpreter.
   * @param {import('../../debug/scheme_debug_runtime.js').SchemeDebugRuntime|null} debugRuntime
   */
  setDebugRuntime(debugRuntime) {
    // A runtime taken away can no longer pause anything, so compiled code may
    // run again.
    if (debugRuntime === null) this.interpretForDebugger(false);
    this.debugRuntime = debugRuntime;
    // Optional, so a runtime written against the older interface still works.
    debugRuntime?.attachInterpreter?.(this);
  }

  /**
   * Runs the procedures compiled over interpreted closures that the debugger
   * chooses as those closures while the program is being debugged, the rest
   * compiled, and every one compiled again once it is not.
   *
   * The debugger pauses only between the interpreter's steps, which compiled
   * code takes none of, so a breakpoint inside a compiled procedure could not
   * fire; the debugger chooses those holding a breakpoint, or every one while
   * it steps (`debugger-interpretation` in debugger.scm). One it calls from
   * compiled code pauses all the same (`step`). Called by the debug runtime
   * whenever what it needs changes. The tier compiles nothing meanwhile. See
   * `interpretCompiledOver`.
   *
   * @param {boolean|Function} which - True for every one, false for none --
   *   the program is not being debugged -- or a Scheme procedure saying of a
   *   closure whether it is one.
   */
  interpretForDebugger(which) {
    this.debugging = which !== false;
    if (this.globalEnv) interpretCompiledOver(which, this.globalEnv);
  }

  /**
   * Runs one top-level form of a program: as the procedure the compiler tier
   * compiled it to, when one is attached and compiles it, and as `run` does
   * otherwise.
   *
   * For the places a program's own forms come in -- a REPL, a file, a page's
   * scripts -- and not for code the implementation runs for itself, which
   * `run` serves.
   *
   * @param {Executable} ast - The analyzed form.
   * @param {Environment} [env] - The environment; the global one by default.
   * @param {Object} [options] - As for `run`.
   * @returns {*} Its value.
   */
  runTopLevel(ast, env = this.globalEnv, options = undefined) {
    const thunk = this.tier ? callSchemeProcedure(this.tier.form, [ast, env]) : false;
    const form = thunk ? new TailAppNode(new LiteralNode(thunk), []) : ast;
    return this.run(form, env, [], undefined, options);
  }

  /**
   * Runs Scheme code asynchronously with periodic yields to the event loop.
   * This enables non-blocking execution for long-running computations.
   *
   * @param {Executable} ast - The AST node to execute.
   * @param {Environment} [env] - The environment to run in.
   * @param {Object} [options={}] - Async execution options.
   * @param {number} [options.stepsPerYield=1000] - Steps between yields.
   * @param {Function} [options.onYield] - Callback invoked on each yield.
   * @returns {Promise<*>} The result of the computation.
   */
  async runAsync(ast, env = this.globalEnv, options = {}) {
    const stepsPerYield = options.stepsPerYield ?? 1000;
    const onYield = options.onYield ?? (() => { });

    if (!this.globalEnv) {
      throw new SchemeError("Interpreter global environment is not set.");
    }

    // A library loaded since the program was last run is compiled, and while
    // the program is being debugged it should run as its closures too.
    this.debugRuntime?.updateInterpretation?.();

    const registers = [null, ast, env, [], undefined];
    this.depth++;
    // As in `run`. An asynchronous run is never called by compiled code.
    const flush = flushState();
    const unwindsOut = this.unwindsOut;
    this.unwindsOut = false;
    const driver = enterRun();
    const receiver = methodReceiver.v;
    methodReceiver.v = undefined;

    try {
      let stepCount = 0;

      while (true) {
        try {
          // Execute one step
          if (this.step(registers)) {
            stepCount++;

            // Check if debugger has paused (e.g., breakpoint, exception)
            if (this.debugRuntime?.isPaused()) {
              await this.debugRuntime.waitForResume();

              // Check if evaluation was aborted while paused
              if (this.debugRuntime.isAborted()) {
                throw new SchemeError("Evaluation aborted");
              }
            }

            // Check if we should yield to the event loop
            if (stepCount >= stepsPerYield) {
              stepCount = 0;
              onYield();
              // Yield to event loop
              await new Promise(resolve => setTimeout(resolve, 0));
            }
            continue;
          }

          // Step returned false - check frame stack
          const fstack = registers[FSTACK];

          if (fstack.length === 0) {
            return unpackForJs(registers[ANS], options);
          }

          const frame = fstack.pop();
          registers[CTL] = frame;
          continue;

        } catch (e) {
          if (e.constructor.name === 'ContinuationUnwind') {
            if (this.depth > 1) throw e;

            const newRegisters = e.registers;
            registers[ANS] = newRegisters[ANS];
            registers[ENV] = newRegisters[ENV];
            registers[FSTACK] = newRegisters[FSTACK];

            if (e.isReturn) {
              const fstack = registers[FSTACK];
              if (fstack.length === 0) {
                return unpackForJs(registers[ANS], options);
              }
              registers[CTL] = fstack.pop();
              continue;
            } else {
              registers[CTL] = newRegisters[CTL];
              continue;
            }
          }

          if (e instanceof SentinelResult) {
            return unpackForJs(e.value, options);
          }

          // As in `run`.
          const raise = takeCompiledRaise(e);
          if (raise !== null) {
            registers[CTL] = raise;
            continue;
          }

          // Check if debugger has paused on this exception
          if (this.debugRuntime?.isPaused()) {
            await this.debugRuntime.waitForResume();
          }

          const handlerIndex = findExceptionHandler(registers[FSTACK]);
          if (handlerIndex !== -1) {
            registers[CTL] = new RaiseNode(wrapJsError(e), false);
            continue;
          }

          throw e;
        }
      }
    } finally {
      this.depth--;
      restoreFlush(flush);
      this.unwindsOut = unwindsOut;
      leaveRun(driver);
      methodReceiver.v = receiver;
    }
  }



  /**
   * Evaluates a Scheme code string asynchronously.
   *
   * @param {string} code - The Scheme source code.
   * @param {Object} [options={}] - Async execution options.
   * @returns {Promise<*>} The result of the computation.
   */
  async evaluateStringAsync(code, options = {}) {
    const { parse } = await import('./reader.js');
    const { analyze } = await import('./expand.js');
    const { list } = await import('./cons.js');
    const { intern } = await import('./symbol.js');

    const expressions = parse(code);
    if (expressions.length === 0) return null;

    let ast;
    if (expressions.length === 1) {
      ast = analyze(expressions[0], this.globalEnv);
    } else {
      // Wrap multiple expressions in begin
      ast = analyze(list(intern('begin'), ...expressions), this.globalEnv);
    }

    return this.runAsync(ast, this.globalEnv, options);
  }
}

