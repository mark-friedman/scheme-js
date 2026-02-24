import { Values, isSchemeClosure } from './values.js';
import { LiteralNode, TailAppNode, ANS, CTL, ENV, FSTACK, ExceptionHandlerFrame, RaiseNode } from './ast.js';
import { SchemeError } from './errors.js';
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
 * converts within vectors, records and objects
 *
 * @param {*} result - The result to unpack
 * @param {Interpreter} interpreter - The interpreter instance
 * @param {Object} [options={}] - Conversion options (passed to schemeToJsDeep)
 * @returns {*} Converted value
 */
function unpackForJs(result, interpreter, options = {}) {
  if (result instanceof Values) {
    result = result.first();
  }

  // Determine conversion mode. Priority:
  // 1. Explicit option passed to run()
  // 2. Global interpreter setting
  // 3. Default ('deep')
  const mode = options.jsAutoConvert ?? (interpreter?.jsAutoConvert ?? 'deep');

  if (mode === 'raw') {
    return result;
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
     * Optional Chrome DevTools debug integration.
     * When set and enabled, the interpreter calls probe functions on source location
     * changes, allowing DevTools to show Scheme source and support breakpoints.
     * @type {import('../../debug/devtools/devtools_debug.js').DevToolsDebugIntegration|null}
     */
    this.devtoolsDebug = null;
  }


  /**
   * Pushes the current Scheme context before calling into JS.
   * @param {Array} fstack - The current frame stack.
   */
  pushJsContext(fstack) {
    this.jsContextStack.push([...fstack]);
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
      return this.jsContextStack[this.jsContextStack.length - 1];
    }
    return [];
  }

  /**
   * Sets the global environment. Required before running.
   * @param {Environment} env The global environment, pre-filled with primitives.
   */
  setGlobalEnv(env) {
    this.globalEnv = env;
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

    // Track recursion depth
    this.depth++;

    // Cache DevTools reference for hot loop (null check is cheaper than property access)
    const devtoolsDebug = this.devtoolsDebug;

    // The Top-Level Trampoline
    try {
      while (true) {
        try {
          // DevTools probe check (only when enabled, only on source change)
          if (devtoolsDebug?.enabled) {
            const src = registers[CTL]?.source;
            if (src) {
              devtoolsDebug.maybeHit(src, registers[ENV]);
            }
          }

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
            return unpackForJs(registers[ANS], this, options);
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
          // Check for Continuation Unwind
          // We check the constructor name to avoid circular dependency imports if possible.
          if (e.constructor.name === 'ContinuationUnwind') {
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
                return unpackForJs(registers[ANS], this, options);
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
            return unpackForJs(e.value, this, options);
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
    const stackWithSentinel = [...parentContext, new SentinelFrame()];
    return this.run(ast, this.globalEnv, stackWithSentinel, thisContext, options);
  }

  /**
   * Invokes a captured continuation from JavaScript.
   * This is called when JS code invokes a callable continuation.
   *
   * @param {Function} continuation - The callable continuation (with fstack attached).
   * @param {*} value - The value to pass to the continuation.
   * @param {*} [thisContext] - The value for the 'this' register.
   * @returns {*} The result of invoking the continuation.
   */
  invokeContinuation(continuation, value, thisContext = undefined) {
    // Build an AST that invokes the continuation
    const ast = new TailAppNode(
      new LiteralNode(continuation),
      [new LiteralNode(value)]
    );

    // Run with sentinel and parent context
    return this.runWithSentinel(ast, thisContext);
  }



  /**
   * Executes a single step of the computation.
   * This polymorphically calls the `step` method on the `ctl` object.
   * @param {Array} registers - The [ans, ctl, env, fstack] registers array.
   * @returns {boolean} `true` to continue the trampoline, `false` to halt.
   */
  step(registers) {
    return registers[CTL].step(registers, this);
  }

  /**
   * Sets the debug runtime for this interpreter.
   * @param {import('../../debug/scheme_debug_runtime.js').SchemeDebugRuntime|null} debugRuntime
   */
  setDebugRuntime(debugRuntime) {
    this.debugRuntime = debugRuntime;
  }

  /**
   * Runs Scheme code asynchronously with cooperative yielding and debug support.
   * This is the single async execution path that replaces runAsync().
   * When a debugRuntime is attached and enabled, it checks for breakpoints,
   * stepping, and manual pause at each yield point. When no debugRuntime is
   * present, it still yields to the event loop periodically for UI responsiveness.
   *
   * @param {Executable} ast - The AST node to execute.
   * @param {Environment} [env] - The environment to run in.
   * @param {Object} [options={}] - Execution options.
   * @param {number} [options.stepsPerYield=1000] - Steps between yields (used when no debugRuntime).
   * @param {Function} [options.onYield] - Callback invoked on each yield.
   * @returns {Promise<*>} The result of the computation.
   */
  async runDebug(ast, env = this.globalEnv, options = {}) {
    if (!this.globalEnv) {
      throw new SchemeError("Interpreter global environment is not set.");
    }

    const registers = [null, ast, env, [], undefined];
    this.depth++;

    const dr = this.debugRuntime;
    const pc = dr?.pauseController;
    const hasDebug = dr && dr.enabled;

    // For nested runDebug() calls (e.g. :eval during a breakpoint pause),
    // isolate the stepping state so inner step commands don't contaminate
    // the outer execution. depth > 1 means this is a nested call.
    const nestedDebug = hasDebug && pc && this.depth > 1;
    if (nestedDebug) {
      pc.pushExecutionContext();
    }

    // Cache DevTools reference for hot loop
    const devtoolsDebugAsync = this.devtoolsDebug;

    // Yield configuration: use pauseController's adaptive yielding when
    // debug is active, otherwise fall back to stepsPerYield option.
    const stepsPerYield = options.stepsPerYield ?? 1000;
    const onYield = options.onYield ?? null;

    try {
      let stepCount = 0;

      while (true) {
        try {
          // --- Cooperative yield check ---
          if (hasDebug && pc) {
            if (pc.shouldYield()) {
              pc.onYield();
              await new Promise(resolve => setTimeout(resolve, 0));

              // Check for manual pause during yield
              if (pc.pauseRequested) {
                pc.pauseRequested = false;
                pc.currentYieldInterval = pc.baseYieldInterval;
                const action = await dr.handlePause(
                  registers[CTL]?.source || null,
                  registers[ENV],
                  'manual'
                );
                if (action === 'abort') {
                  throw new SchemeError("Evaluation aborted");
                }
              }

              // Check for abort-all
              if (pc.abortAll) {
                throw new SchemeError("Evaluation aborted");
              }
            }
          }

          // --- DevTools probe check (source location tracking) ---
          if (devtoolsDebugAsync?.enabled) {
            const src = registers[CTL]?.source;
            if (src) {
              devtoolsDebugAsync.maybeHit(src, registers[ENV]);
            }
          }

          // --- Debug pause check (breakpoints, stepping) ---
          if (hasDebug && registers[CTL]?.source) {
            if (dr.shouldPause(registers[CTL].source, registers[ENV])) {
              const reason = pc.getState() === 'stepping' ? 'step' : 'breakpoint';
              const action = await dr.handlePause(
                registers[CTL].source,
                registers[ENV],
                reason
              );
              if (action === 'abort') {
                throw new SchemeError("Evaluation aborted");
              }
            }
          }

          // --- Execute one step ---
          if (this.step(registers)) {
            stepCount++;

            // Non-debug yield: periodic yield for UI responsiveness
            if (!hasDebug) {
              if (stepCount >= stepsPerYield) {
                stepCount = 0;
                if (onYield) onYield();
                await new Promise(resolve => setTimeout(resolve, 0));
              }
            }
            continue;
          }

          // Step returned false — check frame stack
          const fstack = registers[FSTACK];

          if (fstack.length === 0) {
            return unpackForJs(registers[ANS], this, options);
          }

          registers[CTL] = fstack.pop();
          continue;

        } catch (e) {
          // Check for ContinuationUnwind
          if (e.constructor.name === 'ContinuationUnwind') {
            if (this.depth > 1) throw e;

            const newRegisters = e.registers;
            registers[ANS] = newRegisters[ANS];
            registers[ENV] = newRegisters[ENV];
            registers[FSTACK] = newRegisters[FSTACK];

            if (e.isReturn) {
              const fstack = registers[FSTACK];
              if (fstack.length === 0) {
                return unpackForJs(registers[ANS], this, options);
              }
              registers[CTL] = fstack.pop();
              continue;
            } else {
              registers[CTL] = newRegisters[CTL];
              continue;
            }
          }

          // Check for SentinelResult
          if (e instanceof SentinelResult) {
            return unpackForJs(e.value, this, options);
          }

          // Debug exception handling: check if should break on this exception
          if (hasDebug && dr.exceptionHandler.shouldBreakOnException(e, registers[FSTACK])) {
            // Use CTL source if available; fall back to current stack frame source
            const exceptionSource = registers[CTL]?.source
              || dr.stackTracer.getCurrentFrame()?.source
              || null;
            const action = await dr.handlePause(
              exceptionSource,
              registers[ENV],
              'exception'
            );
            if (action === 'abort') {
              throw new SchemeError("Evaluation aborted");
            }
          }

          // Check for Scheme exception handler
          const handlerIndex = findExceptionHandler(registers[FSTACK]);
          if (handlerIndex !== -1) {
            registers[CTL] = new RaiseNode(wrapJsError(e), false);
            continue;
          }

          throw e;
        }
      }
    } finally {
      if (nestedDebug) {
        pc.popExecutionContext();
      }
      this.depth--;
    }
  }

  /**
   * Evaluates a Scheme code string using the debug-aware async path.
   *
   * When a DevTools integration is active (`this.devtoolsDebug`), each
   * evaluation is assigned a stable `scheme://repl/eval-N.scm` URL (or the
   * caller-supplied `options.sourceUrl`) so that the generated probe script
   * can be registered and source-mapped back to the Scheme code.  Without
   * this, `source.filename` in the trampoline is always `'<unknown>'` and
   * `maybeHit()` never resolves an exprId.
   *
   * @param {string} code - The Scheme source code.
   * @param {Object} [options={}] - Execution options.
   * @param {string} [options.sourceUrl] - Explicit source URL for DevTools registration.
   *   If omitted and devtoolsDebug is active, a URL is auto-generated.
   * @returns {Promise<*>} The result of the computation.
   */
  async evaluateStringDebug(code, options = {}) {
    const { parse } = await import('./reader.js');
    const { analyze } = await import('./analyzer.js');
    const { list } = await import('./cons.js');
    const { intern } = await import('./symbol.js');

    // Determine source URL for DevTools registration (when active)
    let sourceUrl = options.sourceUrl ?? null;
    const devtools = this.devtoolsDebug;
    if (devtools?.enabled && !sourceUrl) {
      // Auto-generate a stable REPL eval URL using a per-interpreter counter
      if (this._replEvalCounter === undefined) this._replEvalCounter = 0;
      sourceUrl = `scheme://repl/eval-${this._replEvalCounter++}.scm`;
    }

    // Parse with filename so AST nodes carry the correct source location
    const expressions = parse(code, sourceUrl ? { filename: sourceUrl } : {});
    if (expressions.length === 0) return null;

    // Register the source with DevTools so probes fire for this eval
    if (devtools?.enabled && sourceUrl) {
      const registry = devtools.sourceRegistry;
      if (registry && !registry.hasSource(sourceUrl)) {
        registry.register(sourceUrl, code, 'repl', expressions);
      }
      // Reset dedup state so the first expression in this eval fires its probe
      devtools.resetHitTracking();
    }

    let ast;
    if (expressions.length === 1) {
      ast = analyze(expressions[0], undefined, this.context);
    } else {
      ast = analyze(list(intern('begin'), ...expressions), undefined, this.context);
    }

    return this.runDebug(ast, this.globalEnv, options);
  }


  /**
   * @deprecated Use runDebug() instead. This is a compatibility alias.
   */
  async runAsync(ast, env = this.globalEnv, options = {}) {
    return this.runDebug(ast, env, options);
  }

  /**
   * @deprecated Use evaluateStringDebug() instead. This is a compatibility alias.
   */
  async evaluateStringAsync(code, options = {}) {
    return this.evaluateStringDebug(code, options);
  }
}

