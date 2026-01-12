import { Values, isSchemeClosure } from './values.js';
import { LiteralNode, TailAppNode, ANS, CTL, ENV, FSTACK, ExceptionHandlerFrame, RaiseNode } from './ast.js';
import { SchemeError } from './errors.js';

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

/**
 * Unpacks a Values object to its first value for JS interop.
 * @param {*} result - The result to unpack
 * @returns {*} First value if Values, otherwise unchanged
 */
function unpackForJs(result) {
  if (result instanceof Values) {
    return result.first();
  }
  return result;
}

/**
 * SentinelFrame is pushed onto the stack when calling Scheme from JS.
 * When control returns to this frame, it throws SentinelResult to
 * break out of the nested interpreter.run() call.
 */
class SentinelFrame {
  step(registers, interpreter) {
    // We have reached the bottom of the inner run's stack.
    // The result is in registers[ANS].
    // We throw a special signal to break out of interpreter.run immediately.
    throw new SentinelResult(registers[ANS]);
  }
}

/**
 * SentinelResult is thrown to signal normal completion of a nested run.
 */
class SentinelResult {
  constructor(value) {
    this.value = value;
  }
}

/**
 * The core Scheme interpreter.
 * Manages the top-level trampoline loop and register state.
 */
export class Interpreter {
  constructor() {
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
   * @returns {*} The final result of the computation.
   */
  run(ast, env = this.globalEnv, initialStack = [], thisContext = undefined) {
    if (!this.globalEnv) {
      throw new Error("Interpreter global environment is not set. Call setGlobalEnv() first.");
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
            return unpackForJs(registers[ANS]);
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
                return unpackForJs(registers[ANS]);
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
            return unpackForJs(e.value);
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
          console.error("Native JavaScript error caught in interpreter:", e);
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
  runWithSentinel(ast, thisContext = undefined) {
    // Get the parent context (the Scheme stack at the point where we entered JS)
    const parentContext = this.getParentContext();
    const stackWithSentinel = [...parentContext, new SentinelFrame()];
    return this.run(ast, this.globalEnv, stackWithSentinel, thisContext);
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
   * Creates a JS-callable wrapper for a Scheme closure.
   * 
   * @deprecated Closures are now created as callable functions directly.
   *             This method is kept for backward compatibility.
   * @param {Function} closure - A Scheme closure (callable function with marker).
   * @param {Array} parentStack - The parent frame stack for context.
   * @returns {Function} A callable wrapper (or the closure itself if already callable).
   */
  createJsBridge(closure, parentStack = []) {
    // If it's already a callable Scheme closure, it can be called directly.
    // This method is now essentially a passthrough for new-style closures.
    if (isSchemeClosure(closure)) {
      return closure;
    }

    // Legacy path for old-style Closure instances (during migration)
    return (...jsArgs) => {
      const argLiterals = jsArgs.map(val => new LiteralNode(val));
      const ast = new TailAppNode(new LiteralNode(closure), argLiterals);
      const stackWithSentinel = [...parentStack, new SentinelFrame()];
      return this.run(ast, this.globalEnv, stackWithSentinel);
    };
  }

  /**
   * Executes a single step of the computation.
   * This polymorphically calls the `step` method on the `ctl` object.
   * @param {Array} registers - The [ans, ctl, env, fstack] registers array.
   * @returns {boolean} `true` to continue the trampoline, `false` to halt.
   */
  step(registers) {
    const ctl = registers[CTL];
    return ctl.step(registers, this);
  }
}
