import { Closure, Values } from './values.js';
import { Literal, TailApp, ANS, CTL, ENV, FSTACK } from './ast.js';

/**
 * Unpacks a Values object to its first value for JS interop (Option C).
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
   * @returns {*} The final result of the computation.
   */
  run(ast, env = this.globalEnv, initialStack = []) {
    if (!this.globalEnv) {
      throw new Error("Interpreter global environment is not set. Call setGlobalEnv() first.");
    }

    // The "CPU registers" - see stepables.js for constant definitions
    // ANS (0): answer - holds result of last computation
    // CTL (1): control - holds next AST node or Frame to execute
    // ENV (2): environment - holds current lexical environment
    // FSTACK (3): frame stack - holds continuation frames
    // We use a COPY of the initialStack to avoid mutating the parent's record of it,
    // although frames themselves are shared.
    const registers = [null, ast, env, [...initialStack]];

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
            let result = unpackForJs(registers[ANS]);

            // BOUNDARY CHECK: If result is a Closure, wrap it in a Bridge
            if (result instanceof Closure) {
              return this.createJsBridge(result);
            }

            return result; // Return the value in 'ans'
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
          // We check the constructor name to avoid circular dependency imports if possible,
          // or we can import ContinuationUnwind at top.
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
                // Done
                let result = unpackForJs(registers[ANS]);
                if (result instanceof Closure) {
                  return this.createJsBridge(result);
                }
                return result;
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

          // Catch errors from native JS calls or logic errors
          console.error("Native JavaScript error caught in interpreter:", e);
          // For REPL, we throw the error so it can be caught and printed.
          throw e;
        }
      }
    } finally {
      this.depth--;
    }
  }

  createJsBridge(closure, parentStack = []) {
    return (...jsArgs) => {
      // 1. Convert JS args to Scheme args (literals)
      const argLiterals = jsArgs.map(val => new Literal(val));

      // 2. Create the invocation AST
      const ast = new TailApp(new Literal(closure), argLiterals);

      // 3. Spin up a NEW interpreter instance
      // We pass 'parentStack' + SentinelFrame to the new run.
      // This ensures that if the closure returns normally (popping frames),
      // it hits the SentinelFrame and stops the inner interpreter,
      // instead of continuing into the parent's frames (Double Execution).
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

class SentinelFrame {
  step(registers, interpreter) {
    // We have reached the bottom of the inner run's stack.
    // The result is in registers[ANS].
    // We throw a special signal to break out of interpreter.run immediately.
    throw new SentinelResult(registers[ANS]);
  }
}

class SentinelResult {
  constructor(value) {
    this.value = value;
  }
}
