import { Closure } from './values.js';
import { Literal, TailApp } from './ast.js';

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
  run(ast, env = this.globalEnv) {
    if (!this.globalEnv) {
      throw new Error("Interpreter global environment is not set. Call setGlobalEnv() first.");
    }

    // The "CPU registers"
    // [0] = ans (answer): Holds the result of the last computation.
    // [1] = ctl (control): Holds the next AST node (or Frame) to execute.
    // [2] = env (environment): Holds the current lexical environment.
    // [3] = fstack (frame stack): Holds the continuation frames.
    const registers = [null, ast, env, []];

    // The Top-Level Trampoline
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
        const fstack = registers[3];

        if (fstack.length === 0) {
          // --- Fate #1: Normal Termination ---
          // Stack is empty, computation is done.
          let result = registers[0];

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
        registers[1] = frame;

        // The 'ans' register (registers[0]) already contains
        // the value this frame was waiting for.

        // The 'env' register (registers[2]) is restored
        // by the frame itself in its `step` method.

        // Loop again to execute the frame's `step`
        continue;

      } catch (e) {
        // Catch errors from native JS calls or logic errors
        console.error("Native JavaScript error caught in interpreter:", e);
        // For REPL, we throw the error so it can be caught and printed.
        throw e;
      }
    }
  }

  createJsBridge(closure) {
    return (...jsArgs) => {
      // 1. Convert JS args to Scheme args (literals)
      // We assume shared types (number, string, array) are passed as is.
      // We wrap them in Literal nodes.
      const argLiterals = jsArgs.map(val => new Literal(val));

      // 2. Create the invocation AST
      // (closure ...args)
      const ast = new TailApp(new Literal(closure), argLiterals);

      // 3. Spin up a NEW interpreter instance (or re-enter this one?)
      // Re-entering 'this.run' is safe because it creates a new 'registers' array.
      // We use the global environment as the base, but the closure has its own captured env.
      return this.run(ast, this.globalEnv);
    };
  }

  /**
   * Executes a single step of the computation.
   * This polymorphically calls the `step` method on the `ctl` object.
   * @param {Array} registers - The [ans, ctl, env, fstack] registers array.
   * @returns {boolean} `true` to continue the trampoline, `false` to halt.
   */
  step(registers) {
    const ctl = registers[1];
    return ctl.step(registers, this);
  }
}
