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
          return registers[0]; // Return the value in 'ans'
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
