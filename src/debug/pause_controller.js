/**
 * @fileoverview PauseController for the Scheme debugger.
 *
 * Manages debugger pause state, stepping logic, and cooperative polling.
 * Tracks whether execution is running, paused, or stepping.
 * Provides a flag-based yield model for cooperative multitasking with
 * adaptive yield intervals (base and emergency).
 *
 * Supports nested pauses via a LIFO resolver stack: each waitForResume()
 * pushes a resolver; resume/step commands pop the topmost one, enabling
 * nested debug levels (e.g. eval during breakpoint).
 */

/**
 * Controls debugger pause state, stepping, and cooperative polling.
 */
export class PauseController {
  constructor() {
    /** @type {'running'|'paused'|'stepping'} */
    this.state = 'running';

    /** @type {'into'|'over'|'out'|null} */
    this.stepMode = null;

    /** @type {number|null} */
    this.targetDepth = null;

    /** @type {string|null} */
    this.pauseReason = null;

    /** @type {*} */
    this.pauseData = null;

    /**
     * Stack of resolver functions for nested pause promises (LIFO).
     * Each waitForResume() call pushes a resolver. _resolveWait() pops the
     * topmost one, enabling nested debug levels (e.g. eval during breakpoint).
     * @type {Function[]}
     */
    this.pauseResolveStack = [];

    /**
     * Stack of saved execution contexts for nested runDebug() calls.
     * Each entry stores {state, stepMode, targetDepth, pauseReason, pauseData}
     * so that inner evals don't contaminate outer stepping state.
     * @type {Array<{state: string, stepMode: string|null, targetDepth: number|null, pauseReason: string|null, pauseData: *}>}
     */
    this.executionContextStack = [];

    /** @type {boolean} */
    this.aborted = false;

    // Cooperative polling fields

    /**
     * Whether a pause has been requested externally (e.g. by the user pressing
     * a pause button). Checked at each yield point.
     * @type {boolean}
     */
    this.pauseRequested = false;

    /**
     * Whether all execution should be aborted back to the top level.
     * Used by the :toplevel command.
     * @type {boolean}
     */
    this.abortAll = false;

    /**
     * Operation counter for cooperative yield scheduling.
     * Incremented by shouldYield(), reset by onYield().
     * @type {number}
     */
    this.opCount = 0;

    /**
     * Default yield interval (number of operations between yield points).
     * @type {number}
     */
    this.baseYieldInterval = 10000;

    /**
     * Reduced yield interval used when a pause has been requested,
     * to respond more quickly.
     * @type {number}
     */
    this.emergencyYieldInterval = 100;

    /**
     * Current yield interval. Starts at baseYieldInterval and switches
     * to emergencyYieldInterval when requestPause() is called.
     * @type {number}
     */
    this.currentYieldInterval = this.baseYieldInterval;
  }

  // ---- Abort ----

  /**
   * Checks if currently aborted.
   * @returns {boolean}
   */
  isAborted() {
    return this.aborted;
  }

  /**
   * Aborts execution.
   */
  abort() {
    this.aborted = true;
    this.resume();
  }

  // ---- State accessors ----

  /**
   * Gets the current state.
   * @returns {'running'|'paused'|'stepping'}
   */
  getState() {
    return this.state;
  }

  /**
   * Gets the current step mode.
   * @returns {'into'|'over'|'out'|null}
   */
  getStepMode() {
    return this.stepMode;
  }

  /**
   * Gets the target depth for step over/out.
   * @returns {number|null}
   */
  getTargetDepth() {
    return this.targetDepth;
  }

  /**
   * Gets the reason for current pause.
   * @returns {string|null}
   */
  getPauseReason() {
    return this.pauseReason;
  }

  /**
   * Gets additional data about the pause (e.g., breakpoint ID).
   * @returns {*}
   */
  getPauseData() {
    return this.pauseData;
  }

  /**
   * Checks if currently paused.
   * @returns {boolean}
   */
  isPaused() {
    return this.state === 'paused';
  }

  // ---- Pause / Resume ----

  /**
   * Pauses execution.
   * @param {string} [reason] - Reason for pause (e.g., 'breakpoint', 'step', 'exception')
   * @param {*} [data] - Additional data (e.g., breakpoint ID)
   */
  pause(reason = null, data = null) {
    this.state = 'paused';
    this.pauseReason = reason;
    this.pauseData = data;
    this.stepMode = null;
    this.targetDepth = null;
  }

  /**
   * Resumes execution.
   * Clears pause state, pauseRequested flag, and resets yield interval.
   * If there's a pending pause promise, resolves it.
   */
  resume() {
    this._resolveWait();

    this.state = 'running';
    this.stepMode = null;
    this.targetDepth = null;
    this.pauseReason = null;
    this.pauseData = null;
    this.pauseRequested = false;
    this.currentYieldInterval = this.baseYieldInterval;
  }

  /**
   * Resolves the topmost pending waitForResume promise (LIFO).
   * Called by resume and step methods to unblock the trampoline.
   * Only resolves one level — outer waits remain pending.
   * @private
   */
  _resolveWait() {
    if (this.pauseResolveStack.length > 0) {
      const resolve = this.pauseResolveStack.pop();
      resolve();
    }
  }

  /**
   * Returns a promise that resolves when resume() or a step command is called.
   * Pushes a resolver onto the stack so nested pauses don't orphan
   * outer waiters. Each resume/step resolves the topmost (LIFO).
   * @returns {Promise<void>}
   */
  waitForResume() {
    if (!this.isPaused()) {
      return Promise.resolve();
    }
    return new Promise(resolve => {
      this.pauseResolveStack.push(resolve);
    });
  }

  // ---- Stepping ----

  /**
   * Step into: pause at the next expression.
   */
  stepInto() {
    this.state = 'stepping';
    this.stepMode = 'into';
    this.targetDepth = null;
    this.pauseReason = null;
    this.pauseData = null;
    this._resolveWait();
  }

  /**
   * Step over: pause at the next expression at same or shallower depth.
   * @param {number} currentDepth - Current stack depth
   */
  stepOver(currentDepth) {
    this.state = 'stepping';
    this.stepMode = 'over';
    this.targetDepth = currentDepth;
    this.pauseReason = null;
    this.pauseData = null;
    this._resolveWait();
  }

  /**
   * Step out: pause at a shallower depth (after returning from current function).
   * @param {number} currentDepth - Current stack depth
   */
  stepOut(currentDepth) {
    this.state = 'stepping';
    this.stepMode = 'out';
    this.targetDepth = currentDepth;
    this.pauseReason = null;
    this.pauseData = null;
    this._resolveWait();
  }

  /**
   * Determines if execution should pause at the current position.
   * @param {number} currentDepth - Current stack depth
   * @returns {boolean} True if should pause
   */
  shouldStepPause(currentDepth) {
    if (this.state !== 'stepping') {
      return false;
    }

    switch (this.stepMode) {
      case 'into':
        // Step into: always pause at next expression
        return true;

      case 'over':
        // Step over: pause at same or shallower depth
        return currentDepth <= this.targetDepth;

      case 'out':
        // Step out: pause only at shallower depth
        return currentDepth < this.targetDepth;

      default:
        return false;
    }
  }

  // ---- Cooperative Polling ----

  /**
   * Increments the operation counter and returns true when the current
   * yield interval has been reached. The interpreter should call this
   * on every operation and yield (await) when it returns true.
   * @returns {boolean} True when the interpreter should yield
   */
  shouldYield() {
    this.opCount++;
    return this.opCount >= this.currentYieldInterval;
  }

  /**
   * Resets the operation counter after a yield. Call this after
   * the interpreter has yielded control back to the event loop.
   */
  onYield() {
    this.opCount = 0;
  }

  /**
   * Requests a pause from outside the interpreter loop. Sets the
   * pauseRequested flag and switches to the emergency yield interval
   * so the interpreter yields control quickly.
   */
  requestPause() {
    this.pauseRequested = true;
    this.currentYieldInterval = this.emergencyYieldInterval;
  }

  // ---- Execution Context Stack ----

  /**
   * Saves current stepping state and resets to defaults.
   * Called when entering a nested eval so that inner stepping
   * commands don't contaminate the outer level.
   * Abort flags and yield counters are intentionally NOT saved — they
   * remain global so abort propagates across all levels.
   */
  pushExecutionContext() {
    this.executionContextStack.push({
      state: this.state,
      stepMode: this.stepMode,
      targetDepth: this.targetDepth,
      pauseReason: this.pauseReason,
      pauseData: this.pauseData
    });
    this.state = 'running';
    this.stepMode = null;
    this.targetDepth = null;
    this.pauseReason = null;
    this.pauseData = null;
  }

  /**
   * Restores the most recently saved stepping state.
   * Called when returning from a nested eval.
   */
  popExecutionContext() {
    if (this.executionContextStack.length === 0) return;
    const ctx = this.executionContextStack.pop();
    this.state = ctx.state;
    this.stepMode = ctx.stepMode;
    this.targetDepth = ctx.targetDepth;
    this.pauseReason = ctx.pauseReason;
    this.pauseData = ctx.pauseData;
  }

  // ---- Reset ----

  /**
   * Resets to initial state. Clears all pause, stepping, abort,
   * cooperative polling, promise, and execution context state.
   */
  reset() {
    // Resolve all pending pause promises (allows clean shutdown)
    while (this.pauseResolveStack.length > 0) {
      const resolve = this.pauseResolveStack.pop();
      resolve();
    }

    this.state = 'running';
    this.stepMode = null;
    this.targetDepth = null;
    this.pauseReason = null;
    this.pauseData = null;
    this.aborted = false;
    this.pauseRequested = false;
    this.abortAll = false;
    this.opCount = 0;
    this.currentYieldInterval = this.baseYieldInterval;
    this.executionContextStack = [];
  }
}
