/**
 * @fileoverview DevToolsDebugIntegration — bridges the interpreter trampoline
 * to Chrome DevTools via probe scripts.
 *
 * This is the core coordinator for the Chrome DevTools debugger integration.
 * It tracks the current Scheme source location and calls the corresponding
 * probe function when the location changes. Since probe functions are
 * source-mapped back to the original Scheme code, Chrome DevTools shows
 * Scheme source, allows setting breakpoints, and supports stepping.
 *
 * Phase 2: Basic maybeHit() with probe calling and env proxy.
 * Phase 4: Async stack tagging via console.createTask — Scheme function
 *          names appear in the native Call Stack under an "Async" separator.
 */

import { createEnvProxy } from './env_proxy.js';

/**
 * Bridges the interpreter trampoline to Chrome DevTools via probe scripts.
 * Tracks current Scheme source location and calls probe functions on change.
 */
export class DevToolsDebugIntegration {
  /**
   * @param {import('./source_registry.js').SchemeSourceRegistry} sourceRegistry
   *   The registry of loaded Scheme sources and their probe functions.
   */
  constructor(sourceRegistry) {
    /**
     * The source registry providing probe function lookup.
     * @type {import('./source_registry.js').SchemeSourceRegistry}
     */
    this.sourceRegistry = sourceRegistry;

    /**
     * Whether DevTools debugging is currently active.
     * When false, maybeHit() is a no-op for zero overhead.
     * @type {boolean}
     */
    this.enabled = false;

    /**
     * Last hit source key ("filename:line") to avoid redundant probe calls.
     * Multi-step expressions that stay on the same line only fire one probe.
     * @type {string|null}
     */
    this.lastHitKey = null;

    /**
     * The environment reference at the last probe hit.
     * Used together with lastHitKey for deduplication: same line + same env
     * is skipped (sub-expressions within one expression), but same line +
     * different env fires (e.g., re-entry via recursive call).
     * @type {Object|null}
     */
    this.lastHitEnv = null;

    /**
     * Cached environment proxy for the current environment.
     * Reused when the environment reference hasn't changed.
     * @type {Proxy|null}
     */
    this.currentEnvProxy = null;

    /**
     * The environment object the cached proxy was built for.
     * Used to detect when a new proxy needs to be created.
     * @type {Object|null}
     */
    this.currentEnvRef = null;

    /**
     * Count of probe calls made (for testing/diagnostics).
     * @type {number}
     */
    this.hitCount = 0;

    /**
     * History of hit keys for testing/diagnostics.
     * Only populated when tracking is enabled via enableTracking().
     * @type {string[]|null}
     */
    this._hitHistory = null;

    // ----- Phase 4: Async Stack Tagging -----

    /**
     * Whether console.createTask is available (Chrome 106+).
     * When false, frame hooks are lightweight no-ops (no task objects).
     * @type {boolean}
     */
    this.hasCreateTask = typeof console !== 'undefined'
      && typeof console.createTask === 'function';

    /**
     * Stack of async tasks mirroring the StackTracer frame stack.
     * Each entry is { task: Object|null, name: string }.
     * When hasCreateTask is true, task is a console.createTask() result;
     * otherwise it is null (stack is still tracked for diagnostics).
     * @type {Array<{task: Object|null, name: string}>}
     */
    this.taskStack = [];
  }

  /**
   * Called by the trampoline on every step that has source info.
   * Only calls the probe when the source location actually changes.
   *
   * @param {Object} source - Source location: { filename, line, column }
   * @param {Object} env - Current Scheme environment
   */
  maybeHit(source, env) {
    const key = `${source.filename}:${source.line}:${source.column}`;

    // Skip if same location AND same environment as last hit.
    // Same location + same env = sub-expressions within one expression (skip).
    // Same location + different env = re-entry via recursive call (fire).
    // Different location = new location (fire).
    if (key === this.lastHitKey && env === this.lastHitEnv) return;
    this.lastHitKey = key;
    this.lastHitEnv = env;

    // Resolve exactly which expression ID we are hitting
    const exprId = this.sourceRegistry.getExprId(source.filename, source.line, source.column);
    if (!exprId) return;

    // Look up the probe function for this expression ID
    const probe = this.sourceRegistry.getProbe(source.filename, exprId);
    if (!probe) return;

    // Build or reuse environment proxy
    if (env !== this.currentEnvRef) {
      this.currentEnvProxy = createEnvProxy(env);
      this.currentEnvRef = env;
    }

    // Track hit for diagnostics
    this.hitCount++;
    if (this._hitHistory) {
      this._hitHistory.push(key);
    }

    // Call the probe via fireProbe to wrap with async task chain
    this.fireProbe(probe, this.currentEnvProxy);
  }

  // =========================================================================
  // Phase 4: Async Stack Tagging
  // =========================================================================

  /**
   * Called when the interpreter enters a Scheme function.
   * Creates an async task for this call frame so that Chrome DevTools
   * shows the function name in the Call Stack under "Async".
   *
   * @param {Object} frameInfo - Frame information
   * @param {string} frameInfo.name - Internal function name
   * @param {string} [frameInfo.originalName] - Original user-facing name
   * @param {Object|null} frameInfo.source - Source location
   */
  onEnterFrame(frameInfo) {
    const displayName = frameInfo.originalName || frameInfo.name || 'anonymous';
    const task = this.hasCreateTask
      ? console.createTask(`scheme: ${displayName}`)
      : null;
    this.taskStack.push({ task, name: displayName });
  }

  /**
   * Called when the interpreter exits a Scheme function.
   * Pops the corresponding task from the stack.
   */
  onExitFrame() {
    if (this.taskStack.length > 0) {
      this.taskStack.pop();
    }
  }

  /**
   * Called on tail call optimization — replaces the current task
   * with a new one for the tail-called function.
   *
   * @param {Object} frameInfo - New frame information
   */
  onReplaceFrame(frameInfo) {
    if (this.taskStack.length > 0) {
      const displayName = frameInfo.originalName || frameInfo.name || 'anonymous';
      const task = this.hasCreateTask
        ? console.createTask(`scheme: ${displayName}`)
        : null;
      this.taskStack[this.taskStack.length - 1] = { task, name: displayName };
    } else {
      // No frame to replace — fall back to enter
      this.onEnterFrame(frameInfo);
    }
  }

  /**
   * Fires a probe function, wrapping it in the current async task chain
   * so that Chrome DevTools links the probe hit to the Scheme call stack.
   *
   * @param {Function} probe - The probe function to call
   * @param {Proxy} envProxy - The environment proxy
   */
  fireProbe(probe, envProxy) {
    if (this.taskStack.length > 0 && this.hasCreateTask) {
      // Run inside the innermost task to create the async stack link
      const { task } = this.taskStack[this.taskStack.length - 1];
      if (task) {
        task.run(() => probe(envProxy));
        return;
      }
    }
    // No tasks or no createTask — call directly
    probe(envProxy);
  }

  // =========================================================================
  // Enable / Disable / Tracking
  // =========================================================================

  /**
   * Enables DevTools debugging. Called when DevTools is detected or
   * explicitly enabled by the user.
   */
  enable() {
    this.enabled = true;
  }

  /**
   * Disables DevTools debugging. Removes all overhead from the hot loop.
   */
  disable() {
    this.enabled = false;
    this.lastHitKey = null;
    this.lastHitEnv = null;
    this.currentEnvProxy = null;
    this.currentEnvRef = null;
    this.taskStack = [];
  }

  /**
   * Resets the last hit key. Should be called at the start of each
   * top-level evaluation to ensure the first line always fires a probe.
   */
  resetHitTracking() {
    this.lastHitKey = null;
    this.lastHitEnv = null;
  }

  /**
   * Enables hit history tracking for testing.
   * Call before running code to capture the sequence of probe hits.
   */
  enableTracking() {
    this._hitHistory = [];
    this.hitCount = 0;
  }

  /**
   * Disables hit history tracking and returns the recorded history.
   *
   * @returns {string[]} The recorded hit keys
   */
  disableTracking() {
    const history = this._hitHistory || [];
    this._hitHistory = null;
    return history;
  }

  /**
   * Gets the recorded hit history (without disabling tracking).
   *
   * @returns {string[]} The recorded hit keys, or empty array
   */
  getHitHistory() {
    return this._hitHistory || [];
  }
}
