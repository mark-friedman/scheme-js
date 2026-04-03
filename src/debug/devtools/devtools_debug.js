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
 * Provides maybeHit() with probe calling and env proxy, plus async stack
 * tagging via console.createTask — Scheme function names appear in the
 * native Call Stack under an "Async" separator.
 */

import { createEnvProxy } from './env_proxy.js';
import { parse as _defaultParse } from '../../core/interpreter/reader.js';
import { analyze as _defaultAnalyze } from '../../core/interpreter/analyzer.js';
import { list as _defaultList } from '../../core/interpreter/cons.js';
import { intern as _defaultIntern } from '../../core/interpreter/symbol.js';
import { prettyPrint as _defaultPrettyPrint } from '../../core/interpreter/printer.js';
import { SchemeError } from '../../core/interpreter/errors.js';
import { SchemeSourceRegistry } from './source_registry.js';
import { installSchemeDebugAPI } from './scheme_debug_api.js';

/**
 * A wrapper exception thrown to trigger Chrome DevTools' "Pause on exceptions".
 * Extends SchemeError to remain compatible with the interpreter's error handling.
 */
export class SchemeRuntimeException extends SchemeError {
  /**
   * @param {Error|SchemeError|string} schemeError - The original error or message
   * @param {Object} source - The source location where the error occurred
   */
  constructor(schemeError, source) {
    const message = schemeError.message || String(schemeError);
    const sourceLoc = source ? ` at ${source?.filename}:${source?.line}` : '';
    super(`Scheme Error: ${message}${sourceLoc}`, [], 'raise');
    this.name = 'SchemeRuntimeException';
    this.schemeValue = schemeError;
    this.schemeSource = source;
  }
}

/**
 * Bridges the interpreter trampoline to Chrome DevTools via probe scripts.
 * Tracks current Scheme source location and calls probe functions on change.
 */
export class DevToolsDebugIntegration {
  /**
   * @param {import('./source_registry.js').SchemeSourceRegistry} sourceRegistry
   *   The registry of loaded Scheme sources and their probe functions.
   * @param {Object} [interpreterUtils] - Core interpreter utilities from the shared
   *   module instance. When provided, these override the module-level defaults,
   *   ensuring that `analyze` (and thus `ImportNode`) captures the correctly
   *   configured `loadLibrarySync` from the shared bundle rather than a bundled
   *   copy where `fileResolver` is null.
   * @param {Function} [interpreterUtils.parse]
   * @param {Function} [interpreterUtils.analyze]
   * @param {Function} [interpreterUtils.list]
   * @param {Function} [interpreterUtils.intern]
   * @param {Function} [interpreterUtils.prettyPrint]
   */
  constructor(sourceRegistry, { parse, analyze, list, intern, prettyPrint } = {}) {
    /**
     * The source registry providing probe function lookup.
     * @type {import('./source_registry.js').SchemeSourceRegistry}
     */
    this.sourceRegistry = sourceRegistry;

    /**
     * Core interpreter utilities — parse, analyze, list, intern.
     * Injected via constructor to ensure the shared bundle instance is used in
     * production (avoiding the null-fileResolver problem in rollup splits).
     * Falls back to the module-level imports for backwards compatibility in tests.
     * @private
     */
    this._parse = parse || _defaultParse;
    this._analyze = analyze || _defaultAnalyze;
    this._list = list || _defaultList;
    this._intern = intern || _defaultIntern;
    this._prettyPrint = prettyPrint || _defaultPrettyPrint;

    /**
     * Whether DevTools debugging is currently active.
     * When false, maybeHit() is a no-op for zero overhead.
     * @type {boolean}
     */
    this.enabled = false;

    /**
     * Last hit source key ("filename:line:column") to avoid redundant probe calls.
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

    // ----- Phase 7: REPL & Library Registration -----

    /**
     * Counter for generating unique REPL eval source IDs.
     * Increments monotonically across disable/enable cycles to avoid
     * URL collisions with persisted breakpoints.
     * @type {number}
     */
    this._replEvalCounter = 0;

    /**
     * Maps breakpoint ID → Set of probe exprIds registered for it.
     * Used to remove the corresponding probe breakpoints when a
     * BreakpointManager breakpoint is removed.
     * @type {Map<string, Set<number>>}
     */
    this._probeBreakpointMap = new Map();

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
  // Phase 6: Exception Handling & Stepping Control
  // =========================================================================

  /**
   * Called when the interpreter catches a Scheme exception.
   * Pauses at the Scheme source location via the probe mechanism (debugger;),
   * then returns so the trampoline can continue with normal exception handling.
   *
   * Previously this threw a SchemeRuntimeException, but that escaped the
   * trampoline's catch block, broke Scheme exception handlers, and caused
   * DevTools to pause in interpreter JS code instead of Scheme source.
   *
   * @param {*} exception - The Scheme exception value or Error
   * @param {Object} source - The source location where it was raised
   * @param {import('../../core/interpreter/environment.js').Environment} env - The environment
   */
  onException(exception, source, env) {
    if (!this.enabled || !source) return;

    // Resolve exprId to fire probe at exception location
    const exprId = this.sourceRegistry.getExprId(source.filename, source.line, source.column);
    if (!exprId) return;

    const probe = this.sourceRegistry.getProbe(source.filename, exprId);
    if (!probe) return;

    // Use or create env proxy
    if (env !== this.currentEnvRef) {
      this.currentEnvProxy = createEnvProxy(env);
      this.currentEnvRef = env;
    }

    // Set force-pause flag so the probe's hit() returns true and debugger; fires.
    // This pauses V8 at the Scheme source location (via the source-mapped probe),
    // not at the interpreter's throw site.
    if (globalThis.__schemeProbeRuntime) {
      globalThis.__schemeProbeRuntime._exceptionPause = true;
    }

    // Fire the probe — hit() will see _exceptionPause and trigger debugger;
    // V8 pauses synchronously here, then resumes when the user continues.
    this.fireProbe(probe, this.currentEnvProxy);

    // Return without throwing — the trampoline continues with its normal
    // exception handling (Scheme exception handlers, propagation, etc.)
  }

  /**
   * Aborts any active stepping operation (e.g., Step Into, Step Over).
   * Used when execution takes a non-local jump (like call/cc) that would
   * invalidate the stepping stop conditions.
   */
  abortStepping() {
    if (globalThis.__schemeProbeRuntime && typeof globalThis.__schemeProbeRuntime.abortStepping === 'function') {
      globalThis.__schemeProbeRuntime.abortStepping();
    }
  }

  // =========================================================================
  // Phase 7: REPL & Library Source Registration
  // =========================================================================

  /**
   * Returns the next unique REPL source URL and increments the counter.
   * URL follows the convention `scheme://repl/eval-N.scm`.
   *
   * @returns {string} A unique REPL source URL
   */
  getNextReplSourceId() {
    return `scheme://repl/eval-${this._replEvalCounter++}.scm`;
  }

  /**
   * Registers a REPL eval input as a debuggable source.
   * Parses the code with a unique `scheme://repl/eval-N.scm` URL,
   * registers it with the source registry, and generates probe scripts.
   *
   * @param {string} code - The Scheme source code entered in the REPL
   * @returns {{sourceId: string, expressions: Array}} The assigned URL and parsed expressions
   */
  registerReplEval(code) {
    const sourceId = this.getNextReplSourceId();
    const expressions = this._parse(code, { filename: sourceId });
    this.sourceRegistry.register(sourceId, code, 'repl', expressions);
    return { sourceId, expressions };
  }

  /**
   * Registers a library source file for DevTools debugging.
   * Parses the code and generates probe scripts so that library code
   * is visible and debuggable in the Sources panel.
   *
   * @param {string} url - Canonical URL (e.g., 'scheme://lib/scheme/base.sld')
   * @param {string} code - The library source code
   */
  registerLibrarySource(url, code) {
    const expressions = this._parse(code, { filename: url });
    this.sourceRegistry.register(url, code, 'library', expressions);
  }

  /**
   * Converts a library name array to a canonical DevTools URL.
   *
   * @param {string[]} libraryName - Library name parts (e.g., ['scheme', 'base'])
   * @returns {string} Canonical URL (e.g., 'scheme://lib/scheme/base.sld')
   */
  libraryNameToUrl(libraryName) {
    return `scheme://lib/${libraryName.join('/')}.sld`;
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
  // Phase 5: __schemeDebug Global API
  // =========================================================================

  /**
   * Installs the `__schemeDebug` global API on `globalThis`.
   *
   * Delegates to the extracted `installSchemeDebugAPI` function in
   * `scheme_debug_api.js`, passing the interpreter, source registry,
   * probe breakpoint map, and interpreter utilities.
   *
   * @param {import('../../core/interpreter/interpreter.js').Interpreter} interpreter
   *   The interpreter instance to inspect.
   */
  installSchemeDebugAPI(interpreter) {
    installSchemeDebugAPI(interpreter, this.sourceRegistry, this._probeBreakpointMap, {
      parse: this._parse,
      analyze: this._analyze,
      list: this._list,
      intern: this._intern,
      prettyPrint: this._prettyPrint,
    });
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
