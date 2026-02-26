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
import { parse } from '../../core/interpreter/reader.js';
import { analyze } from '../../core/interpreter/analyzer.js';
import { list } from '../../core/interpreter/cons.js';
import { intern } from '../../core/interpreter/symbol.js';
import { SchemeError } from '../../core/interpreter/errors.js';

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
   * This API is the bridge between the Chrome DevTools extension (running
   * in extension context) and the interpreter's internal state (running in
   * page context). The extension calls these methods via
   * `chrome.devtools.inspectedWindow.eval()`.
   *
   * @param {import('../../core/interpreter/interpreter.js').Interpreter} interpreter
   *   The interpreter instance to inspect.
   */
  installSchemeDebugAPI(interpreter) {
    globalThis.__schemeDebug = {
      /**
       * Gets the current Scheme call stack.
       * @returns {Array<{name: string, source: {filename: string, line: number, column: number}|null, tcoCount: number}>}
       */
      getStack() {
        const frames = interpreter.debugRuntime?.stackTracer.getStack() || [];
        return frames.map(f => ({
          name: f.name,
          source: f.source || null,
          tcoCount: f.tcoCount || 0
        }));
      },

      /**
       * Gets the local variable bindings for a specific stack frame.
       * @param {number} frameIndex - Index into the stack (0 = bottom, length-1 = top)
       * @returns {Array<{name: string, value: string, type: string, subtype: string|null}>}
       */
      getLocals(frameIndex) {
        const frames = interpreter.debugRuntime?.stackTracer.getStack() || [];
        if (frameIndex < 0 || frameIndex >= frames.length) return [];
        const env = frames[frameIndex].env;
        const inspector = interpreter.debugRuntime?.stateInspector;
        if (!env || !inspector) return [];

        const locals = inspector.getLocals(env);
        const result = [];
        for (const [name, value] of locals) {
          const serialized = inspector.serializeValue(value);
          result.push({
            name,
            value: serialized.description || String(value),
            type: serialized.type,
            subtype: serialized.subtype || null
          });
        }
        return result;
      },

      /**
       * Gets source info for a specific stack frame.
       * @param {number} frameIndex
       * @returns {{filename: string, line: number, column: number}|null}
       */
      getSource(frameIndex) {
        const frames = interpreter.debugRuntime?.stackTracer.getStack() || [];
        if (frameIndex < 0 || frameIndex >= frames.length) return null;
        return frames[frameIndex].source || null;
      },

      /**
       * Evaluates a Scheme expression in the context of a specific stack frame.
       * Uses the synchronous run() path (parse → analyze → run).
       * @param {string} code - Scheme expression to evaluate
       * @param {number} [frameIndex] - Frame index (defaults to top frame)
       * @returns {string} Result as a string
       */
      eval(code, frameIndex) {
        const frames = interpreter.debugRuntime?.stackTracer.getStack() || [];
        const idx = frameIndex ?? (frames.length - 1);
        if (idx < 0 || idx >= frames.length) return '#<error: invalid frame>';
        const env = frames[idx].env;
        try {
          // Use synchronous path: parse → analyze → run
          const expressions = parse(code);
          if (expressions.length === 0) return '';

          let ast;
          if (expressions.length === 1) {
            ast = analyze(expressions[0]);
          } else {
            ast = analyze(list(intern('begin'), ...expressions));
          }

          const result = interpreter.run(ast, env);
          return String(result);
        } catch (e) {
          return `#<error: ${e.message}>`;
        }
      },

      /**
       * Issue a Step Into command.
       * V8 will pause at the next distinct Scheme expression.
       */
      stepInto() {
        if (globalThis.__schemeProbeRuntime) {
          globalThis.__schemeProbeRuntime.stepInto();
        }
      },

      /**
       * Issue a Step Over command.
       * V8 will pause at the next expression at the same or shallower depth.
       */
      stepOver() {
        if (globalThis.__schemeProbeRuntime) {
          globalThis.__schemeProbeRuntime.stepOver();
        }
      },

      /**
       * Issue a Step Out command.
       * V8 will pause at the first expression after the current function returns.
       */
      stepOut() {
        if (globalThis.__schemeProbeRuntime) {
          globalThis.__schemeProbeRuntime.stepOut();
        }
      }
    };
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
