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
import { SchemeError } from '../../core/interpreter/errors.js';
import { SchemeSourceRegistry } from './source_registry.js';

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
   */
  constructor(sourceRegistry, { parse, analyze, list, intern } = {}) {
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
   * This API is the bridge between the Chrome DevTools extension (running
   * in extension context) and the interpreter's internal state (running in
   * page context). The extension calls these methods via
   * `chrome.devtools.inspectedWindow.eval()`.
   *
   * @param {import('../../core/interpreter/interpreter.js').Interpreter} interpreter
   *   The interpreter instance to inspect.
   */
  installSchemeDebugAPI(interpreter) {
    const sourceRegistry = this.sourceRegistry;
    const { _parse, _analyze, _list, _intern } = this;
    globalThis.__schemeDebug = {
      /**
       * Gets the current Scheme call stack.
       * When paused at top-level (no function frames), a synthetic
       * "top-level" frame is included so getStack() always returns at
       * least one frame during a pause.
       * @returns {Array<{name: string, source: {filename: string, line: number, column: number}|null, tcoCount: number}>}
       */
      getStack() {
        const runtime = interpreter.debugRuntime;
        const frames = runtime?.stackTracer.getStack() || [];
        const mapped = frames.map(f => ({
          name: f.name,
          source: f.source || null,
          tcoCount: f.tcoCount || 0
        }));

        // If paused at top-level (no function frames), add a synthetic frame
        // so the stack is never empty during a pause.
        if (mapped.length === 0 && runtime?.isPaused() && runtime._currentPauseEnv) {
          mapped.push({
            name: '<top-level>',
            source: runtime._currentPauseSource || null,
            tcoCount: 0,
            _synthetic: true
          });
        }

        return mapped;
      },

      /**
       * Gets the local variable bindings for a specific stack frame.
       * Supports the synthetic top-level frame created by getStack().
       * @param {number} frameIndex - Index into the stack (0 = bottom, length-1 = top)
       * @returns {Array<{name: string, value: string, type: string, subtype: string|null}>}
       */
      getLocals(frameIndex) {
        const runtime = interpreter.debugRuntime;
        const frames = runtime?.stackTracer.getStack() || [];
        const inspector = runtime?.stateInspector;
        if (!inspector) return [];

        let env;

        if (frames.length === 0 && runtime?.isPaused() && runtime._currentPauseEnv) {
          // Synthetic top-level frame: use the pause environment
          if (frameIndex === 0) {
            env = runtime._currentPauseEnv;
          } else {
            return [];
          }
        } else {
          if (frameIndex < 0 || frameIndex >= frames.length) return [];
          env = frames[frameIndex].env;
        }

        if (!env) return [];

        // When in the global env, filter out system-defined bindings (primitives +
        // standard library procs loaded during bootstrap). html_adapter.js snapshots
        // env._systemBindingNames before running user scripts; any binding whose
        // internal key is in that set was defined by the system, not the user.
        const systemNames = (env.parent === null) ? env._systemBindingNames : null;

        const locals = inspector.getLocals(env);
        const result = [];
        for (const [name, value] of locals) {
          // Skip system-defined names in the global scope
          if (systemNames) {
            const internalName = env.nameMap?.get(name);
            if (systemNames.has(name) || (internalName && systemNames.has(internalName))) {
              continue;
            }
          }
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
       * Gets all registered Scheme sources.
       * Returns an array of { url, content, lines, origin } objects.
       * Used by the DevTools panel to populate the source file list.
       *
       * @returns {Array<{url: string, content: string, lines: number, origin: string}>}
       */
      getSources() {
        return sourceRegistry.getAllSources();
      },

      /**
       * Gets the expression spans for a source URL.
       * Each span describes a single AST expression with its location range,
       * enabling expression-level breakpoints and highlighting in the panel.
       *
       * @param {string} url - The scheme:// URL of the source
       * @returns {Array<{exprId: number, line: number, column: number, endLine: number, endColumn: number}>}
       */
      getExpressions(url) {
        return sourceRegistry.getExpressions(url).map(s => ({
          exprId: s.exprId,
          line: s.line,
          column: s.column,
          endLine: s.endLine,
          endColumn: s.endColumn,
        }));
      },

      /**
       * Gets the content of a specific source URL.
       * @param {string} url - The scheme:// URL of the source
       * @returns {string|null} The source content, or null if not registered
       */
      getSourceContent(url) {
        const info = sourceRegistry.getSourceInfo(url);
        return info ? info.content : null;
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
        // Use the specified frame's env, or fall back to global env for top-level pauses
        let env;
        if (idx >= 0 && idx < frames.length) {
          env = frames[idx].env;
        } else {
          env = interpreter.globalEnv;
        }
        try {
          // Use synchronous path: parse → analyze → run
          const expressions = _parse(code);
          if (expressions.length === 0) return '';

          let ast;
          if (expressions.length === 1) {
            ast = _analyze(expressions[0]);
          } else {
            ast = _analyze(_list(_intern('begin'), ...expressions));
          }

          const result = interpreter.run(ast, env);
          return String(result);
        } catch (e) {
          return `#<error: ${e.message}>`;
        }
      },

      /**
       * Enables debug mode for future page loads.
       * Sets globalThis.__SCHEME_JS_DEBUG so that on next reload, the
       * interpreter is started in debug mode. Returns the current active state.
       *
       * @returns {{active: boolean, needsReload: boolean}}
       */
      activate() {
        globalThis.__SCHEME_JS_DEBUG = true;
        const dr = interpreter.debugRuntime;
        const active = !!(dr?.enabled);
        // Mark the panel as connected so handlePause() blocks (waits for
        // resume commands) instead of auto-resuming.
        if (dr) dr.panelConnected = true;
        // Persist to sessionStorage so activate_debug.js can read it at
        // document_start on the next page reload and set panelConnected
        // BEFORE any Scheme scripts execute. sessionStorage is tab-scoped,
        // preventing contamination of other tabs/pages on the same origin.
        try { sessionStorage.setItem('schemeJS_panelConnected', 'true'); } catch {}
        return { active, needsReload: !active };
      },

      /**
       * Sets a breakpoint at the specified source location.
       * Delegates to the BreakpointManager on the debug runtime.
       *
       * @param {string} url - Source URL (e.g., 'scheme://scheme-sources/app.scm')
       * @param {number} line - Line number (1-indexed)
       * @param {number|null} [column=null] - Column number (1-indexed), or null for line-level
       * @returns {string|null} Breakpoint ID, or null if debug not active
       */
      setBreakpoint(url, line, column = null) {
        const dr = interpreter.debugRuntime;
        if (!dr) return null;
        return dr.setBreakpoint(url, line, column);
      },

      /**
       * Removes a breakpoint by ID.
       *
       * @param {string} id - Breakpoint ID returned from setBreakpoint
       * @returns {boolean} True if removed, false if not found
       */
      removeBreakpoint(id) {
        const dr = interpreter.debugRuntime;
        if (!dr) return false;
        return dr.removeBreakpoint(id);
      },

      /**
       * Gets all currently set breakpoints.
       * @returns {Array<{id: string, filename: string, line: number, column: number|null}>}
       */
      getAllBreakpoints() {
        const dr = interpreter.debugRuntime;
        if (!dr) return [];
        return dr.getAllBreakpoints();
      },

      /**
       * Gets the current source location (top of call stack).
       * Returns null when not paused or stack is empty.
       *
       * @returns {{filename: string, line: number, column: number}|null}
       */
      getCurrentLocation() {
        const dr = interpreter.debugRuntime;
        if (!dr) return null;
        const frame = dr.stackTracer.getCurrentFrame();
        return frame?.source || null;
      },

      /**
       * Gets the current debugger status.
       * When paused, includes the source location of the pause point.
       *
       * @returns {{state: string, reason: string|null, active: boolean, source: Object|null}}
       */
      getStatus() {
        const dr = interpreter.debugRuntime;
        if (!dr) return { state: 'inactive', reason: null, active: false, source: null };
        const state = dr.pauseController.getState();
        let source = null;
        if (state === 'paused') {
          // Get the current pause source from the active debug level
          const level = dr.levelStack.current();
          source = level?.source || null;
        }
        return {
          state,
          reason: dr.pauseController.getPauseReason(),
          active: dr.enabled,
          source
        };
      },

      /**
       * Acknowledges that the panel has received the pause event.
       * Cancels the safety timeout so the pause waits indefinitely
       * for an explicit resume/step/abort from the user.
       */
      ackPause() {
        const dr = interpreter.debugRuntime;
        if (dr) dr.ackPause();
      },

      /**
       * Resumes execution from a pause.
       * Delegates to the debug runtime's resume(), which resolves
       * the pending waitForResume() promise in the interpreter trampoline.
       */
      resume() {
        const dr = interpreter.debugRuntime;
        if (dr) dr.resume();
      },

      /**
       * Issue a Step Into command.
       * Transitions from paused to stepping-into via the PauseController.
       * The interpreter will pause at the next distinct Scheme expression.
       */
      stepInto() {
        const dr = interpreter.debugRuntime;
        if (dr) dr.stepInto();
      },

      /**
       * Issue a Step Over command.
       * Transitions from paused to stepping-over via the PauseController.
       * The interpreter will pause at the next expression at same or shallower depth.
       */
      stepOver() {
        const dr = interpreter.debugRuntime;
        if (dr) dr.stepOver();
      },

      /**
       * Issue a Step Out command.
       * Transitions from paused to stepping-out via the PauseController.
       * The interpreter will pause at the first expression after the current function returns.
       */
      stepOut() {
        const dr = interpreter.debugRuntime;
        if (dr) dr.stepOut();
      }
    };

    // Wire up depth getter for probe runtime stepping.
    // The probe runtime needs to know the current Scheme call stack depth
    // so that step-over and step-out can make depth-aware pause decisions.
    if (globalThis.__schemeProbeRuntime) {
      globalThis.__schemeProbeRuntime._getDepth =
        () => interpreter.debugRuntime?.stackTracer.getDepth() ?? 0;
    }

    // Wire up pause/resume events so the content script can relay
    // pause notifications to the DevTools panel.
    //
    // We dispatch BOTH:
    //   1. CustomEvent — for same-world listeners (test pages, direct page listeners)
    //   2. window.postMessage — for cross-world relay (content script in ISOLATED world)
    //
    // Chrome's CustomEvent.detail is null when read from a different world
    // (MAIN → ISOLATED). postMessage uses structured clone, which correctly
    // copies data across the world boundary.
    const dr = interpreter.debugRuntime;
    if (dr && typeof window !== 'undefined' && typeof window.dispatchEvent === 'function' && typeof CustomEvent !== 'undefined') {
      /**
       * When the interpreter hits a breakpoint or step, dispatch
       * 'scheme-debug-paused' via CustomEvent and postMessage so the
       * content script can relay it to the panel.
       *
       * @param {{reason: string, source: Object|null, stack: Array}} info
       */
      dr.onPause = (info) => {
        const detail = {
          reason: info.reason || 'breakpoint',
          source: info.source || null,
          stack: (info.stack || []).map(f => ({
            name: f.name,
            source: f.source || null,
            tcoCount: f.tcoCount || 0
          }))
        };
        
        // Export boundary target to globalThis for CDP to pick up
        if (info.reason === 'boundary' && info.data) {
           detail.funcName = info.data.funcName;
           if (info.data.func) {
               globalThis.__schemeBoundaryTarget = info.data.func;
           }
        }
        
        // CustomEvent for same-world listeners (test pages, direct page listeners)
        window.dispatchEvent(new CustomEvent('scheme-debug-paused', { detail }));
        // postMessage for cross-world relay (content script in ISOLATED world)
        window.postMessage({ type: 'scheme-debug-paused', detail }, '*');
      };

      /**
       * When execution resumes, dispatch 'scheme-debug-resumed' so the
       * panel can update its state.
       *
       * Only fires for actual resumes ('resume' or 'timeout'), NOT for
       * stepping actions. Stepping immediately re-pauses, so the panel
       * should stay in its "paused" state to avoid flicker and the need
       * to click Resume multiple times.
       *
       * @param {string} action - 'resume', 'timeout', 'stepInto', 'stepOver', 'stepOut'
       */
      dr.onResume = (action) => {
        if (action === 'resume' || action === 'timeout') {
          window.dispatchEvent(new CustomEvent('scheme-debug-resumed'));
          window.postMessage({ type: 'scheme-debug-resumed' }, '*');
        }
      };
    }
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
