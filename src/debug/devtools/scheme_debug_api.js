/**
 * @fileoverview Builds and installs the `__schemeDebug` global API on `globalThis`.
 *
 * This API is the bridge between the Chrome DevTools extension (running in
 * extension context) and the interpreter's internal state (running in page
 * context). The extension calls these methods via
 * `chrome.scripting.executeScript()` or `chrome.devtools.inspectedWindow.eval()`.
 *
 * Extracted from DevToolsDebugIntegration to keep file sizes manageable and
 * separate the API surface from the probe-calling / async-task-tagging core.
 */

// =========================================================================
// Shared Stack Mapping
// =========================================================================

/**
 * Maps raw StackTracer frames into the serializable format used by both
 * `__schemeDebug.getStack()` and the `onPause` event detail, adding
 * synthetic frames as appropriate.
 *
 * Without this helper the same mapping + synthetic-frame logic was
 * duplicated in two places.
 *
 * @param {Array} rawFrames - Frames from `stackTracer.getStack()`
 * @param {Object|null} pauseSource - The actual pause location (overrides
 *   the top frame's source, which is the function definition location)
 * @param {Object|null} pauseEnv - The environment at the pause point
 *   (used to create a synthetic top-level frame when the stack is empty)
 * @returns {Array<{name: string, source: Object|null, callSiteSource: Object|null, tcoCount: number}>}
 */
export function mapStackFrames(rawFrames, pauseSource, pauseEnv) {
  const mapped = rawFrames.map(f => ({
    name: f.name,
    source: f.source || null,
    callSiteSource: f.callSiteSource || null,
    tcoCount: f.tcoCount || 0,
  }));

  // Override top frame's source with the actual pause location.
  // f.source is the function definition location (e.g., line of `define`
  // or `lambda`), while pauseSource is where execution is NOW
  // (e.g., the `let` body or a call inside the function).
  if (mapped.length > 0 && pauseSource) {
    mapped[mapped.length - 1].source = pauseSource;
  }

  if (mapped.length === 0 && pauseEnv) {
    // Paused at top-level (no function frames): add a synthetic frame
    // so the stack is never empty during a pause.
    mapped.push({
      name: '<top-level>',
      source: pauseSource || null,
      tcoCount: 0,
      _synthetic: true,
    });
  } else if (mapped.length > 0) {
    // Add a synthetic bottom frame representing the call site of the
    // bottom function. This shows where the function was called from
    // (e.g., top-level code), giving a more complete call stack.
    const bottomFrame = mapped[0];
    if (bottomFrame.callSiteSource) {
      mapped.unshift({
        name: '<top-level>',
        source: bottomFrame.callSiteSource,
        tcoCount: 0,
        _synthetic: true,
      });
    }
  }

  return mapped;
}

// =========================================================================
// API Installation
// =========================================================================

/**
 * Installs the `__schemeDebug` global API on `globalThis`.
 *
 * @param {import('../../core/interpreter/interpreter.js').Interpreter} interpreter
 *   The interpreter instance to inspect.
 * @param {import('./source_registry.js').SchemeSourceRegistry} sourceRegistry
 *   The registry of loaded Scheme sources and their probe functions.
 * @param {Map<string, Set<number>>} probeBreakpointMap
 *   Maps breakpoint ID → Set of probe exprIds registered for it.
 * @param {Object} utils - Core interpreter utilities
 * @param {Function} utils.parse
 * @param {Function} utils.analyze
 * @param {Function} utils.list
 * @param {Function} utils.intern
 * @param {Function} utils.prettyPrint
 */
export function installSchemeDebugAPI(interpreter, sourceRegistry, probeBreakpointMap, utils) {
  const { parse, analyze, list, intern, prettyPrint } = utils;

  globalThis.__schemeDebug = {
    // --- Stack Inspection ---

    /**
     * Gets the current Scheme call stack.
     * When paused at top-level (no function frames), a synthetic
     * "top-level" frame is included so getStack() always returns at
     * least one frame during a pause.
     * @returns {Array<{name: string, source: Object|null, tcoCount: number}>}
     */
    getStack() {
      const runtime = interpreter.debugRuntime;
      const frames = runtime?.stackTracer.getStack() || [];
      const isSyncPaused = globalThis.__schemeProbeRuntime?._inSyncPath;

      if (runtime?.isPaused() || isSyncPaused) {
        return mapStackFrames(frames, runtime._currentPauseSource, runtime._currentPauseEnv);
      }

      // Not paused — return raw frames without synthetic additions
      return frames.map(f => ({
        name: f.name,
        source: f.source || null,
        callSiteSource: f.callSiteSource || null,
        tcoCount: f.tcoCount || 0,
      }));
    },

    /**
     * Gets the local variable bindings for a specific stack frame.
     * Walks the scope chain to include local, closure, and global variables.
     * Supports the synthetic top-level frame created by getStack().
     * @param {number} frameIndex - Index into the stack (0 = bottom, length-1 = top)
     * @returns {Array<{name: string, value: string, type: string, subtype: string|null, scope: string}>}
     */
    getLocals(frameIndex) {
      const runtime = interpreter.debugRuntime;
      const frames = runtime?.stackTracer.getStack() || [];
      const inspector = runtime?.stateInspector;
      if (!inspector) { return []; }

      let env;

      if (frames.length === 0 && runtime?.isPaused() && runtime._currentPauseEnv) {
        // Synthetic top-level frame (no function frames): use the pause environment
        if (frameIndex === 0) {
          env = runtime._currentPauseEnv;
        } else {
          return [];
        }
      } else {
        // Determine if getStack() would have inserted a synthetic bottom frame.
        // If so, the panel's frameIndex is offset by 1 from the StackTracer index.
        const hasSyntheticBottom = runtime?.isPaused() &&
          frames.length > 0 && frames[0].callSiteSource;
        const realIndex = hasSyntheticBottom ? frameIndex - 1 : frameIndex;

        if (hasSyntheticBottom && frameIndex === 0) {
          // Synthetic bottom frame: use the bottom function's closure env
          // to show the top-level scope where the function was called.
          const bottomFunc = frames[0];
          env = bottomFunc.env?.parent || bottomFunc.env;
        } else {
          if (realIndex < 0 || realIndex >= frames.length) return [];
          // For the top frame (currently executing), use _currentPauseEnv
          // which reflects the actual env at pause time (includes let bindings
          // and other scope extensions since function entry).
          const isTopFrame = (realIndex === frames.length - 1);
          if (isTopFrame && runtime?.isPaused() && runtime._currentPauseEnv) {
            env = runtime._currentPauseEnv;
          } else {
            env = frames[realIndex].env;
          }
        }
      }

      if (!env) { return []; }

      return walkScopeChain(env, inspector, prettyPrint);
    },

    /**
     * Gets source info for a specific stack frame.
     * @param {number} frameIndex
     * @returns {{filename: string, line: number, column: number}|null}
     */
    getSource(frameIndex) {
      const runtime = interpreter.debugRuntime;
      const frames = runtime?.stackTracer.getStack() || [];
      // Account for synthetic bottom frame offset (same logic as getLocals)
      const hasSyntheticBottom = runtime?.isPaused() &&
        frames.length > 0 && frames[0].callSiteSource;
      if (hasSyntheticBottom && frameIndex === 0) {
        return frames[0].callSiteSource;
      }
      const realIndex = hasSyntheticBottom ? frameIndex - 1 : frameIndex;
      if (realIndex < 0 || realIndex >= frames.length) return null;
      return frames[realIndex].source || null;
    },

    // --- Source Registry Wrappers ---

    /**
     * Gets all registered Scheme sources.
     * @returns {Array<{url: string, content: string, lines: number, origin: string}>}
     */
    getSources() {
      return sourceRegistry.getAllSources();
    },

    /**
     * Gets the expression spans for a source URL.
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

    // --- Evaluation ---

    /**
     * Evaluates a Scheme expression in the context of a specific stack frame.
     * Uses the synchronous run() path (parse → analyze → run).
     * @param {string} code - Scheme expression to evaluate
     * @param {number} [frameIndex] - Frame index (defaults to top frame)
     * @returns {string} Result as a string
     */
    eval(code, frameIndex) {
      const runtime = interpreter.debugRuntime;
      const frames = runtime?.stackTracer.getStack() || [];
      // Account for synthetic bottom frame offset
      const hasSyntheticBottom = runtime?.isPaused() &&
        frames.length > 0 && frames[0].callSiteSource;
      const adjustedIdx = frameIndex != null
        ? (hasSyntheticBottom ? frameIndex - 1 : frameIndex)
        : (frames.length - 1);
      // Use the specified frame's env, or fall back to global env for top-level pauses
      let env;
      if (hasSyntheticBottom && frameIndex === 0) {
        // Synthetic bottom frame: use bottom function's parent env (top-level scope)
        env = frames[0].env?.parent || interpreter.globalEnv;
      } else if (adjustedIdx >= 0 && adjustedIdx < frames.length) {
        // For top frame, use current pause env for most accurate scope
        const isTopFrame = (adjustedIdx === frames.length - 1);
        if (isTopFrame && runtime?.isPaused() && runtime._currentPauseEnv) {
          env = runtime._currentPauseEnv;
        } else {
          env = frames[adjustedIdx].env;
        }
      } else {
        env = interpreter.globalEnv;
      }
      try {
        const expressions = parse(code);
        if (expressions.length === 0) return '';

        let ast;
        if (expressions.length === 1) {
          ast = analyze(expressions[0]);
        } else {
          ast = analyze(list(intern('begin'), ...expressions));
        }

        const result = interpreter.run(ast, env);
        return prettyPrint(result);
      } catch (e) {
        return `#<error: ${e.message}>`;
      }
    },

    // --- Activation ---

    /**
     * Enables debug mode for future page loads.
     * Sets globalThis.__SCHEME_JS_DEBUG so that on next reload, the
     * interpreter is started in debug mode. Returns the current active state.
     * @returns {{active: boolean, needsReload: boolean}}
     */
    activate() {
      globalThis.__SCHEME_JS_DEBUG = true;
      const dr = interpreter.debugRuntime;
      const active = !!(dr?.enabled);
      // Mark the panel as connected so handlePause() blocks (waits for
      // resume commands) instead of auto-resuming.
      if (dr) dr.panelConnected = true;
      // Also tell the probe runtime so hit() returns false and debugger;
      // doesn't fire (which would block the trampoline in Sources tab).
      if (globalThis.__schemeProbeRuntime) {
        globalThis.__schemeProbeRuntime._panelConnected = true;
      }
      // Persist to sessionStorage so activate_debug.js can read it at
      // document_start on the next page reload and set panelConnected
      // BEFORE any Scheme scripts execute.
      try { sessionStorage.setItem('schemeJS_panelConnected', 'true'); } catch {}
      return { active, needsReload: !active };
    },

    // --- Breakpoint Management ---

    /**
     * Sets a breakpoint at the specified source location.
     * @param {string} url - Source URL
     * @param {number} line - Line number (1-indexed)
     * @param {number|null} [column=null] - Column number (1-indexed), or null for line-level
     * @returns {string|null} Breakpoint ID, or null if debug not active
     */
    setBreakpoint(url, line, column = null) {
      const dr = interpreter.debugRuntime;
      if (!dr) return null;
      const id = dr.setBreakpoint(url, line, column);
      if (!id) return null;

      // Register matching exprIds with probe runtime so breakpoints
      // fire in synchronous Scheme calls (JS → Scheme callbacks).
      if (globalThis.__schemeProbeRuntime) {
        const spans = sourceRegistry.getExpressions(url);
        const exprIds = new Set();
        if (column === null) {
          // Line-level: register outermost expression only
          let outermost = null;
          for (const span of spans) {
            if (span.line !== line) continue;
            if (!outermost || span.column < outermost.column) {
              outermost = span;
            }
          }
          if (outermost) {
            exprIds.add(outermost.exprId);
            globalThis.__schemeProbeRuntime.setBreakpoint(outermost.exprId);
          }
        } else {
          // Column-level (expression) breakpoint: exact match only
          for (const span of spans) {
            if (span.line !== line || span.column !== column) continue;
            exprIds.add(span.exprId);
            globalThis.__schemeProbeRuntime.setBreakpoint(span.exprId);
          }
        }
        if (exprIds.size > 0) {
          probeBreakpointMap.set(id, exprIds);
        }
      }

      return id;
    },

    /**
     * Removes a breakpoint by ID.
     * @param {string} id - Breakpoint ID returned from setBreakpoint
     * @returns {boolean} True if removed, false if not found
     */
    removeBreakpoint(id) {
      const dr = interpreter.debugRuntime;
      if (!dr) return false;

      const exprIds = probeBreakpointMap.get(id);
      if (exprIds && globalThis.__schemeProbeRuntime) {
        for (const exprId of exprIds) {
          globalThis.__schemeProbeRuntime.removeBreakpoint(exprId);
        }
        probeBreakpointMap.delete(id);
      }

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

    // --- Status & Control ---

    /**
     * Gets the current source location (top of call stack).
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
     * @returns {{state: string, reason: string|null, active: boolean, source: Object|null}}
     */
    getStatus() {
      const dr = interpreter.debugRuntime;
      if (!dr) return { state: 'inactive', reason: null, active: false, source: null };
      const state = dr.pauseController.getState();
      let source = null;
      if (state === 'paused') {
        const level = dr.levelStack.current();
        source = level?.source || null;
      }
      return {
        state,
        reason: dr.pauseController.getPauseReason(),
        active: dr.enabled,
        source,
      };
    },

    /**
     * Acknowledges that the panel has received the pause event.
     */
    ackPause() {
      const dr = interpreter.debugRuntime;
      if (dr) dr.ackPause();
    },

    /** Resumes execution from a pause. */
    resume() {
      const dr = interpreter.debugRuntime;
      if (dr) dr.resume();
    },

    /** Step Into — pause at the next distinct Scheme expression. */
    stepInto() {
      const dr = interpreter.debugRuntime;
      if (dr) dr.stepInto();
    },

    /** Step Over — pause at the next expression at same or shallower depth. */
    stepOver() {
      const dr = interpreter.debugRuntime;
      if (dr) dr.stepOver();
    },

    /** Step Out — pause at the first expression after the current function returns. */
    stepOut() {
      const dr = interpreter.debugRuntime;
      if (dr) dr.stepOut();
    },
  };

  // Wire up depth getter for probe runtime stepping
  if (globalThis.__schemeProbeRuntime) {
    globalThis.__schemeProbeRuntime._getDepth =
      () => interpreter.debugRuntime?.stackTracer.getDepth() ?? 0;
  }

  // Wire up pause/resume events for content script relay
  wireUpPauseEvents(interpreter, prettyPrint);
}

// =========================================================================
// Scope Chain Walking
// =========================================================================

/**
 * Walks the scope chain from the given environment upward through closure
 * and global scopes, collecting variable bindings.
 *
 * @param {Object} env - Starting environment
 * @param {Object} inspector - StateInspector instance
 * @param {Function} prettyPrint - Value printer
 * @returns {Array<{name: string, value: string, type: string, subtype: string|null, scope: string}>}
 */
function walkScopeChain(env, inspector, prettyPrint) {
  const result = [];
  const seen = new Set();
  let currentEnv = env;
  let scopeIndex = 0;

  while (currentEnv) {
    const isGlobal = currentEnv.parent === null;
    const scope = isGlobal ? 'global'
      : scopeIndex === 0 ? 'local'
      : 'closure';

    const systemNames = isGlobal ? currentEnv._systemBindingNames : null;

    const locals = inspector.getLocals(currentEnv);
    for (const [name, value] of locals) {
      if (seen.has(name)) continue;
      seen.add(name);

      // Skip system-defined names in the global scope
      if (systemNames) {
        const internalName = currentEnv.nameMap?.get(name);
        if (systemNames.has(name) || (internalName && systemNames.has(internalName))) {
          continue;
        }
      }

      const serialized = inspector.serializeValue(value);
      result.push({
        name,
        value: prettyPrint(value),
        type: serialized.type,
        subtype: serialized.subtype || null,
        scope,
      });
    }

    currentEnv = currentEnv.parent;
    scopeIndex++;
  }

  return result;
}

// =========================================================================
// Pause/Resume Event Wiring
// =========================================================================

/**
 * Wires up the interpreter's `onPause` and `onResume` callbacks to dispatch
 * DOM events (CustomEvent + postMessage) so the content script can relay
 * pause notifications to the DevTools panel.
 *
 * @param {Object} interpreter - The interpreter instance
 * @param {Function} prettyPrint - Value printer
 */
function wireUpPauseEvents(interpreter, prettyPrint) {
  const dr = interpreter.debugRuntime;
  if (!dr) return;
  if (typeof window === 'undefined' || typeof window.dispatchEvent !== 'function' || typeof CustomEvent === 'undefined') return;

  /**
   * When the interpreter hits a breakpoint or step, dispatch
   * 'scheme-debug-paused' via CustomEvent and postMessage so the
   * content script can relay it to the panel.
   */
  dr.onPause = (info) => {
    const mappedStack = mapStackFrames(
      info.stack || [],
      info.source,
      // For top-level pauses (empty stack), pass a truthy env so the
      // synthetic frame is created. info.source being present is
      // sufficient to trigger this path.
      info.source ? {} : null,
    );

    const detail = {
      reason: info.reason || 'breakpoint',
      source: info.source || null,
      stack: mappedStack,
    };

    // Include the previous expression's result for step pauses
    if (info.reason === 'step' && info.lastResult !== undefined) {
      detail.lastResult = prettyPrint(info.lastResult);
    }

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
   * When execution resumes, dispatch 'scheme-debug-resumed'.
   * Only fires for actual resumes, NOT for stepping actions.
   */
  dr.onResume = (action) => {
    if (action === 'resume' || action === 'timeout') {
      window.dispatchEvent(new CustomEvent('scheme-debug-resumed'));
      window.postMessage({ type: 'scheme-debug-resumed' }, '*');
    }
  };
}
