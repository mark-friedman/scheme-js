/**
 * @fileoverview Chrome DevTools probe runtime.
 *
 * Exposes the global `__schemeProbeRuntime` object which is called by the
 * automatically generated probe scripts. `hit()` returns a boolean indicating
 * whether to pause; the actual `debugger;` statement fires in the probe
 * function so that DevTools shows source-mapped Scheme code.
 */

globalThis.__schemeProbeRuntime = {
    _active: true,
    _breakpoints: new Set(),
    _stepping: false,
    _exceptionPause: false,

    /**
     * Current step mode — determines how depth is compared.
     * @type {'into'|'over'|'out'|null}
     */
    _stepMode: null,

    /**
     * Stack depth when stepping was initiated. Used by step-over (pause at
     * same or shallower depth) and step-out (pause at shallower depth).
     * @type {number}
     */
    _stepStartDepth: 0,

    /**
     * Function that returns the current Scheme call stack depth.
     * Injected by devtools_debug.js when installing the __schemeDebug API,
     * so the probe runtime can make depth-aware stepping decisions.
     * @type {(() => number)|null}
     */
    _getDepth: null,

    /**
     * Whether a debug panel/frontend is connected. When true and `_inSyncPath`
     * is false, hit() returns false — the cooperative pause channel (handlePause
     * in the async trampoline) handles breakpoints instead. The probe's
     * `debugger;` statement must NOT fire on the async path when a panel is
     * connected, because it would cause Chrome's built-in Sources tab to pause
     * synchronously, blocking the trampoline before handlePause() can run.
     *
     * When `_inSyncPath` is true (synchronous `run()` call, e.g. DOM callback),
     * hit() may still return true so the CDP pause can be forwarded to the panel.
     *
     * Set by devtools_debug.js activate() and html_adapter.js from
     * __SCHEME_JS_PANELCONNECTED.
     * @type {boolean}
     */
    _panelConnected: false,

    /**
     * Whether the interpreter is currently executing on the synchronous `run()`
     * path (as opposed to the async `runDebug()` trampoline). Set to true by
     * `interpreter.run()` at entry and restored to the previous value on exit.
     *
     * When true and `_panelConnected` is also true, hit() is allowed to return
     * true so that breakpoints in DOM callbacks (and other synchronous Scheme
     * invocations from JS) can be caught via the CDP channel.
     *
     * @type {boolean}
     */
    _inSyncPath: false,

    /**
     * Called by the probe script at each Scheme expression.
     * Returns true if the probe should pause (via `debugger;` in the probe
     * function itself), false otherwise. The `debugger;` must fire inside
     * the source-mapped probe function so that DevTools shows Scheme code,
     * not this runtime file.
     *
     * When `_panelConnected` is true, always returns false — the cooperative
     * channel handles pausing instead. The `debugger;` statement is only
     * useful when debugging via Chrome's Sources tab (no panel connected).
     *
     * @param {number} exprId - The unique ID of the executing expression
     * @returns {boolean} True if the probe should pause
     */
    hit(exprId) {
        if (!this._active) return false;

        // When a debug panel is connected on the async (runDebug) path, skip
        // the debugger; statement entirely — the cooperative pause channel
        // (handlePause in the async trampoline) handles breakpoints instead.
        // On the synchronous run() path (_inSyncPath=true), allow the probe
        // to fire so the CDP pause can be forwarded to the panel.
        if (this._panelConnected && !this._inSyncPath) return false;

        // Exception pause takes priority — fired by onException() in devtools_debug.js
        if (this._exceptionPause) {
            this._exceptionPause = false;
            return true;
        }

        // Check if breakpoint is set on this expression
        if (this._breakpoints.has(exprId)) {
            return true;
        }

        // Stepping (Step Into / Step Over / Step Out) with depth awareness.
        // Native Chrome CDP stepping drops into interpreter JS files, so we
        // handle Scheme-level stepping here using stack depth.
        if (this._stepping && this._shouldStopForStep(exprId)) {
            this._stepping = false;
            this._stepMode = null;
            return true;
        }

        return false;
    },

    /**
     * Determines if execution should pause for stepping based on step mode
     * and current stack depth.
     *
     * - Step Into: always pauses at the next expression
     * - Step Over: pauses when depth <= start depth (same or shallower)
     * - Step Out: pauses when depth < start depth (shallower only)
     *
     * @param {number} exprId
     * @returns {boolean}
     * @private
     */
    _shouldStopForStep(exprId) {
        // Step Into always stops at the next expression
        if (!this._stepMode || this._stepMode === 'into') {
            return true;
        }

        const currentDepth = this._getDepth ? this._getDepth() : 0;

        if (this._stepMode === 'over') {
            // Step Over: pause at same or shallower depth
            return currentDepth <= this._stepStartDepth;
        }

        if (this._stepMode === 'out') {
            // Step Out: pause only at shallower depth
            return currentDepth < this._stepStartDepth;
        }

        return true;
    },

    // ---------------------------------------------------------------------------
    // APIs for the DevTools Extension Sidebar
    // ---------------------------------------------------------------------------

    setBreakpoint(exprId) {
        this._breakpoints.add(exprId);
    },

    removeBreakpoint(exprId) {
        this._breakpoints.delete(exprId);
    },

    /**
     * Begins a Step Into operation. The next `hit()` call will pause.
     */
    stepInto() {
        this._stepping = true;
        this._stepMode = 'into';
        this._stepStartDepth = 0; // Not needed for step into
    },

    /**
     * Begins a Step Over operation. Pauses at the next expression at
     * the same or shallower call depth.
     */
    stepOver() {
        this._stepping = true;
        this._stepMode = 'over';
        this._stepStartDepth = this._getDepth ? this._getDepth() : 0;
    },

    /**
     * Begins a Step Out operation. Pauses at the next expression at
     * a shallower call depth (after returning from the current function).
     */
    stepOut() {
        this._stepping = true;
        this._stepMode = 'out';
        this._stepStartDepth = this._getDepth ? this._getDepth() : 0;
    },

    /**
     * Resumes normal execution, cancelling any active stepping.
     */
    resume() {
        this._stepping = false;
        this._stepMode = null;
    },

    /**
     * Aborts any in-progress stepping operation.
     */
    abortStepping() {
        this._stepping = false;
        this._stepMode = null;
    }
};
