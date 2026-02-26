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
     * Called by the probe script at each Scheme expression.
     * Returns true if the probe should pause (via `debugger;` in the probe
     * function itself), false otherwise. The `debugger;` must fire inside
     * the source-mapped probe function so that DevTools shows Scheme code,
     * not this runtime file.
     *
     * @param {number} exprId - The unique ID of the executing expression
     * @returns {boolean} True if the probe should pause
     */
    hit(exprId) {
        if (!this._active) return false;

        // Exception pause takes priority — fired by onException() in devtools_debug.js
        if (this._exceptionPause) {
            this._exceptionPause = false;
            return true;
        }

        // Check if breakpoint is set on this expression
        if (this._breakpoints.has(exprId)) {
            return true;
        }

        // Stepping (Step Into / Step Over / Step Out) is manually handled here
        // because Native Chrome CDP drops into interpreter JS files.
        if (this._stepping && this._shouldStopForStep(exprId)) {
            this._stepping = false;
            return true;
        }

        return false;
    },

    /**
     * Determines if execution should pause for stepping.
     * Placeholder: always returns true, so all step modes currently behave
     * as Step Into. To be fully implemented in Phase 2.5
     * (Interpreter-Managed Stepping).
     *
     * @param {number} exprId
     * @returns {boolean}
     * @private
     */
    _shouldStopForStep(exprId) {
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
     * Currently all step modes behave identically (as Step Into).
     */
    stepInto() {
        this._stepping = true;
    },

    /**
     * Begins a Step Over operation. Currently behaves as Step Into;
     * depth-aware logic will be added in Phase 2.5.
     */
    stepOver() {
        this._stepping = true;
    },

    /**
     * Begins a Step Out operation. Currently behaves as Step Into;
     * depth-aware logic will be added in Phase 2.5.
     */
    stepOut() {
        this._stepping = true;
    },

    /**
     * Resumes normal execution, cancelling any active stepping.
     */
    resume() {
        this._stepping = false;
    },

    /**
     * Aborts any in-progress stepping operation.
     */
    abortStepping() {
        this._stepping = false;
    }
};
