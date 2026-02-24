/**
 * @fileoverview Chrome DevTools probe runtime.
 *
 * Exposes the global `__schemeProbeRuntime` object which is called by the
 * automatically generated probe scripts. This runtime handles the actual
 * decision to pause execution (via `debugger;`) based on breakpoints and
 * stepping state.
 */

globalThis.__schemeProbeRuntime = {
    _active: true,
    _breakpoints: new Set(),
    _stepping: false,

    /**
     * Called by the probe script at each Scheme expression.
     *
     * @param {number} exprId - The unique ID of the executing expression
     */
    hit(exprId) {
        if (!this._active) return;

        // Check if breakpoint is set on this expression
        if (this._breakpoints.has(exprId)) {
            // Pause V8 execution. Since interpreter internals are blackboxed,
            // DevTools will show the pause at the generated probe function,
            // which is source-mapped back to the Scheme code.
            debugger;
            return;
        }

        // Check if we should stop for stepping
        if (this._stepping && this._shouldStopForStep(exprId)) {
            this._stepping = false;
            debugger;
            return;
        }
    },

    /**
     * Determines if execution should pause for stepping.
     * To be fully implemented in Phase 2.5 (Interpreter-Managed Stepping).
     *
     * @param {number} exprId
     * @returns {boolean}
     * @private
     */
    _shouldStopForStep(exprId) {
        // Basic placeholder for now
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

    stepInto() {
        this._stepping = true;
        // Context setup will happen here
    },

    stepOver() {
        this._stepping = true;
        // Context setup will happen here
    },

    stepOut() {
        this._stepping = true;
        // Context setup will happen here
    },

    resume() {
        this._stepping = false;
    }
};
