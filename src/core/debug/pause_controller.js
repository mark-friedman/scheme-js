/**
 * @fileoverview PauseController for the Scheme debugger.
 * 
 * Manages debugger pause state and stepping logic.
 * Tracks whether execution is running, paused, or stepping.
 */

/**
 * Controls debugger pause state and stepping.
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
         * Resolver function for the pause promise.
         * Set when waitForResume() is called, cleared when resume() is called.
         * @type {Function|null}
         */
        this.pauseResolve = null;
    }

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
     * If there's a pending pause promise, resolves it.
     */
    resume() {
        // Resolve any pending pause promise first
        if (this.pauseResolve) {
            const resolve = this.pauseResolve;
            this.pauseResolve = null;
            resolve();
        }

        this.state = 'running';
        this.stepMode = null;
        this.targetDepth = null;
        this.pauseReason = null;
        this.pauseData = null;
    }

    /**
     * Returns a promise that resolves when resume() is called.
     * Used by the async interpreter loop to block on pause.
     * @returns {Promise<void>}
     */
    waitForResume() {
        if (!this.isPaused()) {
            return Promise.resolve();
        }
        return new Promise(resolve => {
            this.pauseResolve = resolve;
        });
    }

    /**
     * Step into: pause at the next expression.
     */
    stepInto() {
        this.state = 'stepping';
        this.stepMode = 'into';
        this.targetDepth = null;
        this.pauseReason = null;
        this.pauseData = null;
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

    /**
     * Resets to initial state.
     */
    reset() {
        // Resolve any pending pause first (allows clean shutdown)
        if (this.pauseResolve) {
            this.pauseResolve();
            this.pauseResolve = null;
        }

        this.state = 'running';
        this.stepMode = null;
        this.targetDepth = null;
        this.pauseReason = null;
        this.pauseData = null;
    }
}
