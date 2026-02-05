/**
 * @fileoverview DebugExceptionHandler - Handles break-on-exception logic.
 * 
 * Determines when exceptions should trigger debugger pauses based on
 * configuration (break on caught, break on uncaught) and the current
 * exception handler stack state.
 */

import { getExceptionHandlerFrameClass } from '../interpreter/frame_registry.js';

/**
 * Handles exception debugging logic for the debug runtime.
 * Checks if exceptions should trigger debugger pauses based on configuration.
 */
export class DebugExceptionHandler {
    /**
     * @param {Object} debugRuntime - The parent SchemeDebugRuntime
     */
    constructor(debugRuntime) {
        this.debugRuntime = debugRuntime;

        /**
         * Whether to break on all exceptions (caught and uncaught).
         * @type {boolean}
         */
        this.breakOnCaughtException = false;

        /**
         * Whether to break on uncaught exceptions only.
         * @type {boolean}
         */
        this.breakOnUncaughtException = true;
    }

    /**
     * Determines if execution should pause on this exception.
     * 
     * @param {*} exception - The exception value
     * @param {Array} fstack - The current frame stack
     * @returns {boolean} True if should pause
     */
    shouldBreakOnException(exception, fstack) {
        // Check if debugging is enabled
        if (!this.debugRuntime.enabled) {
            return false;
        }

        // Determine if the exception will be caught
        const willBeCaught = this.isExceptionCaught(fstack);

        // Break on caught exceptions if configured
        if (willBeCaught && this.breakOnCaughtException) {
            return true;
        }

        // Break on uncaught exceptions if configured
        if (!willBeCaught && this.breakOnUncaughtException) {
            return true;
        }

        return false;
    }

    /**
     * Checks if an exception will be caught by a handler in the frame stack.
     * Looks for ExceptionHandlerFrame in the stack.
     * 
     * @param {Array} fstack - The current frame stack
     * @returns {boolean} True if an exception handler is present
     */
    isExceptionCaught(fstack) {
        if (!fstack || !Array.isArray(fstack)) {
            return false;
        }

        const ExceptionHandlerFrame = getExceptionHandlerFrameClass();
        if (!ExceptionHandlerFrame) {
            return false;
        }

        // Check if there's an ExceptionHandlerFrame in the stack
        for (let i = fstack.length - 1; i >= 0; i--) {
            if (fstack[i] instanceof ExceptionHandlerFrame) {
                return true;
            }
        }

        return false;
    }

    /**
     * Resets exception handling configuration to defaults.
     */
    reset() {
        this.breakOnCaughtException = false;
        this.breakOnUncaughtException = true;
    }
}

export default DebugExceptionHandler;
