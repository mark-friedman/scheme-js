/**
 * @fileoverview REPL Debug Backend for interactive debugging.
 */

import { DebugBackend } from './debug_backend.js';
import { prettyPrint } from '../core/interpreter/printer.js';

/**
 * REPL debug backend for interactive debugging in browser and Node.js REPLs.
 *
 * Provides a pull-based model for REPL interaction.
 * When paused, it can notify the REPL to enter a debug command state.
 */
export class ReplDebugBackend extends DebugBackend {
    /**
     * @param {Function} outputFn - Function to write output to the REPL
     */
    constructor(outputFn) {
        super();
        this.outputFn = outputFn;
        this.paused = false;
        this.pauseInfo = null;
        this.onPauseCallback = null;
        this._pauseActionResolver = null;
    }

    /**
     * Sets a callback to be invoked when execution pauses.
     * @param {Function} cb
     */
    setOnPause(cb) {
        this.onPauseCallback = cb;
    }

    /**
     * Called when execution pauses.
     * @param {Object} pauseInfo - Information about the pause
     * @returns {Promise<string>} Action to take: 'resume', 'stepInto', 'stepOver', 'stepOut'
     */
    async onPause(pauseInfo) {
        this.paused = true;
        this.pauseInfo = pauseInfo;

        const { reason, source, breakpointId } = pauseInfo;
        const location = source ? `${source.filename}:${source.line}` : 'unknown location';

        const reasonStr = breakpointId
            ? `breakpoint ${breakpointId} hit`
            : (reason === 'step' ? 'step complete' : reason);

        this.outputFn(`\n;; Paused: ${reasonStr} at ${location}`);
        this.outputFn(`;; Use :bt for backtrace, :locals for variables, :continue to resume`);

        if (this.onPauseCallback) {
            this.onPauseCallback(pauseInfo);
        }

        // Return a promise that resolves when onResume is called with an action.
        // This allows the runtime to await the user's decision if desired.
        return new Promise(resolve => {
            this._pauseActionResolver = resolve;
        });
    }

    /**
     * Called when execution resumes.
     * @param {string} [action='resume'] - The action that caused resumption
     */
    onResume(action = 'resume') {
        this.paused = false;
        this.pauseInfo = null;
        this.outputFn(`;; Resumed`);

        if (this._pauseActionResolver) {
            this._pauseActionResolver(action);
            this._pauseActionResolver = null;
        }
    }

    /**
     * Called when a script/file is loaded.
     * @param {Object} scriptInfo - Script information
     */
    onScriptLoaded(scriptInfo) {
        // No-op for REPL backend
    }

    /**
     * Called when an exception occurs.
     * @param {Object} exceptionInfo - Exception information
     */
    onException(exceptionInfo) {
        // No-op for REPL backend (handled via onPause with reason='exception')
    }

    /**
     * Called when console output occurs.
     * @param {string} type - 'log', 'warn', 'error', etc.
     * @param {Array} args - Console arguments
     */
    onConsole(type, args) {
        // No-op for REPL backend
    }

    /**
     * Formats a value for display in the REPL.
     * @param {*} value - The value to format
     * @returns {string}
     */
    formatValue(value) {
        return prettyPrint(value);
    }

    /**
     * Checks if the backend is currently in a paused state.
     * @returns {boolean}
     */
    isPaused() {
        return this.paused;
    }

    /**
     * Gets information about the current pause.
     * @returns {Object|null}
     */
    getPauseInfo() {
        return this.pauseInfo;
    }
}
