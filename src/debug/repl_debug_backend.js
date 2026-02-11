/**
 * @fileoverview REPL Debug Backend for interactive debugging.
 *
 * Supports nested debug levels by maintaining stacks of pause info
 * and action resolvers (LIFO). Each onPause() call pushes a new
 * entry; resolveAction() pops the topmost one.
 */

import { DebugBackend } from './debug_backend.js';
import { prettyPrint } from '../core/interpreter/printer.js';

/**
 * REPL debug backend for interactive debugging in browser and Node.js REPLs.
 *
 * Provides a pull-based model for REPL interaction.
 * When paused, it can notify the REPL to enter a debug command state.
 * Supports nested pauses (e.g. eval during breakpoint hits another breakpoint).
 */
export class ReplDebugBackend extends DebugBackend {
    /**
     * @param {Function} outputFn - Function to write output to the REPL
     */
    constructor(outputFn) {
        super();
        this.outputFn = outputFn;

        /**
         * Stack of pause info objects, one per active debug level.
         * @type {Object[]}
         */
        this._pauseInfoStack = [];

        /**
         * Stack of action resolvers, one per active debug level.
         * Each resolver is the resolve function of the Promise returned
         * by onPause() for that level.
         * @type {Function[]}
         */
        this._actionResolverStack = [];

        /** @type {Function|null} */
        this.onPauseCallback = null;
    }

    /**
     * Sets a callback to be invoked when execution pauses.
     * @param {Function} cb
     */
    setOnPause(cb) {
        this.onPauseCallback = cb;
    }

    /**
     * Called when execution pauses. Pushes a new debug level onto the
     * internal stacks and returns a promise that resolves when the user
     * issues a resume/step/abort command for this level.
     *
     * @param {Object} pauseInfo - Information about the pause
     * @returns {Promise<string>} Action to take: 'resume', 'stepInto', 'stepOver', 'stepOut', 'abort'
     */
    async onPause(pauseInfo) {
        this._pauseInfoStack.push(pauseInfo);

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

        return new Promise(resolve => {
            this._actionResolverStack.push(resolve);
        });
    }

    /**
     * Called when execution resumes (by the runtime's onResume callback).
     * Only prints a status message — level popping is handled by
     * resolveAction() which already resolved the action promise.
     * @param {string} [action='resume'] - The action that caused resumption
     */
    onResume(action = 'resume') {
        this.outputFn(`;; Resumed`);
    }

    /**
     * Resolves the topmost action resolver without printing ";; Resumed".
     * Called by REPL commands (:continue, :step, etc.) to unblock the
     * current debug level. The runtime will call onResume separately
     * if needed.
     * @param {string} action - 'resume', 'stepInto', 'stepOver', 'stepOut', 'abort'
     */
    resolveAction(action) {
        this._popLevel(action);
    }

    /**
     * Pops the topmost action resolver and pause info, resolving the
     * action promise for that level.
     * @param {string} action - The action to resolve with
     * @private
     */
    _popLevel(action) {
        if (this._actionResolverStack.length > 0) {
            const resolve = this._actionResolverStack.pop();
            this._pauseInfoStack.pop();
            resolve(action);
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
     * True when any debug level is active.
     * @returns {boolean}
     */
    isPaused() {
        return this._pauseInfoStack.length > 0;
    }

    /**
     * Gets information about the current (topmost) pause.
     * @returns {Object|null}
     */
    getPauseInfo() {
        if (this._pauseInfoStack.length === 0) return null;
        return this._pauseInfoStack[this._pauseInfoStack.length - 1];
    }

    /**
     * Gets the current debug nesting depth (number of active pauses).
     * @returns {number}
     */
    getDepth() {
        return this._pauseInfoStack.length;
    }

    /**
     * Called when exiting a debug level (e.g. after handlePause completes).
     * No-op for REPL backend — level management is handled by resolveAction.
     */
    onExitLevel() {
        // No-op
    }
}
