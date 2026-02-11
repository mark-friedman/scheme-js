/**
 * @fileoverview Abstract DebugBackend interface.
 *
 * Defines the interface for debug backends (CDP, REPL, DAP, etc.).
 * The SchemeDebugRuntime calls backend methods to notify of debug events.
 * Backends implement these to provide UI/protocol-specific behavior.
 */

/**
 * Abstract base class for debug backends.
 * Subclasses implement protocol-specific behavior (CDP, REPL commands, DAP).
 *
 * @abstract
 */
export class DebugBackend {
    /**
     * Called when execution pauses (breakpoint, step, exception).
     *
     * @abstract
     * @param {Object} pauseInfo - Information about the pause
     * @param {string} pauseInfo.reason - 'breakpoint', 'step', 'exception'
     * @param {string|null} pauseInfo.breakpointId - Hit breakpoint ID (if applicable)
     * @param {Object} pauseInfo.source - Source location
     * @param {Array} pauseInfo.stack - Current call stack
     * @param {Object} pauseInfo.env - Current environment
     * @returns {Promise<string>} Action to take: 'resume', 'stepInto', 'stepOver', 'stepOut'
     */
    async onPause(pauseInfo) {
        throw new Error('DebugBackend.onPause must be implemented');
    }

    /**
     * Called when execution resumes.
     *
     * @abstract
     */
    onResume() {
        // Default: no-op
    }

    /**
     * Called when a script/file is loaded.
     *
     * @abstract
     * @param {Object} scriptInfo - Script information
     * @param {string} scriptInfo.filename - Script file path
     * @param {string} scriptInfo.source - Source code
     */
    onScriptLoaded(scriptInfo) {
        // Default: no-op
    }

    /**
     * Called when an exception occurs.
     *
     * @abstract
     * @param {Object} exceptionInfo - Exception information
     * @param {*} exceptionInfo.value - Exception value
     * @param {boolean} exceptionInfo.caught - Whether exception was caught
     * @param {Object} exceptionInfo.source - Source location
     */
    onException(exceptionInfo) {
        // Default: no-op
    }

    /**
     * Called when console output occurs.
     *
     * @abstract
     * @param {string} type - 'log', 'warn', 'error', etc.
     * @param {Array} args - Console arguments
     */
    onConsole(type, args) {
        // Default: no-op
    }
}

/**
 * Simple synchronous debug backend for testing.
 * Immediately returns 'resume' action on pause.
 */
export class NoOpDebugBackend extends DebugBackend {
    async onPause(pauseInfo) {
        return 'resume';
    }
}

/**
 * Debug backend that captures events for testing.
 * Supports nested pauses via action queue and resolver stack.
 */
export class TestDebugBackend extends DebugBackend {
    constructor() {
        super();
        this.pauseEvents = [];
        this.resumeEvents = [];
        this.scriptEvents = [];
        this.exceptionEvents = [];
        /** @type {string[]} - Queue of pre-set actions (FIFO) */
        this.pendingActions = [];
        /** @type {Function[]} - Stack of pending resolvers (LIFO) */
        this.actionResolverStack = [];
    }

    async onPause(pauseInfo) {
        this.pauseEvents.push(pauseInfo);

        // If there's a pre-set action, return it (FIFO)
        if (this.pendingActions.length > 0) {
            return this.pendingActions.shift();
        }

        // Otherwise, wait for external action (LIFO stack for nesting)
        return new Promise(resolve => {
            this.actionResolverStack.push(resolve);
        });
    }

    onResume() {
        this.resumeEvents.push({ timestamp: Date.now() });
    }

    onScriptLoaded(scriptInfo) {
        this.scriptEvents.push(scriptInfo);
    }

    onException(exceptionInfo) {
        this.exceptionEvents.push(exceptionInfo);
    }

    /**
     * Sets the action to return on next pause.
     * If a resolver is already waiting, resolves the topmost one (LIFO).
     * Otherwise queues the action for the next onPause call.
     * @param {string} action - 'resume', 'stepInto', 'stepOver', 'stepOut', 'abort'
     */
    setNextAction(action) {
        if (this.actionResolverStack.length > 0) {
            const resolve = this.actionResolverStack.pop();
            resolve(action);
        } else {
            this.pendingActions.push(action);
        }
    }

    /**
     * Clears all captured events.
     */
    clear() {
        this.pauseEvents = [];
        this.resumeEvents = [];
        this.scriptEvents = [];
        this.exceptionEvents = [];
    }
}

/**
 * REPL Debug Backend Interface
 *
 * For REPL integration, the debug backend should implement command handling:
 *
 * Commands (entered at REPL prompt during pause):
 *   :break <file> <line> [column]  - Set breakpoint
 *   :unbreak <id>                   - Remove breakpoint
 *   :step or :s                     - Step into
 *   :next or :n                     - Step over
 *   :finish or :fin                 - Step out
 *   :continue or :c                 - Resume execution
 *   :bt or :backtrace               - Show stack trace
 *   :locals                         - Show local variables
 *   :eval <expr>                    - Evaluate in current scope
 *   :debug on|off                   - Toggle debugging
 *
 * The REPL should:
 * 1. Detect pause via onPause callback
 * 2. Enter a debug prompt loop (e.g., "debug> ")
 * 3. Parse debug commands and call SchemeDebugRuntime methods
 * 4. Return action when user enters step/continue commands
 */
