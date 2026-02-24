/**
 * @fileoverview SchemeDebugRuntime - Main coordinator for debugging functionality.
 *
 * Integrates BreakpointManager, StackTracer, and PauseController to provide
 * a unified debugging interface. This is the main entry point for debug
 * integration with the interpreter.
 */

import { BreakpointManager } from './breakpoint_manager.js';
import { StackTracer } from './stack_tracer.js';
import { PauseController } from './pause_controller.js';
import { DebugExceptionHandler } from './exception_handler.js';
import { StateInspector } from './state_inspector.js';
import { DebugLevel, DebugLevelStack } from './debug_level.js';

/**
 * Main debug runtime coordinator.
 * Connects breakpoints, stack tracing, and pause control.
 */
export class SchemeDebugRuntime {
    /**
     * @param {Object} [options] - Configuration options
     * @param {Function} [options.onPause] - Callback when execution pauses
     * @param {Function} [options.onResume] - Callback when execution resumes
     */
    constructor(options = {}) {
        this.breakpointManager = new BreakpointManager();
        this.stackTracer = new StackTracer();
        this.pauseController = new PauseController();
        this.exceptionHandler = new DebugExceptionHandler(this);
        this.stateInspector = new StateInspector();
        this.levelStack = new DebugLevelStack();

        this.onPause = options.onPause || null;
        this.onResume = options.onResume || null;

        /** @type {DebugBackend|null} */
        this.backend = null;

        /** @type {import('./devtools/devtools_debug.js').DevToolsDebugIntegration|null} */
        this.devtoolsDebug = null;

        /** @type {boolean} */
        this.enabled = false;
    }

    /**
     * Sets the debug backend and hooks up its event handlers.
     * @param {DebugBackend} backend
     */
    setBackend(backend) {
        this.backend = backend;
        this.onPause = (info) => backend.onPause(info);
        this.onResume = () => backend.onResume();

        // Also hook up script loading if supported
        if (backend.onScriptLoaded) {
            this.onScriptLoaded = (info) => backend.onScriptLoaded(info);
        }
    }

    /**
     * Sets the DevTools integration for async stack tagging.
     * When set, frame enter/exit/replace hooks are forwarded to the
     * DevTools integration for console.createTask management.
     * @param {import('./devtools/devtools_debug.js').DevToolsDebugIntegration} integration
     */
    setDevToolsIntegration(integration) {
        this.devtoolsDebug = integration;
    }

    // =========================================================================
    // Exception Configuration (proxied to exceptionHandler)
    // =========================================================================

    /**
     * Gets whether to break on caught exceptions.
     * @type {boolean}
     */
    get breakOnCaughtException() {
        return this.exceptionHandler.breakOnCaughtException;
    }

    /**
     * Sets whether to break on caught exceptions.
     * @type {boolean}
     */
    set breakOnCaughtException(value) {
        this.exceptionHandler.breakOnCaughtException = value;
    }

    /**
     * Gets whether to break on uncaught exceptions.
     * @type {boolean}
     */
    get breakOnUncaughtException() {
        return this.exceptionHandler.breakOnUncaughtException;
    }

    /**
     * Sets whether to break on uncaught exceptions.
     * @type {boolean}
     */
    set breakOnUncaughtException(value) {
        this.exceptionHandler.breakOnUncaughtException = value;
    }

    // =========================================================================
    // Breakpoint Management
    // =========================================================================

    /**
     * Sets a breakpoint.
     * @param {string} filename - Source file path
     * @param {number} line - Line number (1-indexed)
     * @param {number} [column] - Optional column number
     * @returns {string} Breakpoint ID
     */
    setBreakpoint(filename, line, column = null) {
        return this.breakpointManager.setBreakpoint(filename, line, column);
    }

    /**
     * Removes a breakpoint.
     * @param {string} id - Breakpoint ID
     * @returns {boolean} True if removed
     */
    removeBreakpoint(id) {
        return this.breakpointManager.removeBreakpoint(id);
    }

    /**
     * Gets all breakpoints.
     * @returns {Array} Breakpoint list
     */
    getAllBreakpoints() {
        return this.breakpointManager.getAllBreakpoints();
    }

    // =========================================================================
    // Step Control
    // =========================================================================

    /**
     * Resumes execution.
     */
    resume() {
        this.pauseController.resume();
        if (this.onResume) {
            this.onResume('resume');
        }
    }

    /**
     * Step into: pause at next expression.
     */
    stepInto() {
        this.pauseController.stepInto();
        if (this.onResume) {
            this.onResume('stepInto');
        }
    }

    /**
     * Step over: pause at next expression at same or shallower depth.
     */
    stepOver() {
        this.pauseController.stepOver(this.stackTracer.getDepth());
        if (this.onResume) {
            this.onResume('stepOver');
        }
    }

    /**
     * Step out: pause after returning from current function.
     */
    stepOut() {
        this.pauseController.stepOut(this.stackTracer.getDepth());
        if (this.onResume) {
            this.onResume('stepOut');
        }
    }

    // =========================================================================
    // Interpreter Hooks
    // =========================================================================

    /**
     * Called by interpreter before evaluating an expression.
     * Determines if execution should pause.
     *
     * @param {Object} source - Source location info
     * @param {Object} env - Current environment
     * @returns {boolean} True if should pause
     */
    shouldPause(source, env) {
        if (!this.enabled) return false;
        if (!source) return false;

        // Check for breakpoint hit
        if (this.breakpointManager.hasBreakpoint(source)) {
            return true;
        }

        // Check for stepping pause
        const depth = this.stackTracer.getDepth();
        if (this.pauseController.shouldStepPause(depth)) {
            return true;
        }

        return false;
    }

    /**
     * Pauses execution at the current location.
     * @param {Object} source - Source location
     * @param {Object} env - Current environment
     * @param {string} [reason='breakpoint'] - Reason for pause
     */
    pause(source, env, reason = 'breakpoint') {
        // Find matching breakpoint ID if this was a breakpoint hit
        let bpId = null;
        if (reason === 'breakpoint') {
            for (const bp of this.breakpointManager.getAllBreakpoints()) {
                if (bp.filename === source.filename && bp.line === source.line) {
                    bpId = bp.id;
                    break;
                }
            }
        }

        this.pauseController.pause(reason, bpId);

        if (this.onPause) {
            this.onPause({
                reason,
                breakpointId: bpId,
                source,
                stack: this.stackTracer.getStack(),
                env
            });
        }
    }

    /**
     * Handles a debug pause asynchronously. Creates a DebugLevel, notifies
     * the backend, and waits for user action. Supports nested pauses
     * (e.g. eval during breakpoint hits another breakpoint) via LIFO
     * resolver stacks in PauseController and the backend.
     *
     * @param {Object} source - Source location
     * @param {Object} env - Current environment
     * @param {string} [reason='breakpoint'] - Reason for pause
     * @returns {Promise<string>} The action to take: 'resume', 'stepInto', 'stepOver', 'stepOut', 'abort'
     */
    async handlePause(source, env, reason = 'breakpoint') {
        // Find matching breakpoint ID if applicable
        let bpId = null;
        if (reason === 'breakpoint') {
            for (const bp of this.breakpointManager.getAllBreakpoints()) {
                if (bp.filename === source?.filename && bp.line === source?.line) {
                    bpId = bp.id;
                    break;
                }
            }
        }

        // Create debug level
        const level = new DebugLevel(
            this.levelStack.depth(),
            reason,
            source,
            this.stackTracer.getStack(),
            env,
            this.levelStack.current()
        );
        this.levelStack.push(level);

        // Pause the controller
        this.pauseController.pause(reason, bpId);

        const pauseInfo = {
            reason,
            breakpointId: bpId,
            source,
            stack: this.stackTracer.getStack(),
            env,
            level
        };

        let action;

        if (this.backend) {
            // Backend returns a promise that resolves with the user's action.
            // We wire it to resolve the pauseController's wait via _processAction,
            // then await the pauseController (which supports nested waits via
            // its LIFO resolver stack).
            this.backend.onPause(pauseInfo).then(act => {
                this._processAction(act);
            });

            await this.pauseController.waitForResume();

            // Determine action from controller state
            if (this.pauseController.isAborted()) {
                action = 'abort';
            } else {
                const stepMode = this.pauseController.getStepMode();
                if (stepMode) {
                    action = `step${stepMode.charAt(0).toUpperCase() + stepMode.slice(1)}`;
                } else {
                    action = 'resume';
                }
            }
        } else if (this.onPause) {
            // Callback-based path (used in tests without a full backend)
            this.onPause(pauseInfo);
            await this.pauseController.waitForResume();

            if (this.pauseController.isAborted()) {
                action = 'abort';
            } else {
                const stepMode = this.pauseController.getStepMode();
                if (stepMode) {
                    action = `step${stepMode.charAt(0).toUpperCase() + stepMode.slice(1)}`;
                } else {
                    action = 'resume';
                }
            }
        } else {
            action = 'resume';
        }

        // Pop level and notify backend
        this.levelStack.pop();
        if (this.backend && this.backend.onExitLevel) {
            this.backend.onExitLevel();
        }

        return action;
    }

    /**
     * Processes an action string from a backend and calls the appropriate method.
     * @param {string} action - 'resume', 'stepInto', 'stepOver', 'stepOut', 'abort'
     * @private
     */
    _processAction(action) {
        switch (action) {
            case 'stepInto':
                this.stepInto();
                break;
            case 'stepOver':
                this.stepOver();
                break;
            case 'stepOut':
                this.stepOut();
                break;
            case 'abort':
                this.abort();
                break;
            case 'resume':
            default:
                this.resume();
                break;
        }
    }

    /**
     * Pauses on an exception.
     * Called by RaiseNode when shouldBreakOnException returns true.
     *
     * @param {Object} raiseNode - The RaiseNode that raised the exception
     * @param {Object} registers - Current interpreter registers
     * @returns {boolean} Whether execution was paused
     */
    pauseOnException(raiseNode, registers) {
        // Get source from the raiseNode if available
        const source = raiseNode.source || null;
        const exception = raiseNode.exception;
        const env = registers.env;

        this.pauseController.pause('exception', null);

        if (this.onPause) {
            this.onPause({
                reason: 'exception',
                breakpointId: null,
                source,
                stack: this.stackTracer.getStack(),
                env,
                exception,
                continuable: raiseNode.continuable
            });
        }

        return true;
    }

    /**
     * Called when entering a function/procedure.
     * @param {Object} frameInfo - Frame information
     */
    enterFrame(frameInfo) {
        this.stackTracer.enterFrame(frameInfo);
        this.devtoolsDebug?.onEnterFrame(frameInfo);
    }

    /**
     * Called when exiting a function/procedure.
     */
    exitFrame() {
        this.stackTracer.exitFrame();
        this.devtoolsDebug?.onExitFrame();
    }

    /**
     * Called for tail call optimization.
     * @param {Object} frameInfo - New frame information
     */
    replaceFrame(frameInfo) {
        this.stackTracer.replaceFrame(frameInfo);
        this.devtoolsDebug?.onReplaceFrame(frameInfo);
    }

    // =========================================================================
    // State Inspection
    // =========================================================================

    /**
     * Gets the current call stack.
     * @returns {Array} Stack frames
     */
    getStack() {
        return this.stackTracer.getStack();
    }

    /**
     * Gets the current stack depth.
     * @returns {number}
     */
    getDepth() {
        return this.stackTracer.getDepth();
    }

    /**
     * Gets the current frame.
     * @returns {Object|null}
     */
    getCurrentFrame() {
        return this.stackTracer.getCurrentFrame();
    }

    /**
     * Checks if currently paused.
     * @returns {boolean}
     */
    isPaused() {
        return this.pauseController.isPaused();
    }

    /**
     * Gets pause state info.
     * @returns {{state: string, reason: string|null, data: *}}
     */
    getPauseState() {
        return {
            state: this.pauseController.getState(),
            reason: this.pauseController.getPauseReason(),
            data: this.pauseController.getPauseData()
        };
    }

    // =========================================================================
    // Debug Control
    // =========================================================================

    /**
     * Enables debugging.
     */
    enable() {
        this.enabled = true;
    }

    /**
     * Disables debugging (interpreter runs without debug checks).
     */
    disable() {
        this.enabled = false;
    }

    /**
     * Aborts execution.
     */
    abort() {
        this.pauseController.abort();
    }

    /**
     * Resets all debug state.
     */
    reset() {
        this.breakpointManager.clearAll();
        this.stackTracer.clear();
        this.pauseController.reset();
        this.exceptionHandler.reset();
        this.levelStack.popAll();
    }
}

export default SchemeDebugRuntime;
