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

        this.onPause = options.onPause || null;
        this.onResume = options.onResume || null;

        /** @type {DebugBackend|null} */
        this.backend = null;

        /** @type {boolean} */
        this.enabled = true;
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
    }

    /**
     * Called when exiting a function/procedure.
     */
    exitFrame() {
        this.stackTracer.exitFrame();
    }

    /**
     * Called for tail call optimization.
     * @param {Object} frameInfo - New frame information
     */
    replaceFrame(frameInfo) {
        this.stackTracer.replaceFrame(frameInfo);
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
     * Resets all debug state.
     */
    reset() {
        this.breakpointManager.clearAll();
        this.stackTracer.clear();
        this.pauseController.reset();
        this.exceptionHandler.reset();
    }
}

export default SchemeDebugRuntime;
