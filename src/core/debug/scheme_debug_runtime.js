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

        this.onPause = options.onPause || null;
        this.onResume = options.onResume || null;

        /** @type {boolean} */
        this.enabled = true;
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
            this.onResume();
        }
    }

    /**
     * Step into: pause at next expression.
     */
    stepInto() {
        this.pauseController.stepInto();
        if (this.onResume) {
            this.onResume();
        }
    }

    /**
     * Step over: pause at next expression at same or shallower depth.
     */
    stepOver() {
        this.pauseController.stepOver(this.stackTracer.getDepth());
        if (this.onResume) {
            this.onResume();
        }
    }

    /**
     * Step out: pause after returning from current function.
     */
    stepOut() {
        this.pauseController.stepOut(this.stackTracer.getDepth());
        if (this.onResume) {
            this.onResume();
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
    }
}

export default SchemeDebugRuntime;
