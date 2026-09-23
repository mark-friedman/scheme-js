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
import { ENV } from '../core/interpreter/stepables_base.js';
import { globalMacroRegistry } from '../core/interpreter/macro_registry.js';

/**
 * Whether a source span contains a location.
 *
 * `endColumn` is exclusive, matching the reader. A location with no column is
 * a whole-line breakpoint, and is inside the span if its line is.
 *
 * @param {Object|null|undefined} source - A span: `{filename, line, column,
 *   endLine, endColumn}`.
 * @param {string} filename - Source file path.
 * @param {number} line - Line number (1-indexed).
 * @param {number|null} column - Column (1-indexed), or null for a line.
 * @returns {boolean} True if the location is inside the span.
 */
function spanContains(source, filename, line, column) {
    if (!source || source.filename !== filename) return false;
    const endLine = source.endLine ?? source.line;
    if (line < source.line || line > endLine) return false;
    if (column === null || column === undefined) return true;
    if (line === source.line && column < source.column) return false;
    if (line === endLine && source.endColumn != null && column >= source.endColumn) return false;
    return true;
}

/**
 * How many lines a span covers, for choosing the innermost of two.
 * @param {Object} source - A span.
 * @returns {number} Line count.
 */
function spanLines(source) {
    return (source.endLine ?? source.line) - source.line + 1;
}

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
    // Compiled Code
    // =========================================================================

    /**
     * Connects this runtime to the interpreter it debugs.
     *
     * Needed so the runtime can find compiled procedures, which never reach
     * the interpreter's step loop and so are invisible to every hook the
     * runtime otherwise has. Called by `Interpreter.setDebugRuntime`.
     *
     * @param {Object} interpreter - The interpreter.
     */
    attachInterpreter(interpreter) {
        this.interpreter = interpreter;
    }

    /**
     * Finds the compiled procedure whose source contains a location, if any.
     *
     * A breakpoint there is accepted and never fires: the only place this
     * runtime can pause is the interpreter's step loop, and compiled code does
     * not run through it. Callers use this to say so rather than fail silently.
     *
     * Only top-level bindings are searched. That is sufficient because a
     * definition is compiled as a unit -- every procedure nested inside a
     * compiled one is compiled too, and lies inside its parent's span.
     *
     * The answer is worked out when asked rather than recorded when a
     * breakpoint is set, so a breakpoint placed first and compiled over later
     * is still reported.
     *
     * @param {string} filename - Source file path.
     * @param {number} line - Line number (1-indexed).
     * @param {number|null} [column] - Column (1-indexed), or null for a line.
     * @returns {{name: string, source: Object}|null} The innermost compiled
     *   procedure containing the location, or null if it is not compiled code.
     */
    compiledProcedureAt(filename, line, column = null) {
        let found = null;
        for (let env = this.interpreter?.globalEnv; env; env = env.parent) {
            for (const value of env.bindings.values()) {
                if (typeof value !== 'function' || value.$compiled !== true) continue;
                if (!spanContains(value.source, filename, line, column)) continue;
                // Prefer the tightest span, should a redefinition leave two
                // procedures covering the same lines.
                if (found === null || spanLines(value.source) < spanLines(found.source)) {
                    found = { name: value.schemeName ?? 'anonymous', source: value.source };
                }
            }
        }
        return found;
    }

    /**
     * Finds the `define-macro` transformer whose source contains a location,
     * if any.
     *
     * A breakpoint there is accepted and never fires. A transformer runs while
     * code is expanded, and expansion happens inside the analyzer, which is
     * synchronous and finishes before the interpreter runs a step of the code
     * being expanded; the only place this runtime can make execution wait is
     * between the steps of an asynchronous run. So transformers run on an
     * interpreter with no debug runtime at all: were a breakpoint to fire
     * during expansion it could not stop anything, and the runtime would be
     * left paused, to stop the program at its first step instead -- somewhere
     * the pause did not name. Callers use this to say so rather than fail
     * silently.
     *
     * The registries searched are those the attached interpreter's analysis
     * uses, with the global registry every context inherits from. Like
     * `compiledProcedureAt`, the answer is worked out when asked, so a
     * breakpoint placed before its macro was defined is still reported.
     *
     * @param {string} filename - Source file path.
     * @param {number} line - Line number (1-indexed).
     * @param {number|null} [column] - Column (1-indexed), or null for a line.
     * @returns {{name: string, source: Object}|null} The innermost transformer
     *   containing the location, or null if it is not transformer code.
     */
    macroTransformerAt(filename, line, column = null) {
        const context = this.interpreter?.context;
        const seen = new Set();
        let found = null;
        for (const start of [context?.currentMacroRegistry, context?.macroRegistry, globalMacroRegistry]) {
            for (let registry = start; registry && !seen.has(registry); registry = registry.parent) {
                seen.add(registry);
                for (const [name, transformer] of registry.macros) {
                    const source = transformer?.transformerProcedure?.source;
                    if (!spanContains(source, filename, line, column)) continue;
                    if (found === null || spanLines(source) < spanLines(found.source)) {
                        found = { name, source };
                    }
                }
            }
        }
        return found;
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
     * @param {Array} registers - Current interpreter registers. This is the
     *   evaluator's register *array*, indexed by the constants in
     *   `stepables_base.js`, not an object with named fields.
     * @returns {boolean} Whether execution was paused
     */
    pauseOnException(raiseNode, registers) {
        // Get source from the raiseNode if available
        const source = raiseNode.source || null;
        const exception = raiseNode.exception;
        const env = registers[ENV];

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
    }
}

export default SchemeDebugRuntime;
