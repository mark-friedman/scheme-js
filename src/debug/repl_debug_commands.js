/**
 * @fileoverview REPL Debug Commands parser and executor.
 *
 * Stack frame navigation (:up, :down, :bt, :locals) is scoped to the
 * current debug level's captured stack. Each level maintains its own
 * selected frame index. Debug level navigation (:abort, :top) moves
 * between levels.
 */

import { parse } from '../core/interpreter/reader.js';
import { analyze, SyntacticEnv } from '../core/interpreter/analyzer.js';
import { prettyPrint } from '../core/interpreter/printer.js';
import { intern } from '../core/interpreter/symbol.js';

/**
 * Parses and executes REPL debug commands.
 * Commands start with ':' prefix.
 */
export class ReplDebugCommands {
    /**
     * @param {Interpreter} interpreter - The interpreter instance
     * @param {SchemeDebugRuntime} debugRuntime - The debug runtime
     * @param {ReplDebugBackend} backend - The REPL debug backend
     */
    constructor(interpreter, debugRuntime, backend) {
        this.interpreter = interpreter;
        this.debugRuntime = debugRuntime;
        this.backend = backend;

        /**
         * Per-level selected frame index stack. Kept in sync with
         * backend._pauseInfoStack.length via _syncFrameStack().
         * -1 means newest (top) frame for that level.
         * @type {number[]}
         * @private
         */
        this._selectedFrameStack = [];

        /**
         * Which debug level is currently being viewed (1-indexed).
         * -1 means the topmost (newest) level. Commands like :bt,
         * :locals, :up/:down, and :eval operate on this level.
         * @type {number}
         * @private
         */
        this._viewedLevel = -1;
    }

    /**
     * Checks if a string is a debug command.
     * @param {string} input
     * @returns {boolean}
     */
    isDebugCommand(input) {
        return input.trim().startsWith(':');
    }

    /**
     * Executes a debug command.
     * @param {string} input
     * @returns {Promise<string>} Output message
     */
    async execute(input) {
        const trimmed = input.trim();
        const parts = trimmed.slice(1).split(/\s+/);
        const cmd = parts[0].toLowerCase();
        const args = parts.slice(1);

        switch (cmd) {
            case 'debug':
                return this.handleDebug(args);
            case 'break':
                return this.handleBreak(args);
            case 'unbreak':
                return this.handleUnbreak(args);
            case 'breakpoints':
                return this.handleListBreakpoints();
            case 'step':
            case 's':
                return this.handleStepInto();
            case 'next':
            case 'n':
                return this.handleStepOver();
            case 'finish':
            case 'fin':
                return this.handleStepOut();
            case 'continue':
            case 'c':
                return this.handleContinue();
            case 'bt':
            case 'backtrace':
                return this.handleBacktrace();
            case 'locals':
                return this.handleLocals();
            case 'eval':
                return await this.handleEval(args.join(' '));
            case 'abort':
            case 'a':
                return this.handleAbort();
            case 'toplevel':
            case 'top':
                return this.handleToplevel();
            case 'up':
            case 'u':
                return this.handleFrameUp();
            case 'down':
            case 'd':
                return this.handleFrameDown();
            case 'level':
                return this.handleLevel(args);
            case 'levels':
                return this.handleLevels();
            case 'help':
            case 'h':
            case '?':
                return this.handleHelp();
            default:
                return `;; Unknown debug command: ${cmd}. Type :help for commands.`;
        }
    }

    handleDebug(args) {
        if (args.length === 0) {
            return `;; Debugging is ${this.debugRuntime.enabled ? 'ON' : 'OFF'}`;
        }
        const val = args[0].toLowerCase();
        if (val === 'on') {
            this.debugRuntime.enable();
            return ';; Debugging enabled';
        } else if (val === 'off') {
            this.debugRuntime.disable();
            let msg = ';; Debugging disabled';
            if (typeof window !== 'undefined') {
                msg += '\n;; WARNING: Fast Mode enabled. UI will freeze during long computations.';
            }
            return msg;
        }

        return ';; Usage: :debug on|off';
    }

    handleAbort() {
        if (this.backend.isPaused()) {
            this._viewedLevel = -1;
            this.backend.resolveAction('abort');
        } else {
            this.debugRuntime.abort();
        }
        return ';; Evaluation aborted';
    }

    /**
     * Handles :toplevel — pops all debug levels and returns to the top-level REPL.
     * Sets the abortAll flag on the PauseController so all nested runDebug() calls
     * will throw, and resolves all pending pause promises via resolveAction.
     * @returns {string}
     */
    handleToplevel() {
        if (!this.backend.isPaused()) return ';; Not paused';
        this._viewedLevel = -1;
        this.debugRuntime.pauseController.abortAll = true;
        // Resolve all pending action resolvers to unblock all nested levels
        while (this.backend.isPaused()) {
            this.backend.resolveAction('abort');
        }
        return ';; Returning to top level';
    }

    handleBreak(args) {
        if (args.length < 2) {
            return ';; Usage: :break <file> <line> [column]';
        }
        const filename = args[0];
        const line = parseInt(args[1], 10);
        const column = args[2] ? parseInt(args[2], 10) : null;

        if (isNaN(line)) return ';; Invalid line number';

        const id = this.debugRuntime.setBreakpoint(filename, line, column);
        return `;; Breakpoint ${id} set at ${filename}:${line}${column ? ':' + column : ''}`;
    }

    handleUnbreak(args) {
        if (args.length === 0) return ';; Usage: :unbreak <id>';
        const id = args[0];
        const success = this.debugRuntime.removeBreakpoint(id);
        return success ? `;; Breakpoint ${id} removed` : `;; Breakpoint ${id} not found`;
    }

    handleListBreakpoints() {
        const breakpoints = this.debugRuntime.breakpointManager.getAllBreakpoints();
        if (breakpoints.length === 0) return ';; No breakpoints set';

        let output = ';; Breakpoints:\n';
        for (const bp of breakpoints) {
            output += `;;   ${bp.id}: ${bp.filename}:${bp.line}${bp.column ? ':' + bp.column : ''} (${bp.enabled ? 'enabled' : 'disabled'})\n`;
        }
        return output.trim();
    }

    handleStepInto() {
        if (!this.backend.isPaused()) return ';; Not paused';
        this._viewedLevel = -1;
        this.backend.resolveAction('stepInto');
        return ';; Stepping into...';
    }

    handleStepOver() {
        if (!this.backend.isPaused()) return ';; Not paused';
        this._viewedLevel = -1;
        this.backend.resolveAction('stepOver');
        return ';; Stepping over...';
    }

    handleStepOut() {
        if (!this.backend.isPaused()) return ';; Not paused';
        this._viewedLevel = -1;
        this.backend.resolveAction('stepOut');
        return ';; Stepping out...';
    }

    handleContinue() {
        if (!this.backend.isPaused()) return ';; Not paused';
        this._viewedLevel = -1;
        this.backend.resolveAction('resume');
        return ';; Continuing...';
    }

    // =========================================================================
    // Stack & Level Helpers
    // =========================================================================

    /**
     * Synchronizes the internal selected-frame stack with the backend's
     * pause depth. Pushes -1 (select newest frame) for new levels,
     * trims if levels have been popped. Also clamps _viewedLevel.
     * @private
     */
    _syncFrameStack() {
        const depth = this.backend.getDepth();
        while (this._selectedFrameStack.length < depth) {
            this._selectedFrameStack.push(-1);
        }
        while (this._selectedFrameStack.length > depth) {
            this._selectedFrameStack.pop();
        }
        // Reset viewed level to top when levels change
        if (this._viewedLevel > depth) {
            this._viewedLevel = -1;
        }
    }

    /**
     * Gets the effective viewed level (1-indexed). Defaults to the
     * topmost level when _viewedLevel is -1.
     * @returns {number} 1-indexed level number
     * @private
     */
    _getViewedLevel() {
        this._syncFrameStack();
        const depth = this.backend.getDepth();
        if (depth === 0) return 0;
        return this._viewedLevel === -1 ? depth : this._viewedLevel;
    }

    /**
     * Gets the pause info for the currently viewed debug level.
     * @returns {Object|null} Pause info for the viewed level
     * @private
     */
    _getViewedPauseInfo() {
        const level = this._getViewedLevel();
        if (level === 0) return null;
        return this.backend._pauseInfoStack[level - 1] || null;
    }

    /**
     * Gets the stack for the currently viewed debug level from its
     * snapshotted pause info. Falls back to the live stack tracer.
     * @returns {Array} Stack frames for the viewed level
     * @private
     */
    _getPausedStack() {
        const pauseInfo = this._getViewedPauseInfo();
        if (pauseInfo && pauseInfo.stack) return pauseInfo.stack;
        return this.debugRuntime.getStack();
    }

    /**
     * Gets the selected frame index for the currently viewed level.
     * @param {Array} stack - The stack for the viewed level
     * @returns {number} Frame index (0 = oldest, stack.length-1 = newest)
     * @private
     */
    _getSelectedIndex(stack) {
        const level = this._getViewedLevel();
        if (level === 0) return stack.length - 1;
        const sel = this._selectedFrameStack[level - 1];
        if (sel === -1) return stack.length - 1;
        return Math.min(sel, stack.length - 1);
    }

    /**
     * Sets the selected frame index for the currently viewed level.
     * @param {number} idx - Frame index to select
     * @private
     */
    _setSelectedIndex(idx) {
        const level = this._getViewedLevel();
        if (level > 0) {
            this._selectedFrameStack[level - 1] = idx;
        }
    }

    // =========================================================================
    // Stack Frame Commands (scoped to current level)
    // =========================================================================

    handleBacktrace() {
        const stack = this._getPausedStack();
        if (stack.length === 0) return ';; No call stack info available';

        const totalDepth = this.backend.getDepth();
        const viewedLevel = this._getViewedLevel();
        let output = totalDepth > 1
            ? `;; Call Stack (debug level ${viewedLevel}):\n`
            : ';; Call Stack:\n';
        // newest first (0 is oldest though, so we reverse for display)
        const displayStack = [...stack].reverse();
        for (let i = 0; i < displayStack.length; i++) {
            const frame = displayStack[i];
            const realIdx = stack.length - 1 - i;
            const marker = realIdx === this._getSelectedIndex(stack) ? '=>' : '  ';
            const loc = frame.source ? `${frame.source.filename}:${frame.source.line}` : 'unknown location';
            output += `;; ${marker} #${realIdx} ${frame.name || '(anonymous procedure)'} at ${loc}\n`;
        }
        return output.trim();
    }

    handleLocals() {
        if (!this.backend.isPaused()) return ';; Not paused';
        const stack = this._getPausedStack();
        const idx = this._getSelectedIndex(stack);
        if (idx < 0 || idx >= stack.length) return ';; Invalid frame selected';

        const frame = stack[idx];
        const properties = this.debugRuntime.stateInspector.getScopeProperties(frame.env);

        if (properties.length === 0) return `;; Frame #${idx} has no local bindings`;

        let output = `;; Local variables for frame #${idx}:\n`;
        for (const prop of properties) {
            output += `;;   ${prop.name} = ${prop.value.description || prop.value.value}\n`;
        }
        return output.trim();
    }

    async handleEval(expr) {
        if (!this.backend.isPaused()) return ';; Not paused';
        if (!expr) return ';; Usage: :eval <expression>';

        const stack = this._getPausedStack();
        const idx = this._getSelectedIndex(stack);
        if (idx < 0 || idx >= stack.length) return ';; Invalid frame selected';

        const frame = stack[idx];
        const env = frame.env;

        try {
            const sexp = parse(expr)[0];

            // Reconstruct SyntacticEnv from the interpreter's Environment nameMap.
            // This allows the analyzer to correctly resolve alpha-renamed local variables.
            let syntacticEnv = null;
            let currEnv = env;
            const envChain = [];
            while (currEnv) {
                envChain.push(currEnv);
                currEnv = currEnv.parent;
            }

            for (let i = envChain.length - 1; i >= 0; i--) {
                const frame = envChain[i];
                const nextSyntacticEnv = new SyntacticEnv(syntacticEnv);
                if (frame.nameMap) {
                    for (const [orig, renamed] of frame.nameMap) {
                        nextSyntacticEnv.bindings.push({ id: intern(orig), newName: renamed });
                    }
                }
                syntacticEnv = nextSyntacticEnv;
            }

            const ast = analyze(sexp, syntacticEnv, this.interpreter.context);

            // Debugging stays enabled during eval — if the expression hits a
            // breakpoint or throws, a nested debug level is created. The
            // PauseController's execution context stack isolates stepping state,
            // and the backend's LIFO resolver stack handles nested pauses.
            const result = await this.interpreter.runDebug(ast, env, { jsAutoConvert: 'raw' });
            return `;; result: ${this.backend.formatValue(result)}`;
        } catch (e) {
            return `;; Error during eval: ${e.message}`;
        }
    }

    handleFrameUp() {
        const stack = this._getPausedStack();
        let currentIdx = this._getSelectedIndex(stack);
        if (currentIdx > 0) {
            const newIdx = currentIdx - 1;
            this._setSelectedIndex(newIdx);
            return `;; Selected frame #${newIdx}`;
        }
        return ';; Already at oldest frame';
    }

    handleFrameDown() {
        const stack = this._getPausedStack();
        let currentIdx = this._getSelectedIndex(stack);
        if (currentIdx < stack.length - 1) {
            const newIdx = currentIdx + 1;
            this._setSelectedIndex(newIdx);
            return `;; Selected frame #${newIdx}`;
        }
        return ';; Already at newest frame';
    }

    // =========================================================================
    // Debug Level Commands
    // =========================================================================

    /**
     * Handles :level [N] — switch which debug level is being viewed,
     * or show the current viewed level.
     * @param {string[]} args - Optional level number
     * @returns {string}
     */
    handleLevel(args) {
        const depth = this.backend.getDepth();
        if (depth === 0) return ';; No active debug levels';

        if (args.length === 0) {
            const viewed = this._getViewedLevel();
            return `;; Viewing debug level ${viewed} of ${depth}`;
        }

        const n = parseInt(args[0], 10);
        if (isNaN(n) || n < 1 || n > depth) {
            return `;; Invalid level: ${args[0]}. Valid range: 1..${depth}`;
        }

        this._viewedLevel = n;
        const pauseInfo = this.backend._pauseInfoStack[n - 1];
        const reason = pauseInfo?.reason || 'unknown';
        const loc = pauseInfo?.source
            ? `${pauseInfo.source.filename}:${pauseInfo.source.line}`
            : 'unknown location';
        return `;; Now viewing level ${n}: ${reason} at ${loc}`;
    }

    /**
     * Shows all active debug levels with their pause reason and location.
     * Marks the currently viewed level with =>.
     * @returns {string}
     */
    handleLevels() {
        const depth = this.backend.getDepth();
        if (depth === 0) return ';; No active debug levels';

        const viewed = this._getViewedLevel();
        let output = ';; Debug Levels:\n';
        for (let i = depth; i >= 1; i--) {
            const pauseInfo = this.backend._pauseInfoStack[i - 1];
            const marker = i === viewed ? '=>' : '  ';
            const reason = pauseInfo?.reason || 'unknown';
            const loc = pauseInfo?.source
                ? `${pauseInfo.source.filename}:${pauseInfo.source.line}`
                : 'unknown location';
            output += `;; ${marker} level ${i}: ${reason} at ${loc}\n`;
        }
        return output.trim();
    }

    // =========================================================================
    // Help
    // =========================================================================

    handleHelp() {
        return `;; Debug Commands:
;;   :debug on|off     - Enable/disable debugging
;;   :break <file> <l> [c] - Set breakpoint
;;   :unbreak <id>     - Remove breakpoint
;;   :breakpoints      - List all breakpoints
;;   :step / :s        - Step into
;;   :next / :n        - Step over
;;   :finish / :fin    - Step out
;;   :continue / :c    - Resume execution
;;
;; Frame Navigation (within viewed level):
;;   :bt / :backtrace  - Show backtrace
;;   :locals           - Show local variables
;;   :eval <expr>      - Evaluate in selected frame's scope
;;   :up / :u          - Select older stack frame
;;   :down / :d        - Select newer stack frame
;;
;; Level Navigation:
;;   :levels           - Show all debug levels
;;   :level [N]        - View/switch to debug level N
;;   :abort / :a       - Pop one debug level
;;   :toplevel / :top  - Pop all debug levels, return to REPL
;;   :help / :h / :?   - Show this help`;
    }

    /**
     * Resets all frame selection and level viewing state.
     */
    resetSelection() {
        this._selectedFrameStack = [];
        this._viewedLevel = -1;
    }
}
