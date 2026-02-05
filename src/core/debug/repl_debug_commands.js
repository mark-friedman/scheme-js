/**
 * @fileoverview REPL Debug Commands parser and executor.
 */

import { parse } from '../interpreter/reader.js';
import { analyze, SyntacticEnv } from '../interpreter/analyzer.js';
import { prettyPrint } from '../interpreter/printer.js';
import { intern } from '../interpreter/symbol.js';

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
        this.selectedFrameIndex = -1; // -1 means newest (top) frame
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
     * @returns {string} Output message
     */
    execute(input) {
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
                return this.handleEval(args.join(' '));
            case 'up':
                return this.handleFrameUp();
            case 'down':
                return this.handleFrameDown();
            case 'help':
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
            return ';; Debugging disabled';
        }
        return ';; Usage: :debug on|off';
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
        this.debugRuntime.stepInto();
        return ';; Stepping into...';
    }

    handleStepOver() {
        if (!this.backend.isPaused()) return ';; Not paused';
        this.debugRuntime.stepOver();
        return ';; Stepping over...';
    }

    handleStepOut() {
        if (!this.backend.isPaused()) return ';; Not paused';
        this.debugRuntime.stepOut();
        return ';; Stepping out...';
    }

    handleContinue() {
        if (!this.backend.isPaused()) return ';; Not paused';
        this.debugRuntime.resume();
        return ';; Continuing...';
    }

    handleBacktrace() {
        const stack = this.debugRuntime.getStack();
        if (stack.length === 0) return ';; No call stack info available';

        let output = ';; Call Stack:\n';
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
        const stack = this.debugRuntime.getStack();
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

    handleEval(expr) {
        if (!this.backend.isPaused()) return ';; Not paused';
        if (!expr) return ';; Usage: :eval <expression>';

        const stack = this.debugRuntime.getStack();
        const idx = this._getSelectedIndex(stack);
        if (idx < 0 || idx >= stack.length) return ';; Invalid frame selected';

        const frame = stack[idx];
        const env = frame.env;

        try {
            const sexp = parse(expr)[0];

            // Reconstruct SyntacticEnv from the interpreter's Environment nameMap
            // This allows the analyzer to correctly resolve alpha-renamed local variables.
            let syntacticEnv = null;
            let currEnv = env;
            const envChain = [];
            while (currEnv) {
                envChain.push(currEnv);
                currEnv = currEnv.parent;
            }

            // Reconstruct SyntacticEnv from global upwards
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
            const result = this.interpreter.run(ast, env, undefined, undefined, { jsAutoConvert: 'raw' });
            return `;; result: ${this.backend.formatValue(result)}`;
        } catch (e) {
            return `;; Error during eval: ${e.message}`;
        }
    }

    handleFrameUp() {
        const stack = this.debugRuntime.getStack();
        let currentIdx = this._getSelectedIndex(stack);
        if (currentIdx > 0) {
            this.selectedFrameIndex = currentIdx - 1;
            return `;; Selected frame #${this.selectedFrameIndex}`;
        }
        return ';; Already at oldest frame';
    }

    handleFrameDown() {
        const stack = this.debugRuntime.getStack();
        let currentIdx = this._getSelectedIndex(stack);
        if (currentIdx < stack.length - 1) {
            this.selectedFrameIndex = currentIdx + 1;
            return `;; Selected frame #${this.selectedFrameIndex}`;
        }
        return ';; Already at newest frame';
    }

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
;;   :bt / :backtrace  - Show backtrace
;;   :locals           - Show local variables
;;   :eval <expr>      - Evaluate in selected frame's scope
;;   :up / :down       - Navigate stack frames
;;   :help             - Show this help`;
    }

    _getSelectedIndex(stack) {
        if (this.selectedFrameIndex === -1) return stack.length - 1;
        return Math.min(this.selectedFrameIndex, stack.length - 1);
    }

    /**
     * Resets the selected frame to the top.
     */
    resetSelection() {
        this.selectedFrameIndex = -1;
    }
}
