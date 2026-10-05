/**
 * @fileoverview The REPL's debug commands: lines beginning with `:`.
 *
 * Each is run by the debugger's Scheme (`debugger-command` in
 * src/core/scheme/debugger.scm), which answers with the text to show. Only
 * `:eval` comes back to be done here: an expression to evaluate in a frame's
 * environment, which needs the expander to see that frame's renamed locals.
 */

import { parse } from '../core/interpreter/reader.js';
import { analyzeInEnvironment } from '../core/interpreter/expand.js';
import { systemLibrary } from '../core/interpreter/library_seed.js';
import { callSchemeProcedure } from '../core/interpreter/values.js';

/**
 * Calls a procedure of `(scheme-js debugger)`.
 * @param {string} name - The procedure's name.
 * @param {...*} args - Its arguments.
 * @returns {*}
 */
function debuggerCall(name, ...args) {
    return callSchemeProcedure(systemLibrary(['scheme-js', 'debugger']).get(name), args);
}

/**
 * Runs the REPL's debug commands.
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
    }

    /**
     * Whether a line is a debug command.
     * @param {string} input
     * @returns {boolean}
     */
    isDebugCommand(input) {
        return debuggerCall('debugger-command?', input);
    }

    /**
     * Runs a debug command.
     * @param {string} input
     * @returns {Promise<string>} What to show.
     */
    async execute(input) {
        const answer = debuggerCall('debugger-command', this.debugRuntime.scheme, input);
        if (answer !== null && typeof answer === 'object' && 'expression' in answer) {
            return this.evaluate(String(answer.expression), answer.env);
        }
        return String(answer);
    }

    /**
     * Evaluates an expression in a paused frame's environment, for `:eval`:
     * analyzed with the frame's renamed locals in view, and run to its end,
     * at no breakpoint, with the debugger set aside. The run it is evaluated
     * within is paused, and an asynchronous run here waits on that same pause
     * after its first step: before this was done so, a variable answered and
     * `(+ v 1)` never did.
     * @param {string} expression - The expression, as written.
     * @param {Environment} env - The frame's environment.
     * @returns {Promise<string>} What to show.
     */
    async evaluate(expression, env) {
        const debugRuntime = this.interpreter.debugRuntime;
        this.interpreter.debugRuntime = null;
        try {
            const ast = analyzeInEnvironment(parse(expression)[0], env, this.interpreter.context);
            const result = this.interpreter.run(ast, env, undefined, undefined, { jsAutoConvert: 'raw' });
            return String(debuggerCall('eval-answer', this.backend.formatValue(result)));
        } catch (e) {
            return String(debuggerCall('eval-failure', e.message));
        } finally {
            this.interpreter.debugRuntime = debugRuntime;
        }
    }

    /**
     * Selects the newest frame again.
     */
    resetSelection() {
        debuggerCall('reset-frame-selection!', this.debugRuntime.scheme);
    }
}
