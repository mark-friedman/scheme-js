/**
 * @fileoverview SchemeDebugRuntime: the evaluator's door into the debugger.
 *
 * The debugger's logic is Scheme, in `(scheme-js debugger)`
 * (src/core/scheme/debugger.scm): breakpoints, the calls a program is in, the
 * run's mode and whether a step stops, whether an exception breaks, where a
 * breakpoint cannot fire, the REPL's commands. It runs on the library system's
 * own interpreter, which no debugger is attached to, so it is never paused or
 * stepped itself. This class is what the evaluator holds: each hook calls a
 * procedure of that library with the runtime's `debugger` record.
 *
 * What only JavaScript can do is here, given to the Scheme as its host: the
 * promise a paused asynchronous run waits on, the backend told of pauses and
 * resumptions, having compiled code run as its closures while the program is
 * debugged, and listing an environment's bindings, the compiled procedures and
 * the macro transformers there are. And so are properties the evaluator reads
 * at every step -- `enabled`, `debugging`, `paused`, `aborted` -- which the
 * Scheme sets after each change, since a call into Scheme there would cost
 * every step of every program being debugged.
 */

import { systemLibrary } from '../core/interpreter/library_seed.js';
import { callSchemeProcedure, callWithSchemeValues, settleTailCalls, SCHEME_PRIMITIVE } from '../core/interpreter/values.js';
import { suspendFlush, restoreFlush } from '../core/interpreter/unwind.js';
import { Cons } from '../core/interpreter/cons.js';
import { ENV } from '../core/interpreter/stepables_base.js';
import { globalMacroRegistry } from '../core/interpreter/macro_registry.js';
import { isCompiledOver } from '../core/interpreter/library_registry.js';
import { getExceptionHandlerFrameClass } from '../core/interpreter/frame_registry.js';

/** The procedures `(scheme-js debugger)` exports, once it is loaded. */
let debuggerLibrary = null;

/**
 * A procedure of `(scheme-js debugger)`, loading the library the first time.
 * @param {string} name - The procedure's name.
 * @returns {Function}
 */
function debuggerProcedure(name) {
    if (debuggerLibrary === null) debuggerLibrary = systemLibrary(['scheme-js', 'debugger']);
    return debuggerLibrary.get(name);
}

/**
 * Calls a procedure of `(scheme-js debugger)`.
 * @param {string} name - The procedure's name.
 * @param {...*} args - Its arguments, as Scheme values.
 * @returns {*} What it returns.
 */
function debuggerCall(name, ...args) {
    return callSchemeProcedure(debuggerProcedure(name), args);
}

/**
 * Calls one of the procedures the evaluator calls at every step or every
 * call: through its raw entry, with compiled frames kept from moving, as a
 * primitive is called, not run on the interpreter as `callSchemeProcedure`
 * runs one, which costs some thirty times as much. What that run is for -- a
 * continuation captured, a recursion deeper than the JavaScript stack --
 * cannot happen in these, which capture none and go no deeper than the list
 * of breakpoints.
 * @param {string} name - The procedure's name.
 * @param {...*} args - Its arguments, as Scheme values.
 * @returns {*} What it returns.
 */
function hookCall(name, ...args) {
    const flush = suspendFlush();
    try {
        return settleTailCalls(callWithSchemeValues(debuggerProcedure(name), args));
    } finally {
        restoreFlush(flush);
    }
}

/**
 * A JavaScript function as a procedure the Scheme calls with Scheme values.
 * @param {Function} fn - The function.
 * @returns {Function} It, marked.
 */
function procedure(fn) {
    fn[SCHEME_PRIMITIVE] = true;
    return fn;
}

/**
 * Pairs as a Scheme list of (name . value).
 * @param {Iterable<[*, *]>} pairs - The pairs.
 * @returns {Cons|null}
 */
function alist(pairs) {
    let list = null;
    for (const [name, value] of [...pairs].reverse()) list = new Cons(new Cons(name, value), list);
    return list;
}

/**
 * The debugger a program is debugged with.
 */
export class SchemeDebugRuntime {
    /**
     * @param {Object} [options] - Configuration options
     * @param {Function} [options.onPause] - Called when the program pauses.
     * @param {Function} [options.onResume] - Called when it resumes.
     */
    constructor(options = {}) {
        this.onPause = options.onPause || null;
        this.onResume = options.onResume || null;

        /** @type {DebugBackend|null} */
        this.backend = null;
        /** @type {Object|null} The interpreter debugged. */
        this.interpreter = null;

        /** @type {boolean} Whether debugging is on. */
        this.enabled = false;
        /** @type {boolean} Whether the program is being debugged. */
        this.debugging = false;
        /** @type {boolean} Whether it is paused. */
        this.paused = false;
        /** @type {boolean} Whether its run was aborted. */
        this.aborted = false;

        /** @type {Function|null} Lets the asynchronous run waiting on a pause go on. */
        this.waiting = null;
        /**
         * The `debugger` record, made the first time it is needed, so that a
         * runtime attached and never used, as the CLI's is, loads nothing.
         * @type {Object|null}
         */
        this.record = null;
    }

    /**
     * The `debugger` record, made at the first use.
     * @type {Object}
     */
    get scheme() {
        if (this.record === null) this.record = debuggerCall('make-debugger', this.host());
        return this.record;
    }

    /**
     * The host the Scheme is given: what only JavaScript can do.
     * @returns {Object} A `debugger-host`.
     */
    host() {
        return debuggerCall('make-debugger-host',
            procedure((enabled, debugging, paused, aborted, interpretation) => {
                this.enabled = enabled;
                this.debugging = debugging;
                this.paused = paused;
                this.aborted = aborted;
                this.interpreter?.interpretForDebugger?.(interpretation);
            }),
            procedure(() => {
                const waiting = this.waiting;
                this.waiting = null;
                waiting?.();
            }),
            procedure((how) => this.onResume?.(String(how))),
            procedure((info) => this.onPause?.(info)),
            procedure((env) => alist(env?.bindings ?? [])),
            procedure(() => alist(this.compiledProcedures())),
            procedure(() => alist(this.macroTransformers())));
    }

    /**
     * Sets the debug backend, which is told of pauses and resumptions.
     * @param {DebugBackend} backend
     */
    setBackend(backend) {
        this.backend = backend;
        this.onPause = (info) => backend.onPause(info);
        this.onResume = (how) => backend.onResume(how);
        if (backend.onScriptLoaded) {
            this.onScriptLoaded = (info) => backend.onScriptLoaded(info);
        }
    }

    // =========================================================================
    // Exceptions
    // =========================================================================

    /** @type {boolean} Whether an exception a handler will catch pauses. */
    get breakOnCaughtException() {
        return debuggerCall('debugger-breaks-on-caught?', this.scheme);
    }

    set breakOnCaughtException(value) {
        debuggerCall('set-debugger-breaks-on-caught!', this.scheme, value);
    }

    /** @type {boolean} Whether an exception none will catch pauses. */
    get breakOnUncaughtException() {
        return debuggerCall('debugger-breaks-on-uncaught?', this.scheme);
    }

    set breakOnUncaughtException(value) {
        debuggerCall('set-debugger-breaks-on-uncaught!', this.scheme, value);
    }

    /**
     * Whether an exception raised now pauses the program. Whether a handler
     * will catch it is read from the frame stack, the evaluator's.
     * @param {*} exception - What was raised.
     * @param {Array} fstack - The frame stack.
     * @returns {boolean}
     */
    shouldBreakOnException(exception, fstack) {
        if (!this.enabled) return false;
        const ExceptionHandlerFrame = getExceptionHandlerFrameClass();
        const caught = Array.isArray(fstack) && ExceptionHandlerFrame !== undefined
            && fstack.some((frame) => frame instanceof ExceptionHandlerFrame);
        return debuggerCall('breaks-on-exception?', this.scheme, caught);
    }

    // =========================================================================
    // Breakpoints
    // =========================================================================

    /**
     * Sets a breakpoint.
     * @param {string} filename - Source file path
     * @param {number} line - Line number (1-indexed)
     * @param {number} [column] - Column (1-indexed), or none for the line.
     * @returns {string} Its id.
     */
    setBreakpoint(filename, line, column = null) {
        return String(debuggerCall('add-breakpoint!', this.scheme, filename, line, column ?? false));
    }

    /**
     * Removes a breakpoint.
     * @param {string} id - Its id.
     * @returns {boolean} Whether there was one.
     */
    removeBreakpoint(id) {
        return debuggerCall('remove-breakpoint!', this.scheme, id);
    }

    /**
     * The breakpoints.
     * @returns {Array<{id: string, filename: string, line: number, column: number|null}>}
     */
    getAllBreakpoints() {
        return debuggerCall('breakpoints->js', this.scheme);
    }

    // =========================================================================
    // Compiled code and transformers
    // =========================================================================

    /**
     * Connects this runtime to the interpreter it debugs, which it tells
     * whether the program is being debugged (`Interpreter.interpretForDebugger`).
     * Called by `Interpreter.setDebugRuntime`.
     * @param {Object} interpreter - The interpreter.
     */
    attachInterpreter(interpreter) {
        this.interpreter = interpreter;
        this.updateInterpretation();
    }

    /**
     * Tells the interpreter again whether the program is being debugged, so
     * that compiled code runs as the closures it replaced while it is.
     */
    updateInterpretation() {
        if (this.record === null) this.interpreter?.interpretForDebugger?.(false);
        else debuggerCall('debugger-changed!', this.record);
    }

    /**
     * The compiled procedures bound at top level that do not run as closures
     * while the program is debugged, with their spans: those a breakpoint is
     * accepted in and never fires. A procedure compiled over a closure -- the
     * standard library's, or one the tier compiled -- runs as the closure
     * then, so is not one. Every procedure nested in a compiled one is
     * compiled with it, inside its span, so the top level is enough.
     * @returns {Array<[string, Object]>} (name, span) pairs.
     */
    compiledProcedures() {
        const found = [];
        for (let env = this.interpreter?.globalEnv; env; env = env.parent) {
            for (const value of env.bindings.values()) {
                if (typeof value !== 'function' || value.$compiled !== true || isCompiledOver(value)) continue;
                if (value.source) found.push([value.schemeName ?? 'anonymous', value.source]);
            }
        }
        return found;
    }

    /**
     * The `define-macro` transformers the interpreter's analysis can reach,
     * with their spans: a breakpoint in one is accepted and never fires, since
     * a transformer runs while code is expanded, before any of it runs, on an
     * interpreter with no debugger (`analyzeDefineMacro`).
     * @returns {Array<[string, Object]>} (name, span) pairs.
     */
    macroTransformers() {
        const context = this.interpreter?.context;
        const seen = new Set();
        const found = [];
        for (const start of [context?.currentMacroRegistry, context?.macroRegistry, globalMacroRegistry]) {
            for (let registry = start; registry && !seen.has(registry); registry = registry.parent) {
                seen.add(registry);
                for (const [name, transformer] of registry.macros) {
                    const source = transformer?.transformerProcedure?.source;
                    if (source) found.push([name, source]);
                }
            }
        }
        return found;
    }

    /**
     * The compiled procedure innermost around a location, in which a
     * breakpoint never fires, or null.
     * @param {string} filename - Source file path.
     * @param {number} line - Line number (1-indexed).
     * @param {number|null} [column] - Column (1-indexed), or null for a line.
     * @returns {{name: string, source: Object}|null}
     */
    compiledProcedureAt(filename, line, column = null) {
        return debuggerCall('compiled-procedure-at', this.scheme, filename, line, column ?? false);
    }

    /**
     * The macro transformer innermost around a location, in which a
     * breakpoint never fires, or null.
     * @param {string} filename - Source file path.
     * @param {number} line - Line number (1-indexed).
     * @param {number|null} [column] - Column (1-indexed), or null for a line.
     * @returns {{name: string, source: Object}|null}
     */
    macroTransformerAt(filename, line, column = null) {
        return debuggerCall('transformer-at', this.scheme, filename, line, column ?? false);
    }

    // =========================================================================
    // Running, paused and stepping
    // =========================================================================

    /** Resumes the program. */
    resume() {
        debuggerCall('resume!', this.scheme);
    }

    /** Steps into whatever runs next. */
    stepInto() {
        debuggerCall('step-into!', this.scheme);
    }

    /** Steps over the current call. */
    stepOver() {
        debuggerCall('step-over!', this.scheme);
    }

    /** Steps out of the current call. */
    stepOut() {
        debuggerCall('step-out!', this.scheme);
    }

    /** Aborts the run. */
    abort() {
        debuggerCall('abort!', this.scheme);
    }

    /**
     * Whether the program is paused.
     * @returns {boolean}
     */
    isPaused() {
        return this.paused;
    }

    /**
     * Whether the run was aborted.
     * @returns {boolean}
     */
    isAborted() {
        return this.aborted;
    }

    /**
     * A promise that settles when the paused program resumes, which the
     * asynchronous run waits on.
     * @returns {Promise<void>}
     */
    waitForResume() {
        if (!this.paused) return Promise.resolve();
        return new Promise((resolve) => { this.waiting = resolve; });
    }

    /**
     * The run's state.
     * @returns {{state: string, reason: string|null, data: *}}
     */
    getPauseState() {
        return debuggerCall('pause-state->js', this.scheme);
    }

    // =========================================================================
    // The evaluator's hooks
    // =========================================================================

    /**
     * Whether to pause before a step at a location.
     * @param {Object} source - The location.
     * @param {Object} env - The environment there.
     * @returns {boolean}
     */
    shouldPause(source, env) {
        return this.enabled && source != null
            && hookCall('should-pause?', this.scheme, source.filename, source.line, source.column ?? false);
    }

    /**
     * Pauses before a step, telling the backend.
     * @param {Object|null} source - Where.
     * @param {Object|null} env - The environment there.
     * @param {string} [reason] - Why: by default, the breakpoint there, or
     *   the step in progress.
     */
    pause(source, env, reason = null) {
        debuggerCall('pause-at!', this.scheme, source ?? false, env ?? false, reason ?? false);
    }

    /**
     * Pauses at an exception raised, telling the backend.
     * @param {Object} raiseNode - The RaiseNode raising it.
     * @param {Array} registers - The evaluator's registers.
     * @returns {boolean} True.
     */
    pauseOnException(raiseNode, registers) {
        return debuggerCall('pause-on-exception!', this.scheme, raiseNode.source ?? false,
            registers[ENV] ?? false, raiseNode.exception, raiseNode.continuable === true);
    }

    /**
     * A call to a closure begun.
     * @param {{name: string, source: Object|null, env: Object}} frameInfo
     */
    enterFrame(frameInfo) {
        hookCall('enter-activation!', this.scheme, frameInfo.name, frameInfo.source ?? false, frameInfo.env);
    }

    /**
     * A call begun in tail position, replacing the newest.
     * @param {{name: string, source: Object|null, env: Object}} frameInfo
     */
    replaceFrame(frameInfo) {
        hookCall('replace-activation!', this.scheme, frameInfo.name, frameInfo.source ?? false, frameInfo.env);
    }

    /** The newest call returned. */
    exitFrame() {
        if (this.record !== null) hookCall('exit-activation!', this.record);
    }

    // =========================================================================
    // State
    // =========================================================================

    /**
     * The calls the program is in, oldest first.
     * @returns {Array<{name: string, source: Object|null, env: Object, tcoCount: number}>}
     */
    getStack() {
        return debuggerCall('activations->js', this.scheme);
    }

    /**
     * How many calls the program is in.
     * @returns {number}
     */
    getDepth() {
        return this.record === null ? 0 : Number(debuggerCall('debugger-depth', this.record));
    }

    /**
     * The newest call, or null.
     * @returns {Object|null}
     */
    getCurrentFrame() {
        const stack = this.getStack();
        return stack.length === 0 ? null : stack[stack.length - 1];
    }

    // =========================================================================
    // Debug control
    // =========================================================================

    /** Turns debugging on. */
    enable() {
        debuggerCall('set-debugger-enabled!', this.scheme, true);
    }

    /** Turns debugging off: the evaluator runs with no debugging checks. */
    disable() {
        debuggerCall('set-debugger-enabled!', this.scheme, false);
    }

    /** Forgets every breakpoint, call, step and pause. */
    reset() {
        debuggerCall('reset-debugger!', this.scheme);
    }
}

export default SchemeDebugRuntime;
