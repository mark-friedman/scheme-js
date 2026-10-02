/**
 * Scheme Runtime Value Types
 * 
 * These classes and factory functions represent first-class values in the Scheme runtime
 * that are not primitive JavaScript values. They are used for closures, continuations,
 * and multiple return values.
 * 
 * IMPORTANT: Closures and Continuations are now created as callable JavaScript functions
 * with marker properties. This allows them to be called directly from JavaScript code
 * anywhere they appear (in variables, arrays, objects, Maps, etc.).
 */

import { suspendFlush, restoreFlush } from './unwind.js';
import { LiteralNode, TailAppNode } from './ast_nodes.js';
import { Cons } from './cons.js';
import { SchemeError } from './errors.js';
import { jsToScheme, schemeToJsDeep } from './js_interop.js';

// =============================================================================
// Marker Symbols for Type Identification
// =============================================================================

/**
 * Symbol used to mark callable functions as Scheme closures.
 * @type {symbol}
 */
export const SCHEME_CLOSURE = Symbol.for('scheme.closure');

/**
 * Symbol used to mark callable functions as Scheme continuations.
 * @type {symbol}
 */
export const SCHEME_CONTINUATION = Symbol.for('scheme.continuation');

/**
 * Symbol used to mark JavaScript functions as Scheme-aware primitives.
 * @type {symbol}
 */
export const SCHEME_PRIMITIVE = Symbol.for('scheme.primitive');

/**
 * Symbol naming a closure's entry point for callers that already speak Scheme.
 *
 * A Scheme closure is represented as a callable JavaScript function so that it
 * can be handed to `addEventListener` and friends. Calling it therefore means
 * crossing *into* Scheme from JavaScript, and the wrapper converts accordingly:
 * arguments through `jsToScheme`, the result through `unpackForJs`.
 *
 * The compiler tier is not a JavaScript caller. It holds Scheme values and
 * expects Scheme values back, so it needs an entry that converts nothing --
 * otherwise an exact integer comes back as a double and a bignum outside the
 * safe integer range throws. That is what this symbol names.
 *
 * @type {symbol}
 */
export const SCHEME_RAW_CALL = Symbol.for('scheme.rawCall');

/**
 * Symbol naming a closure's entry point for primitives that hold Scheme values
 * and must also supply `this`: a method call through dot notation, a
 * `define-class` constructor body, a `super.method` call.
 *
 * Like `SCHEME_RAW_CALL` it converts nothing, so an integer-valued flonum stays
 * a flonum, a vector arrives as the caller's own vector rather than a copy,
 * and a bignum beyond 2^53 does not throw. Unlike it, it binds `this`, and it
 * marks the nested run's boundary exactly as the JavaScript-facing wrapper
 * does, since those primitives are reached the same way a JavaScript caller
 * is. Called as `closure[SCHEME_RAW_METHOD_CALL](thisArg, args)`.
 *
 * @type {symbol}
 */
export const SCHEME_RAW_METHOD_CALL = Symbol.for('scheme.rawMethodCall');

// =============================================================================
// Type Checking Functions
// =============================================================================

/**
 * Checks if a value is a Scheme closure (callable function with closure marker).
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isSchemeClosure(x) {
    return typeof x === 'function' && x[SCHEME_CLOSURE] === true;
}

/**
 * Checks if a value is a Scheme continuation (callable function with continuation marker).
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isSchemeContinuation(x) {
    return typeof x === 'function' && x[SCHEME_CONTINUATION] === true;
}

/**
 * Checks if a value is a Scheme-aware function (closure, continuation, or primitive).
 * Used by the interop layer to decide whether to auto-convert arguments.
 * @param {*} x - Value to check
 * @returns {boolean}
 */
export function isSchemePrimitive(x) {
    return typeof x === 'function' && (
        x[SCHEME_PRIMITIVE] === true ||
        x[SCHEME_CLOSURE] === true ||
        x[SCHEME_CONTINUATION] === true
    );
}

// =============================================================================
// Factory Functions
// =============================================================================

/**
 * The arguments JavaScript gives a procedure, fitted to its parameters.
 *
 * A Scheme procedure called with the wrong number of arguments signals an
 * error; a JavaScript function takes what it is given, and JavaScript calls
 * functions so -- an event handler with the event, `Array.prototype.map`'s
 * callback with an index and the array. So a procedure JavaScript calls takes
 * the arguments it has parameters for, as a JavaScript function would: those
 * beyond them are dropped, and a parameter given none is undefined.
 *
 * @param {Array<*>} args - The arguments JavaScript gave.
 * @param {number} required - How many parameters the procedure has, besides
 *   a rest parameter.
 * @param {boolean} rest - Whether it has a rest parameter, which takes any
 *   arguments beyond them.
 * @returns {Array<*>} As many arguments as the procedure takes.
 */
function fitToParameters(args, required, rest) {
    if (args.length === required || (rest && args.length > required)) return args;
    const fitted = new Array(required);
    for (let i = 0; i < required; i++) fitted[i] = args[i];
    return fitted;
}

/**
 * Arguments fitted to a Scheme procedure's parameters, as a JavaScript
 * caller's are (`fitToParameters`), for a caller that passes them as
 * JavaScript does -- a class's constructor, passing its arguments on to its
 * parent's, as `super(...args)` does. Anything else is given them as they are.
 * @param {Function} proc - The procedure.
 * @param {Array<*>} args - The arguments.
 * @returns {Array<*>} The arguments it takes.
 */
export function argumentsFittedTo(proc, args) {
    if (proc[SCHEME_CLOSURE] === true) {
        return fitToParameters(args, proc.params.length, proc.restParam !== null && proc.restParam !== undefined);
    }
    if (proc.$compiled === true) return fitToParameters(args, proc[SCHEME_RAW_CALL].length, proc.$rest === true);
    return args;
}

/**
 * Creates a callable Scheme closure.
 * 
 * The returned function can be called directly from JavaScript and will
 * invoke the Scheme interpreter to execute the closure body.
 * 
 * @param {Array<string>} params - Parameter names.
 * @param {Executable} body - The body AST node.
 * @param {Environment} env - The captured lexical environment.
 * @param {string|null} restParam - Name of rest parameter, or null if none.
 * @param {Interpreter} interpreter - The interpreter instance.
 * @param {string} [name='anonymous'] - Optional name for debugging.
 * @param {Object} [source=null] - Optional source location.
 * @param {Array<string>} [originalParams] - Original parameter names.
 * @param {string|null} [originalRestParam] - Original rest parameter name.
 * @returns {Function} A callable function representing the Scheme closure.
 */
export function createClosure(params, body, env, restParam, interpreter, name = 'anonymous', source = null, originalParams = null, originalRestParam = null) {
    // Create the callable wrapper
    const closure = function (...jsArgs) {
        // Build the invocation AST: apply this closure to the given args
        // Normalize args entering Scheme from JS
        const argLiterals = fitToParameters(jsArgs, params.length, restParam !== null && restParam !== undefined)
            .map(val => new LiteralNode(jsToScheme(val)));
        const ast = new TailAppNode(new LiteralNode(closure), argLiterals);

        // Run through the interpreter with a sentinel frame to capture result.
        // Unpacking will respect the default interop policy (deep conversion by default).
        return interpreter.runWithSentinel(ast, this);
    };

    // Entry point for callers that already hold Scheme values -- today, the
    // compiler tier. The wrapper above exists for JavaScript callers and so
    // converts in both directions; routing compiled code through it silently
    // turned exact integers into doubles and threw on bignums beyond 2^53.
    closure[SCHEME_RAW_CALL] = function (...schemeArgs) {
        const ast = new TailAppNode(
            new LiteralNode(closure),
            schemeArgs.map((value) => new LiteralNode(value)));
        return interpreter.runWithSentinel(
            ast, undefined, { jsAutoConvert: 'raw', compiledBoundary: true });
    };

    // Entry point for primitives that hold Scheme values and bind `this`.
    closure[SCHEME_RAW_METHOD_CALL] = function (thisArg, schemeArgs) {
        const ast = new TailAppNode(
            new LiteralNode(closure),
            schemeArgs.map((value) => new LiteralNode(value)));
        return interpreter.runWithSentinel(ast, thisArg, { jsAutoConvert: 'raw' });
    };

    // Attach marker and closure data
    closure[SCHEME_CLOSURE] = true;
    closure.params = params;
    closure.body = body;
    closure.env = env;
    closure.restParam = restParam;

    // Set function name safely (function.name is normally read-only)
    Object.defineProperty(closure, 'name', { value: name, configurable: true });
    closure.schemeName = name; // Also store in custom property for clarity
    closure.source = source;
    closure.originalParams = originalParams || params;
    closure.originalRestParam = originalRestParam || restParam;
    // Calls left before the compiler tier compiles this closure, counted down
    // as it is applied; 0 when it is not waiting to be compiled, which is
    // every closure but a program's top-level procedures (`src/compiler/tiering.js`).
    // Set here, so that every closure has the same shape.
    closure.tierCountdown = 0;

    // Custom toString for pretty-printing
    closure.toString = () => `#<procedure${name !== 'anonymous' ? ' ' + name : ''}>`;

    return closure;
}

/**
 * The interpreter each global environment belongs to, for compiled code
 * called from JavaScript, which runs on it (`createCompiledProcedure`).
 * @type {WeakMap<Object, Object>}
 */
const interpreterOfGlobalEnvironment = new WeakMap();

/**
 * Records the interpreter a global environment belongs to. Called by the
 * interpreter when it is given one.
 * @param {Environment} env - A global environment.
 * @param {Interpreter} interpreter - Its interpreter.
 */
export function registerGlobalEnvironment(env, interpreter) {
    interpreterOfGlobalEnvironment.set(env, interpreter);
}

/**
 * The interpreter an environment belongs to: that of the global environment
 * it is inside, as a library's environment is inside the global environment
 * of the interpreter that loaded it.
 * @param {Environment} env - The environment.
 * @returns {Interpreter} The interpreter.
 * @throws {SchemeError} If it is inside no interpreter's global environment.
 */
function interpreterOf(env) {
    let root = env;
    while (root.parent) root = root.parent;
    const interpreter = interpreterOfGlobalEnvironment.get(root);
    if (interpreter === undefined) {
        throw new SchemeError('a compiled procedure was called from JavaScript, but its environment belongs to no interpreter');
    }
    return interpreter;
}

/**
 * Creates the procedure a compiled procedure's code is the raw entry of: the
 * value Scheme holds and JavaScript is given.
 *
 * Its plain call faces JavaScript as an interpreted closure's does
 * (`createClosure`): its arguments converted into Scheme, the call run on an
 * interpreter -- so that pending tail calls run to a value, and a capture or a
 * move of compiled frames to the heap finishes within the call -- and its
 * result converted out. Code that holds Scheme values -- compiled code, the
 * interpreter, `callSchemeProcedure` -- calls the raw entry instead
 * (`SCHEME_RAW_CALL`), which takes and returns Scheme values and may return a
 * pending tail call or the unwind sentinel.
 *
 * The interpreter it runs on is the one whose global environment the
 * procedure's is inside: the program's, or for the compiler's own procedures
 * the compiler's.
 *
 * @param {Function} raw - The compiled code.
 * @param {Environment} env - The environment the procedure closes over.
 * @returns {Function} The procedure.
 */
export function createCompiledProcedure(raw, env) {
    const procedure = function (...jsArgs) {
        const ast = new TailAppNode(
            new LiteralNode(procedure),
            fitToParameters(jsArgs, raw.length, procedure.$rest === true)
                .map((value) => new LiteralNode(jsToScheme(value))));
        return interpreterOf(env).runWithSentinel(ast, this);
    };
    procedure[SCHEME_RAW_CALL] = raw;
    // For `callSchemeProcedure`, which runs it on the same interpreter.
    procedure.$environment = env;
    return procedure;
}

/**
 * What a continuation is invoked with: nothing, one value, or several.
 * @param {Array<*>} args - The arguments it was called with.
 * @returns {*} The value.
 */
function continuationValue(args) {
    if (args.length === 0) return null;
    if (args.length === 1) return args[0];
    return new Values(args);
}

/**
 * Creates a callable Scheme continuation.
 * 
 * The returned function can be called directly from JavaScript and will
 * invoke the continuation, rewinding the Scheme stack appropriately.
 * 
 * @param {Array} fstack - The captured frame stack (will be copied).
 * @param {Interpreter} interpreter - The interpreter instance.
 * @returns {Function} A callable function representing the Scheme continuation.
 */
export function createContinuation(fstack, interpreter) {
    // Create the callable wrapper
    // Arguments entering Scheme from JavaScript are converted, as a closure's
    // are.
    const continuation = function (...jsArgs) {
        return interpreter.invokeContinuation(continuation, continuationValue(jsArgs.map(jsToScheme)), this);
    };

    // Attach marker and continuation data
    continuation[SCHEME_CONTINUATION] = true;
    continuation.fstack = [...fstack];  // Store a copy
    // For callers that hold Scheme values (`invokeWithSchemeValues`): a
    // property rather than an entry of its own, since a program may capture a
    // continuation at every call.
    continuation.interpreter = interpreter;

    // Custom toString for pretty-printing, one function for every continuation.
    continuation.toString = continuationText;

    return continuation;
}

/**
 * How a continuation shows itself to JavaScript, as its `toString`.
 * @returns {string} Its text.
 */
function continuationText() {
    return '#<continuation>';
}

/**
 * Invokes a continuation with Scheme values, converting nothing: what compiled
 * code, a primitive and `callSchemeProcedure` do, where JavaScript's plain call
 * of the continuation converts its arguments.
 * @param {Function} continuation - The continuation.
 * @param {Array<*>} args - Scheme values.
 * @returns {*} What the run it is invoked in returns.
 */
function invokeWithSchemeValues(continuation, args) {
    return continuation.interpreter.invokeContinuation(continuation, continuationValue(args), undefined,
        { jsAutoConvert: 'raw' });
}

// =============================================================================
// Legacy Classes (Kept for reference and internal data access)
// =============================================================================

/**
 * Base class for all procedures.
 * @deprecated Use isSchemeClosure/isSchemeContinuation for type checking
 */
export class Procedure { }

/**
 * Closure data class - kept for backward compatibility and documentation.
 * 
 * @deprecated Closures are now created via createClosure() and are callable functions.
 *             Use isSchemeClosure() to check if a value is a Scheme closure.
 */
export class Closure extends Procedure {
    /**
     * @param {Array<string>} params - Parameter names.
     * @param {Executable} body - The body AST node.
     * @param {Environment} env - The captured lexical environment.
     * @param {string|null} restParam - Name of rest parameter, or null if none.
     */
    constructor(params, body, env, restParam = null) {
        super();
        this.params = params;
        this.body = body;
        this.env = env;
        this.restParam = restParam;
    }

    toString() {
        return "#<procedure>";
    }
}

/**
 * Continuation data class - kept for backward compatibility and documentation.
 * 
 * @deprecated Continuations are now created via createContinuation() and are callable functions.
 *             Use isSchemeContinuation() to check if a value is a Scheme continuation.
 */
export class Continuation {
    /**
     * @param {Array} fstack - The captured frame stack (copied).
     */
    constructor(fstack) {
        this.fstack = [...fstack];
    }

    toString() {
        return "#<continuation>";
    }
}

// =============================================================================
// Other Value Types
// =============================================================================

/**
 * TailCall "Thunk" for trampoline control flow.
 * Returned by primitives to request the interpreter to perform a tail call.
 */
export class TailCall {
    /**
     * @param {*} func - The target (Closure, AST node, etc.)
     * @param {Array} args - Arguments for the call.
     */
    constructor(func, args) {
        this.func = func;
        this.args = args;
    }
}

/**
 * Drives a value returned by a Scheme procedure to completion.
 *
 * A primitive that calls a Scheme procedure and then uses the result has to do
 * this first. A *compiled* procedure signals a tail call by returning a
 * `TailCall` instead of a value, so a primitive that treats what it gets back
 * as the answer is holding a promise to make a call, not the call's result.
 *
 * That is not a theoretical hazard. `call-with-input-file` read
 * `try { return proc(port); } finally { port.close(); }`, so when `proc` was
 * compiled and ended in a tail call, the port closed before the call ran and
 * the program failed with "port is closed" -- which is what the `read1`
 * benchmark did under the compiler tier while passing under the interpreter.
 *
 * Only a `TailCall` naming a procedure can be continued here. One carrying an
 * expression for the interpreter to evaluate cannot be, since a primitive has
 * no evaluator; it is returned as it arrived, which is what happened before
 * this existed.
 *
 * @param {*} result - What the procedure returned.
 * @returns {*} The settled value.
 */
export function settleTailCalls(result) {
    if (!(result instanceof TailCall)) return result;
    // A primitive is the caller here, which could not receive the unwind that
    // moves compiled frames to the heap stack.
    const flush = suspendFlush();
    try {
        while (result instanceof TailCall && typeof result.func === 'function') {
            result = callWithSchemeValues(result.func, result.args || []);
        }
    } finally {
        restoreFlush(flush);
    }
    return result;
}

/**
 * Calls a Scheme procedure from JavaScript that holds Scheme values and wants
 * a Scheme value back, converting nothing either way: public, for any
 * JavaScript, and what the evaluator's hooks and the door into the compiler
 * (`src/compiler/lowering.js`) use. A procedure's plain call is exactly this
 * with `jsToScheme` on its arguments and `schemeToJsDeep` on its result
 * (`docs/Interoperability.md`).
 *
 * Otherwise it does what the plain call does. An interpreted closure or a
 * compiled procedure runs on the interpreter its environment belongs to, as
 * its plain call does, so pending tail calls run to a value, a recursion
 * deeper than the JavaScript stack moves to the interpreter's heap, and a
 * continuation captured inside works. Anything else -- a primitive, a
 * continuation, whose raw entry runs it on its own interpreter, or a function
 * of JavaScript's own -- is called directly, its pending tail calls run, with
 * compiled frames kept from moving to the heap beneath it, since the unwind
 * that moves them would come back here as its result.
 *
 * Called by the evaluator, the tier's Scheme therefore runs on the compiler's
 * own interpreter, never on the program's, so the program's debugger, which
 * can pause only that interpreter's run, never sees it.
 *
 * @param {Function} proc - A Scheme procedure.
 * @param {Array<*>} args - Scheme values.
 * @returns {*} Its result, a Scheme value.
 */
export function callSchemeProcedure(proc, args) {
    const env = proc.$compiled === true ? proc.$environment
        : proc[SCHEME_CLOSURE] === true ? proc.env : undefined;
    if (env !== undefined) {
        const ast = new TailAppNode(new LiteralNode(proc), args.map((value) => new LiteralNode(value)));
        return interpreterOf(env).runWithSentinel(ast, undefined, { jsAutoConvert: 'raw' });
    }
    const flush = suspendFlush();
    try {
        return settleTailCalls(callWithSchemeValues(proc, args));
    } finally {
        restoreFlush(flush);
    }
}

/**
 * Calls a procedure with Scheme values, from code that holds them and is not
 * the interpreter: compiled code, a primitive settling a tail call.
 *
 * A function with a raw entry is called through it -- an interpreted closure
 * or a compiled procedure that way skips the conversions its JavaScript-facing
 * plain call makes. A function without one is a primitive, a continuation, or
 * JavaScript's own, and is called by `callForeign`.
 *
 * @param {Function} fn - The callee.
 * @param {Array<*>} args - Scheme values.
 * @returns {*} Its result, which may be a pending `TailCall`.
 */
export function callWithSchemeValues(fn, args) {
    const raw = fn[SCHEME_RAW_CALL];
    return raw !== undefined ? raw(...args) : callForeign(fn, args);
}

/**
 * Calls a function with no raw entry, from code holding Scheme values: a
 * continuation with its arguments unconverted (`invokeWithSchemeValues`), a
 * procedure marked `SCHEME_PRIMITIVE` directly, and a JavaScript function as
 * the interpreter calls one. Its arguments are converted to
 * JavaScript values throughout (`schemeToJsDeep`), an exact integer to a
 * number and a mutable string to the characters it holds, and its result back
 * one level, as `js-invoke` converts it: an integral number to an exact
 * integer -- always, as the interpreter converts both for the tail calls
 * compiled code hands it. Compiled frames may
 * not move to the heap while it runs, since a compiled procedure it calls back
 * would find no interpreter beneath it to finish the move.
 *
 * @param {Function} fn - The callee.
 * @param {Array<*>} args - Scheme values.
 * @returns {*} Its result.
 */
export function callForeign(fn, args) {
    if (fn[SCHEME_CONTINUATION] === true) return invokeWithSchemeValues(fn, args);
    if (isSchemePrimitive(fn)) return fn(...args);
    const flush = suspendFlush();
    try {
        return jsToScheme(fn(...args.map((a) => schemeToJsDeep(a))));
    } finally {
        restoreFlush(flush);
    }
}

/**
 * Whether a function takes and returns Scheme values, so that a primitive
 * calling it should pass its arguments unconverted: an interpreted closure or
 * a compiled procedure, through its raw entry, or a function marked
 * `SCHEME_PRIMITIVE` -- a primitive, a `define-class` class. A continuation is
 * excluded: its callable form is a JavaScript entry point that converts what it
 * is given.
 * @param {*} f - The value to check.
 * @returns {boolean}
 */
export function takesSchemeValues(f) {
    return typeof f === 'function' &&
        (f[SCHEME_CLOSURE] === true || f[SCHEME_PRIMITIVE] === true || f.$compiled === true);
}

/**
 * Calls a procedure for which `takesSchemeValues` holds, from a primitive that
 * holds Scheme values, binding `this`. Nothing is converted in either
 * direction.
 * @param {Function} proc - The procedure.
 * @param {*} thisArg - The value `this` is bound to.
 * @param {Array} args - The arguments, as Scheme values.
 * @returns {*} The procedure's result, as a Scheme value.
 */
export function callSchemeMethod(proc, thisArg, args) {
    const method = proc[SCHEME_RAW_METHOD_CALL];
    if (method !== undefined) {
        return method(thisArg, args);
    }
    // A compiled procedure, through its raw entry, may return a pending tail
    // call rather than a value. It may not move its frames to the heap stack
    // beneath this JavaScript.
    const flush = suspendFlush();
    try {
        return settleTailCalls((proc[SCHEME_RAW_CALL] ?? proc).apply(thisArg, args));
    } finally {
        restoreFlush(flush);
    }
}

/**
 * Error thrown to unwind the JavaScript stack when invoking a Continuation.
 * This allows jumping across JS boundaries (e.g. inside js-eval).
 */
export class ContinuationUnwind extends Error {
    /**
     * @param {Array} registers - The register state to restore.
     * @param {boolean} isReturn - True if this is a value return, false for tail call.
     */
    constructor(registers, isReturn = false) {
        super("Continuation Unwind");
        this.registers = registers;
        this.isReturn = isReturn;
    }
}

/**
 * Multiple Values wrapper.
 * Used by `values` and `call-with-values` to pass multiple return values.
 * 
 * In R7RS, (values 1 2 3) returns a "multiple values" object.
 * call-with-values unpacks it and applies to the consumer.
 */
export class Values {
    /**
     * @param {Array} values - The array of values being returned.
     */
    constructor(values) {
        this.values = values;
    }

    /**
     * Get the first value (used in single-value contexts and JS interop).
     * @returns {*} The first value, or undefined if empty.
     */
    first() {
        return this.values[0];
    }

    /**
     * Get all values as an array.
     * @returns {Array}
     */
    toArray() {
        return this.values;
    }

    toString() {
        return `#<values: ${this.values.length} values>`;
    }
}

/**
 * No values, as `(values)` returns them: a consumer of them is given no
 * arguments, and a REPL shows nothing for them, as for the unspecified value.
 * @type {Values}
 */
export const NO_VALUES = new Values(Object.freeze([]));
