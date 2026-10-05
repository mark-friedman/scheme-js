/**
 * @fileoverview The JavaScript the compiler's Scheme calls, as the library
 * `(scheme-js compiler host)`.
 *
 * The compiler's driver and the tier's policies are Scheme (`driver.scm`,
 * `safety.scm`, `tier.scm`); what is here is what Scheme cannot do itself.
 * Each procedure is one of four kinds:
 *
 *  - **Generating code**: `new Function`, which only JavaScript has, and the
 *    probe for whether a Content-Security-Policy forbids it.
 *  - **Reading the interpreter's own structures**: an analyzed form as the
 *    tagged lists the compiler reads (`marshal.js`), the lambda behind an
 *    interpreted closure, an environment's bindings, whether a name is still
 *    bound to its primitive. These are JavaScript objects whose shape the
 *    interpreter owns, and the library registry that keeps what each library
 *    imported.
 *  - **Running the program's code**, in the program's interpreter.
 *  - **Weak tables**, which JavaScript has as `WeakMap` and R7RS has not: the
 *    tier remembers each closure waiting to be compiled without keeping alive
 *    one the program has dropped.
 *
 * Every procedure takes and returns Scheme values, as a primitive does: names
 * are strings, lists are lists, and "none" is `#f`.
 */

import { LambdaNode, LiteralNode, TailAppNode } from '../core/interpreter/ast_nodes.js';
import { Cons } from '../core/interpreter/cons.js';
import { SCHEME_PRIMITIVE, SCHEME_RAW_CALL, runCompiled } from '../core/interpreter/values.js';
import { globalContext } from '../core/interpreter/context.js';
import {
  registerBuiltinLibrary, recordCompiledOver, switchBackToClosure
} from '../core/interpreter/library_registry.js';
import { astToScheme, toArray } from './marshal.js';
import * as R from './runtime.js';
import { sourceText } from '../core/interpreter/source_texts.js';

/**
 * The library's name.
 * @type {string[]}
 */
export const HOST_LIBRARY = ['scheme-js', 'compiler', 'host'];

/**
 * A Scheme string as the JavaScript string it holds.
 * @param {*} s - A Scheme string.
 * @returns {string}
 */
const text = (s) => String(s);

/**
 * A list of pairs as a `Map`.
 * @param {*} alist - A list of pairs.
 * @returns {Map<*, *>}
 */
const alistToMap = (alist) => new Map(toArray(alist).map((pair) => [pair.car, pair.cdr]));

/**
 * Calls a compiled thunk from the interpreter, and returns its value.
 *
 * From the interpreter rather than directly, so that it runs as compiled code
 * the interpreter called: a continuation captured in it, or frames moved to the
 * heap when it recurses deeply, are finished where they should be.
 *
 * @param {Object} interpreter - The interpreter.
 * @param {Object} env - The environment.
 * @param {Function} thunk - The compiled thunk.
 * @returns {*} Its value.
 */
export function runCompiledThunk(interpreter, env, thunk) {
  return R.settle(interpreter.run(new TailAppNode(new LiteralNode(thunk), []), env, [], undefined,
    { jsAutoConvert: 'raw' }));
}

/**
 * The procedures of `(scheme-js compiler host)`, by their Scheme names.
 * @type {Object<string, Function>}
 */
const hostProcedures = {
  // -- Generating code -------------------------------------------------------

  'code-generation-allowed?': () => {
    try {
      new Function('return 1');
      return true;
    } catch (e) {
      return false;
    }
  },

  // Turns generated source into a procedure: the body of a function of the
  // runtime `R`, the environment `E` and the constant pool `K`, recorded with
  // the source span it came from. `new Function` rather than `eval`, so the
  // generated code gets a scope of its own and cannot see the compiler's
  // bindings. Returns the procedure, or a string saying why it could not be
  // made.
  //
  // The code runs in strict mode, as the same code does in the prebuilt
  // tables, which are modules. A procedure's fast form tests how many
  // arguments it was given with `arguments.length`, and in sloppy code
  // `arguments` is an object aliased to the parameters: dearer to make, and
  // frames larger than the stack room each procedure reserves, so deep
  // compiled recursion ran out of JavaScript stack before its frames moved to
  // the heap.
  'instantiate': (source, env, constants, span) => {
    try {
      const procedure = new Function('R', 'E', 'K', `'use strict';\n${text(source)}`)(R, env, toArray(constants));
      return R.recordSource(procedure, span === false ? null : span);
    } catch (e) {
      return `code generation failed: ${e.message}`;
    }
  },

  // -- Reading the interpreter's structures ------------------------------------

  'ast->scheme': (node) => astToScheme(node),

  // An analyzed form's source span, or #f.
  'ast-span': (node) => node.source ?? false,

  // A definition's span: its value's, or else its own; or #f.
  'definition-span': (node) => (node.valueExpr ?? node.value)?.source ?? node.source ?? false,

  'interpreted-closure?': (value) => typeof value === 'function' && value.body !== undefined
    && value.compiled === undefined,

  // The lambda an interpreted closure was made from, as the compiler reads it.
  // A closure keeps its parameters, body and environment, so a procedure that
  // was loaded and interpreted can be compiled afterwards from them.
  'closure-lambda': (closure, name) => astToScheme(new LambdaNode(
    closure.params, closure.body, closure.restParam, text(name),
    closure.originalParams, closure.originalRestParam)),

  'closure-body': (closure) => astToScheme(closure.body),

  'closure-environment': (closure) => closure.env,

  'closure-span': (closure) => closure.source ?? false,

  // The bindings an environment holds itself, as a list of (name . value).
  'environment-bindings': (env) => {
    let out = null;
    const entries = [...env.bindings];
    for (let i = entries.length - 1; i >= 0; i--) {
      out = new Cons(new Cons(entries[i][0], entries[i][1]), out);
    }
    return out;
  },

  // The environment an environment is inside, or #f.
  'environment-parent': (env) => env.parent ?? false,

  // The name of the library an environment belongs to, as the vector of
  // strings the library registry keys it by, or #f for one that is not a
  // library's.
  'environment-library': (env) => env.libraryName ?? false,

  // Whether an environment holds what was imported into it and nothing else:
  // a library's, a program's that began with import declarations, or one
  // `environment` made.
  'environment-strict?': (env) => env.strict === true,

  // The text code read under a name was read from, where nothing could fetch
  // it by the name -- a page's inline script -- or #f.
  'source-text': (name) => sourceText(text(name)) ?? false,

  // The value a name has where an environment finds it, or #f if it is unbound.
  'environment-value': (env, name) => {
    const holder = env.findEnv(text(name));
    return holder === null ? false : holder.bindings.get(text(name));
  },

  'environment-define!': (env, name, value) => { env.define(text(name), value); },

  // Whether a global is bound, in an environment, to the primitive its name
  // had when the system started: the condition for expanding it inline.
  'bound-to-primitive?': (env, name) => {
    const cell = R.primitiveCell(text(name));
    return cell.primitive !== null && R.currentBinding(env, text(name)) === cell.primitive;
  },

  // Whether any library is being loaded. A definition made then is registered
  // with the scopes that library's macros resolve through. A program that
  // began with import declarations runs under a scope of its own too, which
  // is not a library's.
  'library-loading?': () => globalContext.definingScopes.some(
    (scope) => globalContext.lookupLibraryEnv(scope)?.libraryName !== undefined),

  // Whether an interpreter is running a program under a debugger.
  'debugging?': (interpreter) => interpreter.debugging === true,

  // How many more calls a closure waits before the interpreter tells the tier
  // it is due; 0 for none. Kept on the closure, which the interpreter counts
  // down as it applies it.
  'wait-calls!': (closure, calls) => { closure.tierCountdown = Number(calls); },

  // -- The library registry -----------------------------------------------------

  // Makes a closure run as the compiled procedure made of it, staying the
  // object every holder of it has (`runCompiled` in values.js).
  'run-compiled!': (closure, procedure) => { runCompiled(closure, procedure); },

  // Each (closure . procedure) made to run compiled in an environment,
  // recorded so a debugger can run the closures as themselves, and so one can
  // be switched back for good.
  'record-compiled-over!': (compiled, env) => { recordCompiledOver(alistToMap(compiled), env); },

  // Runs a closure as itself again for good, found by its compiled
  // procedure's resumable form. Whether there was one to switch.
  'switch-back-to-closure!': (twin) => switchBackToClosure(twin),

  // -- Running the program's code --------------------------------------------

  // Runs an analyzed form in an interpreter, with Scheme values.
  'run-form': (interpreter, node, env) => interpreter.run(node, env, [], undefined, { jsAutoConvert: 'raw' }),

  'run-thunk': (interpreter, env, thunk) => runCompiledThunk(interpreter, env, thunk),

  // -- Weak tables -------------------------------------------------------------

  'make-weak-table': () => new WeakMap(),
  'weak-table-ref': (table, key) => (table.has(key) ? table.get(key) : false),
  'weak-table-set!': (table, key, value) => { table.set(key, value); },
  'weak-table-delete!': (table, key) => { table.delete(key); }
};

for (const fn of Object.values(hostProcedures)) {
  fn[SCHEME_PRIMITIVE] = true;
  fn[SCHEME_RAW_CALL] = fn;
}

/**
 * Registers `(scheme-js compiler host)` in the current library registry, so the
 * compiler's library can import it. Called wherever that library is loaded.
 * @param {Object} env - The global environment of the interpreter loading it.
 */
export function registerCompilerHost(env) {
  registerBuiltinLibrary(HOST_LIBRARY, hostProcedures, env);
}
