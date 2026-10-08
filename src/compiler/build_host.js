/**
 * @fileoverview What the build steps and the compiler's harnesses, Scheme
 * programs run from the CLI, need of the host, as the library
 * `(scheme-js compiler build)`.
 *
 * The build compiles every shipped library, and the compiler's own, into the
 * prebuilt tables (`scripts/generate_compiled_libraries.scm`,
 * `scripts/generate_compiled_compiler.scm`): it loads each library from its
 * source in a library registry of its own, has the compiler generate code for
 * the procedures the library defines, installs that code before the libraries
 * that import it load, and writes the tables (`scripts/lib/table_writer.scm`).
 * The deciding and the writing are Scheme. What is here is what only the
 * host can do, each procedure one of two kinds:
 *
 *  - **Doors into the library system and the expander**, whose registries,
 *    loader and instance are held by JavaScript (`library_registry.js`,
 *    `library_loader.js`, `expand.js`): a registry of the program's own, with
 *    a resolver and a load hook that are Scheme procedures, loading a library
 *    in it, and expanding a form.
 *  - **Generating code**: installing generated code into a library as its
 *    table would be (`installPrebuilt`, which calls `new Function`), and the
 *    fingerprint the runtime checks a table's sources by, which there is one
 *    of, the runtime's.
 *
 * What the build reads of the evaluator's own objects -- a closure's
 * parameters, the span of source a datum came from -- it reads through
 * `(scheme-js interop)`.
 *
 * Registered by the CLI only (`repl.js`): no page builds anything.
 */

import { Cons, list, toArray } from '../core/interpreter/cons.js';
import { globalContext } from '../core/interpreter/context.js';
import { intern } from '../core/interpreter/symbol.js';
import { SCHEME_PRIMITIVE, SCHEME_RAW_CALL, callSchemeProcedure } from '../core/interpreter/values.js';
import { createInterpreter } from '../core/interpreter/index.js';
import { expandToCore, analyze } from '../core/interpreter/expand.js';
import { parse } from '../core/interpreter/reader.js';
import { assemble } from '../core/interpreter/assembler.js';
import { loadLibrarySync, programEnvironment } from '../core/interpreter/library_loader.js';
import {
  registerBuiltinLibrary, withPrivateLibraries, currentLibraryRegistry, callLibrarySystem
} from '../core/interpreter/library_registry.js';
import { stringValue } from '../core/primitives/string_class.js';
import { installPrebuilt, installLibraryTable, fingerprintSources, RUNTIME_INTERFACE } from './prebuilt.js';
import { registerCompilerHost } from './host.js';
import { AHEAD_PRIMITIVES } from './ahead.js';
import prebuiltLibraries from '../packaging/compiled_libraries.js';

/**
 * The library's name.
 * @type {string[]}
 */
export const BUILD_LIBRARY = ['scheme-js', 'compiler', 'build'];

/**
 * The interpreter each private registry's libraries are loaded on, innermost
 * last.
 * @type {Array<{interpreter: Object, env: Object}>}
 */
const loading = [];

/**
 * The procedures of `(scheme-js compiler build)`. Each takes and returns
 * Scheme values, as a primitive does: names are strings, lists are lists, and
 * "none" is `#f`.
 */
const buildProcedures = {
  /**
   * Runs a thunk with a library registry of its own, empty, current: what it
   * loads is found by nothing outside it (`withPrivateLibraries`).
   * @param {procedure} resolver - Called with a library's name, or a file's
   *   path, as a list of strings; answers the file's text.
   * @param {procedure|boolean} hook - Called with each library loaded, by
   *   name, and its environment; or #f.
   * @param {procedure} thunk - What to run.
   * @returns {*} What the thunk returns.
   */
  'with-private-libraries': (resolver, hook, thunk) => withPrivateLibraries({
    resolver: (parts) => stringValue(callSchemeProcedure(resolver, [list(...parts)])),
    hook: hook === false ? null : (name, env) => { callSchemeProcedure(hook, [list(...name), env]); }
  }, () => {
    loading.push(createInterpreter());
    try {
      return callSchemeProcedure(thunk, []);
    } finally {
      loading.pop();
    }
  }),

  /**
   * Registers `(scheme-js compiler host)` in the current registry, for loading
   * the compiler's library in it.
   */
  'register-compiler-host!': () => {
    registerCompilerHost(loading[loading.length - 1].env);
    return undefined;
  },

  /**
   * Loads a library by name in the current private registry, and what it
   * imports first, each top-level form of a library's body expanded and given,
   * with its core form, to a procedure before it runs.
   * @param {list} name - The library's name, a list of symbols.
   * @param {procedure} note - Called with each form and its core form.
   * @returns {list} The names it exports, as symbols.
   */
  'load-library': (name, note) => {
    const { interpreter, env } = loading[loading.length - 1];
    const noting = (form) => {
      const core = expandToCore(form);
      callSchemeProcedure(note, [form, core]);
      return assemble(core, analyze);
    };
    const exports = loadLibrarySync(toArray(name), noting, interpreter, env);
    return list(...[...exports.keys()].map(intern));
  },

  /**
   * The variables a library loaded in the current registry exports, by its
   * key: its exports that are not syntactic keywords. Read through the
   * library system's own procedures, whose records the registry and its
   * exports are; a program that imports the library system has a copy of its
   * own, whose record types are not those.
   * @param {string} key - The library's key, `scheme.base`.
   * @returns {list} Each `(name . value)`.
   */
  'library-variables': (key) => {
    const exports = callLibrarySystem('registered-exports', currentLibraryRegistry(), key);
    if (exports === false) throw new Error(`library-variables: no library ${stringValue(key)} is loaded`);
    return list(...toArray(exports).filter((entry) => callLibrarySystem('syntactic-keyword?', entry.cdr) === false));
  },

  /**
   * A program's forms, read as the CLI reads a program's file: with dot
   * notation on, unless the file turns it off, where a library's file has it
   * off (`%read-forms`).
   * @param {string} text - The program's text.
   * @param {string} filename - Its file, which its forms' spans name.
   * @returns {list} The forms.
   */
  'read-program': (text, filename) => list(...parse(stringValue(text), { filename: stringValue(filename) })),

  /**
   * A program's environment, made in the current private registry from the
   * import declarations it begins with, and each of its other forms expanded
   * there into its core form, as it would be to run, but not run: what a
   * build that compiles the program ahead of time compiles. A macro the
   * program defines is defined as its definition expands, for the forms
   * after it.
   * @param {list} forms - The program's forms, as read.
   * @returns {pair} `(env . core-forms)`.
   */
  'expand-program': (forms) => {
    const { interpreter, env } = loading[loading.length - 1];
    const program = programEnvironment(toArray(forms), analyze, interpreter, env);
    const scope = program.env.libraryScope;
    const cores = program.forms.map((form) => {
      if (scope === undefined) return expandToCore(form, program.env);
      globalContext.pushDefiningScope(scope);
      try {
        return expandToCore(form, program.env);
      } finally {
        globalContext.popDefiningScope();
      }
    });
    return new Cons(program.env, list(...cores));
  },

  /**
   * A form expanded into its core form by the system's expander, at the top
   * level of the process, as the evaluator expands one before running it:
   * the input the compiler's lowering takes, for a harness that lowers code
   * it has read.
   * @param {*} form - The form.
   * @returns {*} Its core form.
   */
  'expand': (form) => expandToCore(form),

  /**
   * Installs a shipped library's prebuilt table, as the build found it, into
   * the library as it loads, as a page does when it cannot restore one
   * (`installLibraryTable`).
   * @param {list} name - The library's name, as strings.
   * @param {Object} env - Its environment.
   * @param {procedure} sourceOf - Answers one of its files' text by name, or #f.
   * @returns {boolean} Whether its table was stale, and so nothing installed.
   */
  'install-table!': (name, env, sourceOf) => {
    const outcome = installLibraryTable(prebuiltLibraries, toArray(name).map(stringValue), env, (file) => {
      const text = callSchemeProcedure(sourceOf, [file]);
      return text === false ? undefined : stringValue(text);
    });
    return outcome !== null && outcome.stale;
  },

  /**
   * The names of the primitives a program compiled ahead of time runs with
   * (`AHEAD_PRIMITIVES` in ahead.js), which the build refuses a program for
   * reaching any other of.
   * @returns {list} The names, as symbols.
   */
  'ahead-primitive-names': () => list(...Object.keys(AHEAD_PRIMITIVES).map(intern)),

  /**
   * The fingerprint of the runtime's interface, which a table records as the
   * one its code was generated against (`RUNTIME_INTERFACE`).
   * @returns {string}
   */
  'runtime-interface': () => RUNTIME_INTERFACE,

  /**
   * The fingerprint of a library's sources, as the runtime checks a table's
   * (`fingerprintSources`).
   * @param {list} texts - The text of each of its files, in its table's order.
   * @returns {string}
   */
  'fingerprint-sources': (texts) => fingerprintSources(toArray(texts).map(stringValue)),

  /**
   * Installs code generated for a library's procedures into it, as its table
   * will be installed at run time, so that a library loaded after it calls it
   * compiled, as it will in the bundle.
   * @param {Object} env - The library's environment.
   * @param {string} fingerprint - Its sources' fingerprint.
   * @param {list} files - Its files' names.
   * @param {list} entries - Each `(name params rest constants source)`.
   * @returns {unspecified}
   */
  'install-generated!': (env, fingerprint, files, entries) => {
    const procedures = {};
    for (const entry of toArray(entries)) {
      const [name, params, rest, constants, source] = toArray(entry);
      procedures[stringValue(name)] = {
        params: toArray(params).map(stringValue),
        rest: rest === false ? null : stringValue(rest),
        constants: toArray(constants),
        make: new Function('R', 'E', 'K', stringValue(source))
      };
    }
    const print = stringValue(fingerprint);
    installPrebuilt(env, { fingerprint: print, files: toArray(files).map(stringValue), procedures }, print);
    return undefined;
  }
};

for (const fn of Object.values(buildProcedures)) {
  fn[SCHEME_PRIMITIVE] = true;
  fn[SCHEME_RAW_CALL] = fn;
}

/**
 * Registers `(scheme-js compiler build)` in the current library registry.
 * @param {Object} env - The global environment of the CLI's interpreter.
 */
export function registerBuildHost(env) {
  registerBuiltinLibrary(BUILD_LIBRARY, buildProcedures, env);
}
