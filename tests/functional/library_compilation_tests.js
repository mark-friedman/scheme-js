/**
 * @fileoverview Compiling a procedure must reach the libraries that export it.
 *
 * Importing copies values: a library's export map, and the environment of every
 * library that imported it, hold the procedure object that was bound when the
 * library loaded. The standard library is loaded interpreted and compiled
 * afterwards, by replacing each global binding in place. Nothing updated the
 * copies, so a library loaded after start-up received the *interpreted* `map`,
 * `equal?` and the rest -- paying the interpreter boundary on its hottest
 * calls, and holding procedures that were not `eq?` to the ones user code saw.
 *
 * These tests pin that both install paths carry the replacement through to the
 * library registry.
 */

import { assert } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { Environment } from '../../src/core/interpreter/environment.js';
import {
  registerLibrary, getLibraryExports, getFileResolver, setFileResolver,
  setLibraryLoadHook
} from '../../src/core/interpreter/library_registry.js';
import { loadLibrarySync } from '../../src/core/interpreter/library_loader.js';
import { compileEnvironment } from '../../src/compiler/index.js';
import { interpretedLibrary, installStandardLibrary } from '../harness/standard_library.js';

/**
 * Evaluates source in an interpreter's environment.
 * @param {Object} interpreter - The interpreter.
 * @param {Object} env - The environment to evaluate in.
 * @param {string} source - Scheme source.
 * @returns {void}
 */
function evaluate(interpreter, env, source) {
  for (const form of parse(source)) {
    interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
  }
}

/**
 * Registers a library that exports, and has imported, one global's value.
 * @param {string} key - The registry key.
 * @param {string} name - The global to copy.
 * @param {Object} env - The environment holding it.
 * @returns {Environment} The library's own environment.
 */
function libraryCopying(key, name, env) {
  const libraryEnv = new Environment(null);
  libraryEnv.define(name, env.lookup(name));
  registerLibrary(key, new Map([[name, env.lookup(name)]]), libraryEnv);
  return libraryEnv;
}

/**
 * Runs the library-compilation tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runLibraryCompilationTests(logger) {
  logger.title('Compiled procedures reach the libraries that export them');

  {
    const { interpreter, env } = createInterpreter();
    evaluate(interpreter, env, '(define (area w h) (* w h))');
    const libraryEnv = libraryCopying('test.compile-environment', 'area', env);

    compileEnvironment(env);
    const compiled = env.lookup('area');
    assert(logger, 'setup: compileEnvironment compiled the global', compiled.$compiled, true);
    assert(logger, 'compileEnvironment updates the export map',
      getLibraryExports('test.compile-environment').get('area') === compiled, true);
    assert(logger, "compileEnvironment updates an importing library's binding",
      libraryEnv.lookup('area') === compiled, true);
  }

  {
    const { env } = interpretedLibrary();
    const libraryEnv = libraryCopying('test.install-prebuilt', 'map', env);

    installStandardLibrary(env);
    const compiled = env.lookup('map');
    assert(logger, 'setup: installPrebuilt installed map', compiled.$compiled, true);
    assert(logger, 'installPrebuilt updates the export map',
      getLibraryExports('test.install-prebuilt').get('map') === compiled, true);
    assert(logger, "installPrebuilt updates an importing library's binding",
      libraryEnv.lookup('map') === compiled, true);
  }

  // Only the replaced object is substituted. A library that bound the same name
  // to something else keeps it.
  {
    const { interpreter, env } = createInterpreter();
    evaluate(interpreter, env, '(define (area w h) (* w h))');
    const own = (w, h) => w + h;
    registerLibrary('test.unrelated', new Map([['area', own]]), new Environment(null));

    compileEnvironment(env);
    assert(logger, 'a different value under the same name is left alone',
      getLibraryExports('test.unrelated').get('area') === own, true);
  }

  // --- Libraries loaded after start-up ---------------------------------------
  //
  // Compiling the environment once at start-up cannot reach a library loaded
  // later, so a loader hook runs on each library read from a file. Whoever
  // owns the process decides what the hook does; the bundle compiles the
  // libraries it ships.
  const sources = {
    'test.hooked': `(define-library (test hooked) (export double)
                      (begin (define (double x) (* 2 x))))`,
    'test.inline': `(define-library (test inline) (export triple)
                      (begin (define (triple x) (* 3 x))))`,
    'test.compiled-on-load': `(define-library (test compiled-on-load) (export quadruple)
                      (begin (define (quadruple x) (* 4 x))))`
  };
  const savedResolver = getFileResolver();
  setFileResolver((name) => sources[name.join('.')]);
  try {
    const { interpreter, env } = createInterpreter();
    const calls = [];
    setLibraryLoadHook((name, libraryEnv) => calls.push({ name: name.join('.'), libraryEnv }));

    loadLibrarySync(['test', 'hooked'], analyze, interpreter, env);
    assert(logger, 'the hook runs when a library is loaded from its file',
      calls.map((c) => c.name).join(','), 'test.hooked');
    assert(logger, "the hook receives the library's own environment",
      typeof (calls[0] && calls[0].libraryEnv.bindings.get('double')), 'function');

    loadLibrarySync(['test', 'hooked'], analyze, interpreter, env);
    assert(logger, 'the hook does not run again for a cached library', calls.length, 1);

    // A library written inline is the program's own code, not one loaded by
    // name, so the hook leaves it alone.
    evaluate(interpreter, env, sources['test.inline']);
    assert(logger, 'the hook does not run for an inline define-library', calls.length, 1);

    setLibraryLoadHook((name, libraryEnv) => compileEnvironment(libraryEnv));
    const exports = loadLibrarySync(['test', 'compiled-on-load'], analyze, interpreter, env);
    assert(logger, 'a hook that compiles the library leaves its exports compiled',
      exports.get('quadruple').$compiled, true);
  } finally {
    setLibraryLoadHook(null);
    setFileResolver(savedResolver);
  }
}
