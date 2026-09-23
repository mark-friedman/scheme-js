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
  registerLibrary, getLibraryExports
} from '../../src/core/interpreter/library_registry.js';
import { compileEnvironment } from '../../src/compiler/index.js';
import { installPrebuilt, fingerprintSources } from '../../src/compiler/prebuilt.js';
import PREBUILT, { LIBRARY_FILES } from '../../src/packaging/compiled_stdlib.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';

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
    const { interpreter, env } = createInterpreter();
    for (const file of LIBRARY_FILES) {
      evaluate(interpreter, env, BUNDLED_SOURCES[file]);
    }
    const libraryEnv = libraryCopying('test.install-prebuilt', 'map', env);

    installPrebuilt(env, PREBUILT,
      fingerprintSources(LIBRARY_FILES.map((file) => BUNDLED_SOURCES[file])));
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
}
