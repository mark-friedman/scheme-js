/**
 * @fileoverview The standard library, interpreted at top level, for tests.
 *
 * Tests of compiling and of installing prebuilt code need *interpreted*
 * closures to work on. Loading `(scheme base)` does not reliably give them: the
 * library registry is shared by the whole process, so by the time a test runs
 * the library is usually already loaded, and already compiled. Evaluating the
 * standard library's files straight into a fresh environment does, every time.
 *
 * The files are the ones `(scheme core)`, `(scheme control)` and
 * `(scheme case-lambda)` include, read from their `.sld` files so that the list
 * cannot drift from what the libraries are made of.
 */

import { createInterpreter } from '../../src/core/interpreter/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { parseDefineLibrary } from '../../src/core/interpreter/library_loader.js';
import { installPrebuilt, fingerprintSources } from '../../src/compiler/prebuilt.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';
import LIBRARIES from '../../src/packaging/compiled_libraries.js';

/**
 * The libraries the standard library's procedures are defined in, in the
 * order their files load.
 * @type {string[]}
 */
const STANDARD_LIBRARIES = ['core', 'control', 'case-lambda'];

/**
 * The files the standard library is made of, in load order.
 * @type {string[]}
 */
export const STANDARD_LIBRARY_FILES = STANDARD_LIBRARIES.flatMap(
  (name) => parseDefineLibrary(parse(BUNDLED_SOURCES[`${name}.sld`])[0]).includes);

/**
 * The prebuilt table holding the standard library's procedures. `(scheme core)`
 * defines all of them; the other two libraries define only syntax.
 * @type {{fingerprint: string, files: string[], procedures: Object}}
 */
export const STANDARD_LIBRARY_TABLE = LIBRARIES['scheme.core'];

/**
 * The fingerprint of the sources the standard library's table covers, as they
 * are in this tree.
 * @returns {string} The fingerprint.
 */
export function standardLibraryFingerprint() {
  return fingerprintSources(STANDARD_LIBRARY_TABLE.files.map((file) => BUNDLED_SOURCES[file]));
}

/**
 * Bootstraps a fresh interpreter with the standard library interpreted at top
 * level.
 * @param {Object} [options] - Options.
 * @param {boolean} [options.filenames=false] - Record each file's name in the
 *   source spans of what it defines, as loading the library does.
 * @returns {{interpreter: Object, env: Object}} The interpreter and environment.
 */
export function interpretedLibrary({ filenames = false } = {}) {
  const { interpreter, env } = createInterpreter();
  for (const file of STANDARD_LIBRARY_FILES) {
    const forms = parse(BUNDLED_SOURCES[file], filenames ? { filename: file } : undefined);
    for (const form of forms) {
      interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
    }
  }
  return { interpreter, env };
}

/**
 * Installs the standard library's prebuilt table into an environment holding
 * it interpreted.
 * @param {Object} env - An environment from `interpretedLibrary`.
 * @returns {Object} What `installPrebuilt` did.
 */
export function installStandardLibrary(env) {
  return installPrebuilt(env, STANDARD_LIBRARY_TABLE, standardLibraryFingerprint());
}
