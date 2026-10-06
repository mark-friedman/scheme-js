/**
 * Tests of how the harnesses that run code as a page does load libraries
 * (`tests/harness/page_libraries.js`): a shipped library restored from its
 * prebuilt table rather than read from its source. A harness that read the
 * source instead measured, and tested, what no page does (R124), so each way
 * a library can load is checked: restored, read from its source because its
 * table is stale, and read from its source because it does not ship.
 */

import { pageLibraries } from '../harness/page_libraries.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { loadLibrarySync } from '../../src/core/interpreter/library_loader.js';
import { withPrivateLibraries } from '../../src/core/interpreter/library_registry.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';
import { assert } from '../harness/helpers.js';

/**
 * A shipped library's file, or one it includes, from the bundle.
 * @param {Array} name - The library's name, or the file's path.
 * @returns {string|undefined}
 */
function bundled(name) {
  const last = String(name[name.length - 1]);
  return BUNDLED_SOURCES[`${last}.sld`] ?? BUNDLED_SOURCES[last];
}

/**
 * Whether a library ships with the bundle.
 * @param {Array} name - Its name.
 * @returns {boolean}
 */
const ships = (name) => BUNDLED_SOURCES[`${String(name[name.length - 1])}.sld`] !== undefined;

/**
 * Loads libraries, each by name, in a registry of their own, as `run_tier.js`
 * does, and gives back what the page libraries noted.
 * @param {Object} libraries - From `pageLibraries`.
 * @param {Array<Array<string>>} names - The libraries to load.
 * @returns {{exports: Array<Map>, restored: Set<string>, fromSource: Set<string>}}
 */
function load(libraries, names) {
  const exports = withPrivateLibraries(
    { resolver: libraries.resolve, hook: libraries.hook, restorer: libraries.restorer },
    () => {
      const { interpreter, env } = createInterpreter();
      return names.map((name) => loadLibrarySync(name, analyze, interpreter, env));
    });
  return { exports, restored: libraries.restored, fromSource: libraries.fromSource };
}

/**
 * Runs the tests of loading libraries as a page does.
 * @param {Object} logger - Test logger.
 */
export function runPageLibrariesTests(logger) {
  logger.title('Running tests of loading libraries as a page does...');

  // Shipped, with tables built from their files as they are: restored, none
  // read, `(scheme core)` with them, which `(scheme base)` imports
  {
    const { exports, restored, fromSource } = load(
      pageLibraries({ resolve: bundled, isShipped: ships }), [['scheme', 'base'], ['scheme', 'write']]);
    assert(logger, 'shipped libraries are restored from their tables',
      ['scheme.base', 'scheme.write', 'scheme.core'].every((key) => restored.has(key)), true);
    assert(logger, 'and none is read from its source', [...fromSource], []);
    assert(logger, 'and what they export is there', typeof exports[1].get('write'), 'function');
  }

  // A shipped library whose file is not the one its table was built from, as
  // after an edit not rebuilt: read from its source, and named so
  {
    const edited = (name) => (String(name[name.length - 1]) === 'char'
      ? `${bundled(name)}\n;; edited\n` : bundled(name));
    const { exports, fromSource } = load(
      pageLibraries({ resolve: edited, isShipped: ships }), [['scheme', 'char']]);
    assert(logger, 'a shipped library whose table is stale is read from its source, and named',
      [...fromSource], ['scheme.char']);
    assert(logger, 'and loads all the same', typeof exports[0].get('char-upcase'), 'function');
  }

  // A library that does not ship: read from its source, as every page reads one
  {
    const own = (name) => (String(name[0]) === 'own'
      ? '(define-library (own lib) (import (scheme base)) (export twice) (begin (define (twice x) (* 2 x))))'
      : bundled(name));
    const { exports, restored, fromSource } = load(
      pageLibraries({ resolve: own, isShipped: ships }), [['own', 'lib']]);
    assert(logger, 'a library that does not ship is neither restored nor counted as read in error',
      [restored.has('own.lib'), fromSource.has('own.lib')], [false, false]);
    assert(logger, 'and loads', typeof exports[0].get('twice'), 'function');
  }
}
