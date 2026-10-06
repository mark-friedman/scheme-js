/**
 * @fileoverview Libraries loaded as a page loads them, for the harnesses that
 * run code as a page runs it: `benchmarks/run_tier.js`, the tiered Scheme
 * tests (`tests/run_tiered_scheme_tests_lib.js`), and the conformance suites
 * with the standard library compiled (`compliance/compliance_suite.js`).
 *
 * A page (`src/packaging/scheme_entry.js`), the CLI (`repl.js`) and the
 * development page (`web/main.js`) give the library system two things: a
 * restorer, which binds a shipped library's procedures from its prebuilt
 * table -- the library's files fetched, to be fingerprinted, but never read --
 * and a load hook, which installs the table's procedures into a library as it
 * loads. A harness with the hook alone reads, expands and runs every shipped
 * library's source before installing the table over it, which took a fresh
 * interpreter 22.5 ms to import `(scheme base)` and `(scheme write)` where a
 * page takes 1.8 (R124 in `docs/compiler_findings.md`).
 */

import { installLibraryTable, libraryRestorer } from '../../src/compiler/prebuilt.js';
import { libraryNameToKey } from '../../src/core/interpreter/library_registry.js';
import prebuiltLibraries from '../../src/packaging/compiled_libraries.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';

/**
 * How to load libraries as a page does, to give `withPrivateLibraries`.
 *
 * A shipped library is restored from its table; one whose table was not built
 * from its files as they are -- a source edited and not rebuilt -- is read and
 * run from its source, as a page would too, and is named in `fromSource`, so
 * that a harness can refuse to report a run no page would make. Any other
 * library is read and run from its source.
 *
 * @param {Object} options - What the libraries are.
 * @param {function(Array): string} options.resolve - The text of a library's
 *   file, or of a file one includes, by name.
 * @param {function(Array): boolean} options.isShipped - Whether a library is
 *   one the bundle ships, with a prebuilt table.
 * @returns {{resolve: Function, hook: Function, restorer: Function,
 *   isPrebuilt: Function, restored: Set<string>, fromSource: Set<string>,
 *   installation: Map<string, Object>}} The resolver, hook and restorer,
 *   `isShipped` again as the tier's `isPrebuilt`; the shipped libraries
 *   restored and those read from their source; and what installing each
 *   shipped library's table did (`installLibraryTable`): each by key,
 *   `scheme.base`.
 */
export function pageLibraries({ resolve, isShipped }) {
  const restore = libraryRestorer(prebuiltLibraries);
  const restored = new Set();
  const fromSource = new Set();
  const installation = new Map();
  const restorer = (name, texts) => {
    if (!isShipped(name)) return null;
    const restoring = restore(name, texts);
    if (texts !== null && restoring !== null) restored.add(libraryNameToKey(name));
    return restoring;
  };
  // Called for every library loaded, restored or not, as the CLI's is: the
  // table's installing records the library's procedures as compiled, which
  // the tier asks.
  const hook = (name, env) => {
    if (!isShipped(name) || !env) return;
    const key = libraryNameToKey(name);
    if (!restored.has(key)) fromSource.add(key);
    const outcome = installLibraryTable(prebuiltLibraries, name, env, (file) => BUNDLED_SOURCES[file]);
    if (outcome !== null) installation.set(key, outcome);
  };
  return { resolve, hook, restorer, isPrebuilt: isShipped, restored, fromSource, installation };
}
