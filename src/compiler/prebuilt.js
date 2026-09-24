/**
 * @fileoverview Installing library procedures compiled at build time.
 *
 * Every library the bundle ships has a table of its procedures compiled at
 * build time, and so does the compiler, which is a library too. A table is
 * installed into its library's own environment as the library loads -- see
 * `installLibraryTable` -- so a library imported long after start-up arrives
 * compiled without the compiler being present at all. That is what lets a
 * page that never compiles its own code leave the compiler out of the bundle.
 *
 * ## Why build time rather than bootstrap
 *
 * Compiling the library when the system starts costs about 20 ms of a 71 ms
 * bootstrap, and it needs `new Function`, which a strict
 * Content-Security-Policy forbids. Doing it at build time removes both: the
 * generated JavaScript is ordinary module text that the loader evaluates, so
 * nothing is generated at run time and a page with a strict policy gets the
 * compiled library rather than an interpreted one.
 *
 * It also decouples deployment from compile *speed*, which matters for a
 * longer-term reason: a compiler written in Scheme would be slower than the
 * JavaScript one, and if nothing compiles at startup that stops being a
 * deployment concern and becomes a slower build.
 *
 * ## Staleness
 *
 * Prebuilt code that no longer matches the library it was generated from would
 * be the worst kind of wrong: a procedure quietly doing what an older version
 * of its source said. Two checks prevent it, and both fail towards leaving the
 * procedure alone.
 *
 * Before either, a table must have been generated against this runtime's
 * interface -- the names generated code reaches through `R`. The sources can
 * be unchanged while the runtime is not: code generated before a runtime
 * function was renamed calls the old name, and fails when installed or, worse,
 * when first called.
 *
 * The first is a fingerprint of the library's sources -- its `.sld` and every
 * file it includes -- recorded when the code was generated and recomputed
 * here. If one of them changed without the build being re-run, nothing from
 * that library is installed at all.
 *
 * The second is per procedure, and is deliberately about *arity* rather than
 * about names. It is tempting to compare the analyzer's renamed parameter
 * names, and that turns out to be both useless and harmful. Useless because
 * generated code names locals only inside itself -- its sole external
 * references are `globalCell(E, "name")`, `primitiveCell("name")` and `E.set`, and
 * every one of those uses the name as written in the source, never a renamed
 * one. Harmful because renaming comes from a counter that advances as the
 * analyzer works, so a program that bootstraps a second interpreter gets
 * different names for identical source and would silently lose every prebuilt
 * procedure. Arity is renaming-independent and still catches a changed
 * signature.
 *
 * ## What is not prebuilt
 *
 * The library's *source* still loads and is still interpreted first, because
 * that is what creates the macros the analyzer needs and the closures this
 * replaces. Only the compilation step is moved. Skipping the load as well would
 * mean separating each file's macro definitions from its procedure definitions,
 * which is a larger change than this one.
 */

import * as R from './runtime.js';
import { substituteLibraryValues, libraryNameToKey } from '../core/interpreter/library_registry.js';

/**
 * Hashes the library sources into a short fingerprint.
 *
 * FNV-1a, written out here rather than taken from a hashing library because
 * both sides of the check need the identical function and one of them runs in a
 * browser. It is not a security hash; it is guarding against a stale build, and
 * for that a collision is no worse than the check not existing.
 *
 * @param {Array<string>} sources - The library sources, in a fixed order.
 * @returns {string} A hexadecimal fingerprint.
 */
export function fingerprintSources(sources) {
  let hash = 0x811c9dc5;
  for (const source of sources) {
    for (let i = 0; i < source.length; i++) {
      hash ^= source.charCodeAt(i);
      // The FNV prime, as shifts, because `Math.imul` with 16777619 overflows
      // into a double and loses the low bits this relies on.
      hash = Math.imul(hash, 0x01000193) >>> 0;
    }
    // Folded in so that moving text between two files changes the result.
    hash ^= 0x5f5e100;
    hash = Math.imul(hash, 0x01000193) >>> 0;
  }
  return hash.toString(16).padStart(8, '0');
}

/**
 * A fingerprint of the runtime's interface: the names generated code can reach
 * through `R`. It changes when one is added, renamed or removed, which is when
 * code generated against the old set stops being safe to install. A change to
 * what an existing function does, under the same name, is not caught.
 * @type {string}
 */
export const RUNTIME_INTERFACE = fingerprintSources([Object.keys(R).sort().join(' ')]);

/**
 * Installs prebuilt procedures into an environment, replacing the interpreted
 * ones.
 *
 * @param {Object} env - The environment holding the interpreted library.
 * @param {Object} table - A generated table: `{fingerprint, files, procedures}`.
 * @param {string} fingerprint - The fingerprint of the sources actually loaded.
 * @returns {{installed: Array<string>, skipped: Array<{name: string, reason: string}>,
 *   stale: boolean}} What was installed, and what was left interpreted.
 */
export function installPrebuilt(env, table, fingerprint) {
  if (table === undefined || table.fingerprint !== fingerprint) {
    return {
      installed: [],
      skipped: [],
      stale: true
    };
  }

  const installed = [];
  const skipped = [];
  const replaced = new Map();

  for (const [name, entry] of Object.entries(table.procedures)) {
    const closure = env.bindings.get(name);
    if (typeof closure !== 'function' || closure.body === undefined) {
      // Not an interpreted closure any more -- already compiled, redefined, or
      // never loaded. Whatever is there now is what the program asked for.
      skipped.push({ name, reason: 'not an interpreted closure' });
      continue;
    }
    if (!sameArity(closure, entry)) {
      skipped.push({ name, reason: 'arity differs from the generated code' });
      continue;
    }
    // Built against the closure's own environment, so its free variables
    // resolve where they did when it was interpreted.
    const procedure = R.recordSource(entry.make(R, closure.env, entry.constants), closure.source);
    env.define(name, procedure);
    replaced.set(closure, procedure);
    installed.push(name);
  }

  // Libraries imported the interpreted closures by value; see
  // `substituteLibraryValues`.
  substituteLibraryValues(replaced);
  return { installed, skipped, stale: false };
}

/**
 * Installs a library's prebuilt table into the library's own environment, if
 * there is one and it was generated from the sources being loaded.
 *
 * Meant to run from the library loader's hook, once the library's body has
 * been evaluated: the closures it replaces must exist, and nothing has yet
 * imported them except through the export map, which `installPrebuilt` keeps
 * in step.
 *
 * @param {Object<string, Object>} tables - Generated tables, keyed by library
 *   name as `libraryNameToKey` writes it.
 * @param {string[]} libraryName - The library just loaded.
 * @param {Object} env - Its own environment.
 * @param {(file: string) => (string|undefined)} sourceOf - The source of one
 *   of the library's files, by the name its table lists it under.
 * @returns {{installed: Array<string>, skipped: Array<{name: string, reason: string}>,
 *   stale: boolean}|null} What `installPrebuilt` did, or null if the library has
 *   no table.
 */
export function installLibraryTable(tables, libraryName, env, sourceOf) {
  const table = tables[libraryNameToKey(libraryName)];
  if (table === undefined) return null;
  if (table.runtime !== RUNTIME_INTERFACE) return { installed: [], skipped: [], stale: true };
  const sources = table.files.map(sourceOf);
  // A file the table was built from and the loader cannot find now means the
  // library has changed shape since the build, which is staleness too.
  if (sources.some((source) => typeof source !== 'string')) {
    return { installed: [], skipped: [], stale: true };
  }
  return installPrebuilt(env, table, fingerprintSources(sources));
}

/**
 * Whether a live closure still takes the arguments the generated code expects.
 *
 * Names are deliberately not compared; see the note above on why that would be
 * both useless and harmful.
 *
 * @param {Function} closure - The interpreted closure.
 * @param {Object} entry - Its generated-table entry.
 * @returns {boolean} True if the arities agree.
 */
function sameArity(closure, entry) {
  const hasRest = (closure.restParam ?? null) !== null;
  if (hasRest !== ((entry.rest ?? null) !== null)) return false;
  return closure.params.length === entry.params.length;
}
