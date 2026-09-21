/**
 * @fileoverview Installing standard-library procedures compiled at build time.
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
 * The first is a fingerprint of the library sources, recorded when the code was
 * generated and recomputed here. If a `.scm` file changed without the build
 * being re-run, nothing is installed at all.
 *
 * The second is per procedure, and is deliberately about *arity* rather than
 * about names. It is tempting to compare the analyzer's renamed parameter
 * names, and that turns out to be both useless and harmful. Useless because
 * generated code names locals only inside itself -- its sole external
 * references are `globalAccessor(E, "name")`, `currentBinding` and `E.set`, and
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
    env.define(name, entry.make(R, closure.env, entry.constants));
    installed.push(name);
  }

  return { installed, skipped, stale: false };
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
