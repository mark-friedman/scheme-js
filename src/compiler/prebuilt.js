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
 *
 * So whatever the source made as it loaded was made with the closures: SRFI
 * 128's default comparators are records holding `default-hash`, interpreted.
 * Installing the table replaces them there too, in the pairs, vectors and
 * records the library's bindings reach (`substitute-within!` in
 * `src/core/scheme/substitute.scm`), so that a procedure stays `eq?` to itself
 * and runs compiled however it is reached.
 */

import * as R from './runtime.js';
import { substituteLibraryValues, libraryNameToKey, recordCompiledOver } from '../core/interpreter/library_registry.js';
import { list } from '../core/interpreter/cons.js';
import { intern } from '../core/interpreter/symbol.js';

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
 * ones, and everywhere the library system holds those: in every library of the
 * current registry, and inside the values they hold.
 *
 * @param {Object} env - The environment holding the interpreted library.
 * @param {Object} table - A generated table: `{fingerprint, files, procedures}`.
 * @param {string} fingerprint - The fingerprint of the sources actually loaded.
 * @returns {{installed: Array<string>, restored: Array<string>,
 *   skipped: Array<{name: string, reason: string}>, stale: boolean}} What was
 *   installed, what the table had restored already, and what was left
 *   interpreted.
 */
export function installPrebuilt(env, table, fingerprint) {
  return substituteInstalled(installProcedures(env, table, fingerprint), env);
}

/**
 * Installs prebuilt procedures into an environment, replacing the interpreted
 * ones there and nowhere else.
 *
 * For a library no registry holds: the library system's own, which its seed
 * loads before there is a registry, each library installed before the next
 * imports it.
 *
 * @param {Object} env - The environment holding the interpreted library.
 * @param {Object} table - A generated table: `{fingerprint, files, procedures}`.
 * @param {string} fingerprint - The fingerprint of the sources actually loaded.
 * @returns {{installed: Array<string>, restored: Array<string>,
 *   skipped: Array<{name: string, reason: string}>, stale: boolean,
 *   replaced: Map<Function, Function>}} What was installed, what the table
 *   had restored already (`restoreProcedure`), what was left interpreted, and
 *   each closure replaced, mapped to the procedure that replaced it.
 */
export function installProcedures(env, table, fingerprint) {
  const installed = [];
  const restored = [];
  const skipped = [];
  const replaced = new Map();
  if (table === undefined || table.fingerprint !== fingerprint) {
    return { installed, restored, skipped, stale: true, replaced };
  }

  for (const [name, entry] of Object.entries(table.procedures)) {
    const closure = env.bindings.get(name);
    // Restored from the table as the library loaded: under this name, or
    // under another and bound to this one too by a form such as SRFI 125's
    // `(define hash-table-exists? hash-table-contains?)`.
    if (typeof closure === 'function' && closure[RESTORED] !== undefined) {
      restored.push(name);
      continue;
    }
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
  return { installed, restored, skipped, stale: false, replaced };
}

/**
 * Puts procedures just installed wherever else the closures they replaced are.
 *
 * Libraries imported the interpreted closures by value, and values the library
 * made as it loaded hold them; `substituteLibraryValues` replaces both. The
 * closures are kept, for a debugger to run instead (`recordCompiledOver`).
 *
 * @param {{installed: Array<string>, skipped: Array<Object>, stale: boolean,
 *   replaced: Map<Function, Function>}} outcome - What `installProcedures` did.
 * @param {Object} env - The environment they were installed into.
 * @returns {{installed: Array<string>, restored: Array<string>,
 *   skipped: Array<{name: string, reason: string}>, stale: boolean}} The
 *   outcome, without the closures.
 */
function substituteInstalled({ installed, restored, skipped, stale, replaced }, env) {
  substituteLibraryValues(replaced);
  recordCompiledOver(replaced, env);
  return { installed, restored, skipped, stale };
}

/**
 * Installs a library's prebuilt table into the library's own environment, if
 * there is one and it was generated from the sources being loaded, and
 * everywhere the library system holds what it replaces (`installPrebuilt`).
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
 * @returns {{installed: Array<string>, restored: Array<string>,
 *   skipped: Array<{name: string, reason: string}>, stale: boolean}|null} What
 *   `installPrebuilt` did, or null if the library has no table.
 */
export function installLibraryTable(tables, libraryName, env, sourceOf) {
  const outcome = installLibraryProcedures(tables, libraryName, env, sourceOf);
  return outcome === null ? null : substituteInstalled(outcome, env);
}

/**
 * Installs a library's prebuilt table into the library's own environment, if
 * there is one and it was generated from the sources being loaded, replacing
 * the interpreted closures there and nowhere else (`installProcedures`).
 *
 * @param {Object<string, Object>} tables - As for `installLibraryTable`.
 * @param {string[]} libraryName - The library just loaded.
 * @param {Object} env - Its own environment.
 * @param {(file: string) => (string|undefined)} sourceOf - As for
 *   `installLibraryTable`.
 * @returns {Object|null} What `installProcedures` did, or null if the library
 *   has no table.
 */
export function installLibraryProcedures(tables, libraryName, env, sourceOf) {
  const table = tables[libraryNameToKey(libraryName)];
  if (table === undefined) return null;
  const stale = { installed: [], restored: [], skipped: [], stale: true, replaced: new Map() };
  if (table.runtime !== RUNTIME_INTERFACE) return stale;
  const sources = table.files.map(sourceOf);
  // A file the table was built from and the loader cannot find now means the
  // library has changed shape since the build, which is staleness too.
  if (sources.some((source) => typeof source !== 'string')) return stale;
  return installProcedures(env, table, fingerprintSources(sources));
}

/**
 * A restorer over prebuilt tables, for the library system: given a library's
 * name and the text of its files -- its `.sld`, then what it includes, as a
 * table's `files` lists them -- the table that restores the library, if there
 * is one, built against this runtime from that very text.
 *
 * Restoring is the library's top-level forms in the order loading runs them,
 * as the table's `restore` sequence gives them: each procedure the table holds
 * bound straight from its compiled code, with no closure made and nothing else
 * to change, since nothing holds a closure; each other form run as source is
 * (`evaluate-definition!` in src/core/scheme/library_system.scm). A procedure
 * restored so has no closure for a debugger to run instead; it is debugged as
 * compiled code is.
 *
 * @param {Object<string, Object>} tables - Generated tables, keyed by library
 *   name as `libraryNameToKey` writes it.
 * @returns {(libraryName: string[], texts: Array<*>) =>
 *   ({bind: (env: Object, name: string) => void, items: Cons}|null)} The
 *   restorer: what binds a restored procedure, and the sequence as a list of
 *   `(procedure name)` and `(form form)`; or null.
 */
export function libraryRestorer(tables) {
  return (libraryName, texts) => {
    const table = tables[libraryNameToKey(libraryName)];
    if (table === undefined || table.restore === undefined || table.runtime !== RUNTIME_INTERFACE) return null;
    if (texts.some((text) => typeof text !== 'string') || fingerprintSources(texts) !== table.fingerprint) {
      return null;
    }
    return {
      bind: (env, name) => restoreProcedure(table, env, name),
      items: list(...table.restore.map((item) => item.procedure !== undefined
        ? list(PROCEDURE, intern(item.procedure))
        : list(FORM, item.form)))
    };
  };
}

/** The kinds of item in a restore sequence, as the library system reads them. */
const PROCEDURE = intern('procedure');
const FORM = intern('form');

/**
 * Binds a procedure a table restores, from its compiled code, in its
 * library's environment, which its globals resolve in as the closure's would
 * have.
 * @param {Object} table - The table.
 * @param {Object} env - The library's environment.
 * @param {string} name - The procedure's name.
 */
export function restoreProcedure(table, env, name) {
  const entry = table.procedures[name];
  const procedure = R.recordSource(entry.make(R, env, entry.constants), entry.span);
  procedure[RESTORED] = entry;
  env.define(name, procedure);
}

/**
 * The entry a procedure was restored from, on the procedure, so that the
 * table's installing, which follows, counts it restored rather than skipped.
 */
const RESTORED = Symbol('restored from');

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
