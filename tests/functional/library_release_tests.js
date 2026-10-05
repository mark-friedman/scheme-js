/**
 * @fileoverview A library goes once nothing can reach it, and not before.
 *
 * The analyzer finds a library's environment, and the keywords the library
 * binds, by the library's scope, in tables kept for the whole process. A
 * process that makes many library registries -- a benchmark running each
 * program in an interpreter of its own, a tool's private registry -- loads a
 * library afresh into each, and if those tables held every library they were
 * given, each registry's libraries, and through each library's environment
 * the global environment of the program that loaded them, would live as long
 * as the process.
 *
 * A library must stay found, though, for as long as a macro of its own can
 * still be expanded: its template's identifiers name the library's bindings,
 * and its keywords, by the library's scope. The process-wide registry of
 * macros by name can keep such a macro after its registry is gone, and the
 * macro must expand there as it did in its own.
 *
 * Whether something has been collected is observable only from JavaScript,
 * and only where the garbage collector can be run on demand: under Node with
 * `--expose-gc`, as `npm test` runs. Elsewhere the collection tests are
 * skipped.
 */

import { assert, skip } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { loadLibrarySync } from '../../src/core/interpreter/library_loader.js';
import { withPrivateLibraries, getLibraryEnv } from '../../src/core/interpreter/library_registry.js';
import { globalMacroRegistry } from '../../src/core/interpreter/macro_registry.js';
import { globalContext, InterpreterContext } from '../../src/core/interpreter/context.js';
import { GLOBAL_SCOPE_ID } from '../../src/core/interpreter/syntax_object.js';

/**
 * Two libraries, one using the other's macro under a name it gives it.
 *
 * `probe-macro` expands into a call of `probe-secret`, which `(release probe)`
 * does not export, so a use outside the library reaches it only through the
 * library's environment. `user-macro` expands into `probe`, the name
 * `(release user)` imports `probe-macro` under, which nothing defines by name,
 * so its expansion finds the macro only through the keywords `(release user)`
 * binds.
 * @type {Object<string, string>}
 */
const SOURCES = {
  'release.probe': `
    (define-library (release probe)
      (import (scheme primitives))
      (export probe-macro)
      (begin
        (define secret 42)
        (define (probe-secret) secret)
        (define-syntax probe-macro
          (syntax-rules ()
            ((_) (probe-secret))))))`,
  'release.user': `
    (define-library (release user)
      (import (scheme primitives) (rename (release probe) (probe-macro probe)))
      (export user-macro)
      (begin
        (define-syntax user-macro
          (syntax-rules ()
            ((_) (probe))))))`
};

/**
 * Finds the libraries above by name.
 * @param {string[]} name - A library name.
 * @returns {string} Its source.
 */
function resolver(name) {
  const source = SOURCES[name.join('.')];
  if (source === undefined) throw new Error(`no library ${name.join(' ')}`);
  return source;
}

/**
 * Loads `(release user)`, and so `(release probe)`, into a registry of their
 * own, handing each library's environment to `watch`, and returns what
 * `watch` returned and the libraries' scopes, leaving nothing else reachable
 * from what is returned.
 *
 * The libraries define their macros by name for the process, in a copy of
 * the registry of macros by name. Unless `keepMacros` is set, the registry is
 * put back as it was before this returns, as `benchmarks/run_tier.js` puts it
 * back after each program; otherwise the copy stays until `restoreMacros` is
 * called. Either way the copy is reached only through the registry: a caller
 * that is an async function keeps every value its frame has held while it
 * waits, and would keep the libraries alive through one.
 *
 * @param {boolean} keepMacros - Whether to leave the libraries' macros
 *   defined by name.
 * @param {(name: string, env: Object) => *} [watch] - Given each library's
 *   name and environment.
 * @returns {{probe: *, user: *, scopes: number[], restoreMacros: Function}}
 */
function loadReleasable(keepMacros, watch = () => null) {
  const saved = globalMacroRegistry.macros;
  const restoreMacros = () => { globalMacroRegistry.macros = saved; };
  globalMacroRegistry.macros = new Map(saved);
  try {
    return withPrivateLibraries({ resolver }, () => {
      const { interpreter, env } = createInterpreter();
      loadLibrarySync(['release', 'user'], analyze, interpreter, env);
      const probe = getLibraryEnv(['release', 'probe']);
      const user = getLibraryEnv(['release', 'user']);
      return {
        probe: watch('probe', probe),
        user: watch('user', user),
        scopes: [probe.libraryScope, user.libraryScope],
        restoreMacros
      };
    });
  } finally {
    if (!keepMacros) restoreMacros();
  }
}

/**
 * Loads the libraries and runs the garbage collector in the same job, as a
 * program run from start to end in one job loads and drops them -- as
 * `benchmarks/run_tier.js` runs every program it runs. A library must go
 * then, not only once the job has ended, which is when a `WeakRef` lets go
 * of what it refers to.
 * @param {FinalizationRegistry} observer - Given each library's environment,
 *   under its name; observing it keeps nothing alive.
 */
function loadAndCollectInOneJob(observer) {
  loadReleasable(false, (name, env) => observer.register(env, name));
  globalThis.gc();
}

/**
 * Evaluates `(user-macro)` at the top level of a fresh interpreter and
 * library registry, where it is found by name.
 * @returns {*} Its value, or the message of the error it raised.
 */
function useUserMacro() {
  try {
    return withPrivateLibraries({ resolver }, () => {
      const { interpreter, env } = createInterpreter();
      return interpreter.run(analyze(parse('(user-macro)')[0]), env);
    });
  } catch (e) {
    return e.message;
  }
}

/**
 * Lets the next job run.
 * @returns {Promise<void>}
 */
const tick = () => new Promise((resolve) => setTimeout(resolve, 0));

/**
 * Runs the garbage collector over everything the current job has finished
 * with: a `WeakRef` this file makes keeps its target until the job that made
 * or read it ends, so the collection waits for the next one.
 * @returns {Promise<void>}
 */
async function collect() {
  await tick();
  globalThis.gc();
  await tick();
  globalThis.gc();
}

/**
 * Runs the library-release tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runLibraryReleaseTests(logger) {
  logger.title('Library release - a fresh scope is never the top level\'s');
  {
    // The top level's scope is GLOBAL_SCOPE_ID; a library's comes from
    // `freshScope`. Were the first the same number, the first library loaded
    // in a process would be taken for the top level, and the top level for
    // it: every top-level macro would be that library's, its templates'
    // names that library's bindings.
    const context = new InterpreterContext();
    assert(logger, 'the first scope a context makes', context.freshScope() !== GLOBAL_SCOPE_ID, true);
    context.reset();
    assert(logger, 'and the first after a reset', context.freshScope() !== GLOBAL_SCOPE_ID, true);
    withPrivateLibraries({ resolver }, () => {
      const { interpreter, env } = createInterpreter();
      loadLibrarySync(['release', 'probe'], analyze, interpreter, env);
      assert(logger, 'no library\'s scope is the top level\'s',
        globalContext.lookupLibraryEnv(GLOBAL_SCOPE_ID), undefined);
    });
    // Only the first library a process loads could have had the top level's
    // scope, so this is seen only in a process of its own: the CLI's first
    // library is (scheme base), whose environment binds `log` from (scheme
    // primitives), and a program that defines a `log` of its own must have
    // its macro call that one.
    if (typeof process !== 'undefined' && process.versions?.node) {
      const { runCli } = await import('../harness/cli_process.js');
      const { stdout } = await runCli(['-e', `(define (log . items) 'logged)
        (define-syntax note (syntax-rules () ((_ x) (log x))))
        (write (note 1))`]);
      assert(logger, 'a top-level macro\'s template names the top level\'s bindings, in a process of its own',
        stdout, 'logged');
    }
  }

  logger.title('Library release - a registry takes its libraries\' scopes with it');
  {
    const { scopes } = loadReleasable(false);
    assert(logger, 'their entries go when the registry does',
      scopes.filter((scope) => globalContext.lookupLibraryEnv(scope) !== undefined), []);
  }

  logger.title('Library release - a macro outliving its registry expands as it did');
  {
    // The libraries' registry goes, and their scopes' entries with it; their
    // macros stay, defined by name for the process, and a program in another
    // registry that uses one by name gets the expansion it would have got in
    // theirs: a call of `probe-secret`, which only (release probe)'s
    // environment binds, through `probe`, which only (release user)'s
    // keywords bind.
    const kept = loadReleasable(true);
    const interned = globalContext.syntaxInternCache.size;
    try {
      assert(logger, 'used by name in another registry', useUserMacro(), 42n);
    } finally {
      kept.restoreMacros();
    }
    assert(logger, 'and the entries it put back go with that registry',
      kept.scopes.filter((scope) => globalContext.lookupLibraryEnv(scope) !== undefined), []);
    // Each expansion makes a scope of its own, so what it interns is new
    // each time, and stays only if kept.
    assert(logger, 'and so do the syntax objects its expansions interned',
      globalContext.syntaxInternCache.size, interned);
  }

  if (typeof globalThis.gc !== 'function') {
    skip(logger, 'Library release - collection', 'needs the garbage collector exposed: node --expose-gc');
    return;
  }

  logger.title('Library release - a library nothing reaches goes');
  {
    const collected = new Set();
    const observer = new FinalizationRegistry((name) => collected.add(name));
    loadAndCollectInOneJob(observer);
    // The observer is told in a later job; nothing is collected meanwhile.
    for (let i = 0; i < 50 && collected.size < 2; i++) await tick();
    assert(logger, 'in the job that dropped it: the library whose macro another library imported',
      collected.has('probe'), true);
    assert(logger, 'and the library that imported it', collected.has('user'), true);
  }

  logger.title('Library release - a macro that lives keeps its library');
  {
    const kept = loadReleasable(true, (name, env) => new WeakRef(env));
    try {
      await collect();
      assert(logger, 'the library of a macro defined by name', kept.user.deref() !== undefined, true);
      assert(logger, 'and the library whose macro its expansion uses', kept.probe.deref() !== undefined, true);
    } finally {
      kept.restoreMacros();
    }
    await collect();
    assert(logger, 'and goes once the macro does', kept.user.deref(), undefined);
    assert(logger, 'with the library whose macro it used', kept.probe.deref(), undefined);
  }
}
