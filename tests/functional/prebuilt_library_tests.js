/**
 * @fileoverview Libraries compiled at build time, installed as they load.
 *
 * Every library the bundle ships, and the compiler's own library, has a table
 * of its procedures compiled at build time. A table is installed into its
 * library's environment when the library loads, so nothing has to run the
 * compiler to get compiled libraries -- which is what lets the bundle leave
 * the compiler out.
 *
 * These pin the pieces that makes work: the tables match the sources in this
 * tree; a table is installed as its library loads and refused when stale; a
 * library's table holds only what that library defines; and the compiler loads
 * its library into a registry of its own, apart from the program's.
 */

import { assert } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { loadLibrarySync, parseDefineLibrary } from '../../src/core/interpreter/library_loader.js';
import { writeString } from '../../src/core/primitives/io/printer.js';
import {
  withPrivateLibraries, getFileResolver, setFileResolver, setLibraryLoadHook, isCompiledOver,
  isLibraryLoaded, getLibraryEnv, getLibraryExports, registerLibrary, libraryNameToKey
} from '../../src/core/interpreter/library_registry.js';
import { Environment } from '../../src/core/interpreter/environment.js';
import { generateEnvironment } from '../../src/compiler/index.js';
import { installLibraryTable, libraryRestorer, fingerprintSources, RUNTIME_INTERFACE, decodeDatum } from '../../src/compiler/prebuilt.js';
import {
  compilerEnvironment, compilerExports, compilerSourceOf, COMPILER_LIBRARY
} from '../../src/compiler/lowering.js';
import { callSchemeProcedure } from '../../src/core/interpreter/values.js';
import { intern } from '../../src/core/interpreter/symbol.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';
import LIBRARIES from '../../src/packaging/compiled_libraries.js';
import COMPILER from '../../src/packaging/compiled_compiler.js';

/**
 * Finds a bundled library's file by the last part of its name.
 * @param {string[]} name - A library name, or an include's path.
 * @returns {string} Its source.
 */
function bundledResolver(name) {
  const last = name[name.length - 1];
  const source = BUNDLED_SOURCES[`${last}.sld`] ?? BUNDLED_SOURCES[last];
  if (source === undefined) throw new Error(`no bundled file for ${name.join('/')}`);
  return source;
}

/**
 * Loads a bundled library into a registry of its own, installing each
 * library's table as it loads, the way the bundle does.
 * @param {string[]} name - The library to load.
 * @param {(file: string) => (string|undefined)} [sourceOf] - Where the
 *   install reads the sources it fingerprints; the bundled ones by default.
 * @returns {{exports: Map<string, *>, outcomes: Map<string, Object>}} The
 *   library's exports, and what installing each library's table did.
 */
function loadBundled(name, sourceOf = (file) => BUNDLED_SOURCES[file]) {
  const outcomes = new Map();
  const hook = (loaded, env) => {
    const outcome = installLibraryTable(LIBRARIES, loaded, env, sourceOf);
    if (outcome !== null) outcomes.set(libraryNameToKey(loaded), outcome);
  };
  const exports = withPrivateLibraries({ resolver: bundledResolver, hook }, () => {
    const { interpreter, env } = createInterpreter();
    return loadLibrarySync(name, analyze, interpreter, env);
  });
  return { exports, outcomes };
}

/**
 * Runs the prebuilt-library tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runPrebuiltLibraryTests(logger) {
  logger.title('Prebuilt libraries - the tables match the sources');

  // The build runs as part of `npm test`, so every table must match the
  // sources in the same tree; one that does not means every other assertion
  // here would be testing a stale artifact.
  for (const [key, table] of Object.entries(LIBRARIES)) {
    assert(logger, `the table for (${key.replace('.', ' ')}) matches its sources`,
      fingerprintSources(table.files.map((file) => BUNDLED_SOURCES[file])), table.fingerprint);
  }
  {
    const key = libraryNameToKey(COMPILER_LIBRARY);
    const table = COMPILER[key];
    assert(logger, "the compiler's table is keyed by its library's name", table !== undefined, true);
    assert(logger, "the compiler's table matches its sources",
      table && fingerprintSources(table.files.map(compilerSourceOf)), table && table.fingerprint);
    assert(logger, "the compiler's table covers its .sld and every file it includes",
      table && table.files.join(' '), 'compiler.sld ir.scm lift.scm inline.scm liveness.scm emit.scm sourcemap.scm driver.scm safety.scm tier.scm');
  }
  assert(logger, 'the libraries a page uses most have tables',
    ['scheme.core', 'scheme.lazy', 'srfi.1', 'srfi.125', 'srfi.128', 'srfi.152']
      .filter((key) => LIBRARIES[key] === undefined), []);

  logger.title('Prebuilt libraries - the data a table holds, as JSON');
  assert(logger, 'every kind of datum, decoded',
    writeString(decodeDatum('["l","a",["s","b"],12,["n","123456789012345678901234567890"],["f","1.5"],["c",97],'
      + 'null,["d",3,2],["v",1,"b"],true]')),
    '(a "b" 12 123456789012345678901234567890 1.5 #\\a () (2 . 3) #(1 b) #t)');
  assert(logger, "and a library's environment, by its name", decodeDatum('["e","scheme","core"]').library,
    ['scheme', 'core']);

  logger.title('Prebuilt libraries - each table restores its library');
  {
    // A table restores its library from its `restore` sequence: every
    // top-level form loading the library runs -- its `begin` forms, then its
    // included files' -- in that order, each a procedure the table's code
    // binds, the core form the form expanded into -- a macro's definition as
    // one that binds it pending -- or the form itself. Read back against the
    // files it was built from.
    const sourceForms = (files, sourceOf) => {
      const definition = parseDefineLibrary(parse(sourceOf(files[0]))[0]);
      return [...definition.body,
        ...definition.includes.flatMap((file) => parse(sourceOf(file))),
        ...definition.includesCi.flatMap((file) => parse(sourceOf(file), { caseFold: true }))];
    };
    const defines = (form, name) => form.car?.name === 'define'
      && (form.cdr.car.car?.name === name || form.cdr.car.name === name);
    const definedName = (form) => form.cdr.car.car?.name ?? form.cdr.car.name;
    const coreTags = new Set(['lit', 'var', 'library-var', 'scoped-var', 'if', 'seq', 'lambda', 'letrec', 'set',
      'library-set', 'define', 'app', 'import', 'define-library', 'define-syntax']);
    const tables = [...Object.entries(LIBRARIES).map(([key, table]) => [key, table, (f) => BUNDLED_SOURCES[f]]),
      ...Object.entries(COMPILER).map(([key, table]) => [key, table, compilerSourceOf])];
    for (const [key, table, sourceOf] of tables) {
      const library = `(${key.replace('.', ' ')})`;
      const restore = table.restore ?? [];
      const forms = sourceForms(table.files, sourceOf);
      const wrong = restore.map((item, i) => {
        if (forms[i] === undefined) return `item ${i}: no form`;
        if (item.procedure !== undefined) {
          if (!defines(forms[i], item.procedure)) return `item ${i}: ${item.procedure} is not defined there`;
          if (table.procedures[item.procedure]?.span === undefined) return `item ${i}: ${item.procedure} has no span`;
          return null;
        }
        if (item.core !== undefined) {
          const core = decodeDatum(item.core);
          const tag = core.car?.name;
          if (tag === 'define-syntax') {
            return writeString(core.cdr.cdr.car) === writeString(forms[i]) ? null : `item ${i}: not the macro there`;
          }
          if (forms[i].car?.name === 'define' && !(tag === 'define' && core.cdr.car.name === definedName(forms[i]))) {
            return `item ${i}: not the definition there`;
          }
          return coreTags.has(tag) ? null : `item ${i}: not a core form`;
        }
        return writeString(decodeDatum(item.form)) === writeString(forms[i]) ? null : `item ${i}: not the form there`;
      }).filter((problem) => problem !== null);
      assert(logger, `${library} restores from a sequence of its forms, in order`,
        [table.restore !== undefined, restore.length, wrong], [true, forms.length, []]);
    }
    const core = LIBRARIES['scheme.core'].restore;
    const coreOf = (item) => (item.core === undefined ? null : decodeDatum(item.core));
    assert(logger, "(scheme core)'s macros are defined pending", core.some((item) =>
      coreOf(item)?.car?.name === 'define-syntax' && coreOf(item).cdr.car.name === 'cond'), true);
    assert(logger, 'and its procedures are restored', core.some((item) => item.procedure === 'map'), true);
    assert(logger, 'a library of macros alone has a table that restores it',
      LIBRARIES['scheme.control']?.restore?.every((item) => coreOf(item)?.car?.name === 'define-syntax'), true);
  }

  logger.title('Prebuilt libraries - each procedure declares the runtime values it names');
  {
    // The emitter notes each runtime value as it writes its name, and declares
    // those a procedure's code uses; a name written without being noted would
    // fail only when the code naming it ran. So every procedure in the shipped
    // tables is read back: the runtime values it names are those it declares.
    const procedures = [...Object.values(LIBRARIES), ...Object.values(COMPILER)]
      .flatMap((table) => Object.values(table.procedures));
    const declaredIn = (code) =>
      [...(code.match(/const ((?:\$\w+ = R\.\w+(?:, )?)+);/)?.[1] ?? '').matchAll(/(\$\w+) = /g)]
        .map((m) => m[1]);
    const runtimeNames = new Set(procedures.flatMap((p) => declaredIn(p.make.toString())));
    const namedIn = (code) => [...runtimeNames].filter((name) =>
      new RegExp(`(?<![\\w$])${name.replace('$', '\\$')}(?![\\w$])`).test(code.replace(/const (?:\$\w+ = R\.\w+(?:, )?)+;/, '')));
    const wrong = procedures
      .map((p) => p.make.toString())
      .filter((code) => namedIn(code).sort().join() !== declaredIn(code).sort().join());
    assert(logger, 'every runtime value is declared by some procedure', runtimeNames.size, 12);
    assert(logger, 'and each procedure declares exactly those its code names', wrong.length, 0);
  }

  logger.title('Prebuilt libraries - a registry keeps its own records');
  {
    // A compiled procedure installed over its closure is recorded, so that a
    // debugger can run the closure instead; by the registry it was installed
    // in, which takes the record with it when it goes, rather than in one
    // table for the process, which kept every library a short-lived registry
    // had loaded alive.
    let inside = null;
    const hook = (loaded, env) => { installLibraryTable(LIBRARIES, loaded, env, (file) => BUNDLED_SOURCES[file]); };
    const exports = withPrivateLibraries({ resolver: bundledResolver, hook }, () => {
      const { interpreter, env } = createInterpreter();
      const loaded = loadLibrarySync(['srfi', '1'], analyze, interpreter, env);
      inside = isCompiledOver(loaded.get('fold'));
      return loaded;
    });
    assert(logger, 'a procedure installed over its closure is recorded where it was installed', inside, true);
    assert(logger, 'and not in another registry', isCompiledOver(exports.get('fold')), false);
  }

  logger.title('Prebuilt libraries - installed as each library loads');
  {
    const { exports, outcomes } = loadBundled(['srfi', '125']);
    assert(logger, 'a library loaded by name arrives compiled',
      [exports.get('hash-table-ref').$compiled, exports.get('make-hash-table').$compiled], [true, true]);
    assert(logger, 'and so does each library it imports',
      outcomes.get('srfi.128') && outcomes.get('srfi.128').installed.length > 20, true);
    assert(logger, 'with nothing stale or skipped',
      [...outcomes.values()].filter((o) => o.stale || o.skipped.length > 0).length, 0);
  }
  {
    // A stale build is the failure that would matter: prebuilt code quietly
    // doing what an older version of the source said.
    const edited = (file) => (file === 'list_lib.scm' ? `${BUNDLED_SOURCES[file]}\n` : BUNDLED_SOURCES[file]);
    const { exports, outcomes } = loadBundled(['srfi', '1'], edited);
    assert(logger, 'a library whose source changed since the build installs nothing',
      [outcomes.get('srfi.1').stale, outcomes.get('srfi.1').installed.length], [true, 0]);
    assert(logger, 'and stays interpreted', exports.get('fold').$compiled === true, false);
    assert(logger, 'while the libraries it imports still install',
      outcomes.get('scheme.core').stale, false);
  }
  {
    const missing = (file) => (file === 'list_lib.scm' ? undefined : BUNDLED_SOURCES[file]);
    const { outcomes } = loadBundled(['srfi', '1'], missing);
    assert(logger, 'a file the table lists and the loader cannot find counts as stale',
      outcomes.get('srfi.1').stale, true);
  }
  {
    // The fingerprint covers the Scheme a table was compiled from, not the
    // runtime its code calls. Code generated against a runtime whose
    // interface has since changed -- a function renamed or removed -- would
    // fail when installed, or worse, when called.
    assert(logger, 'every table records the runtime interface it was generated against',
      [...Object.values(LIBRARIES), ...Object.values(COMPILER)]
        .filter((table) => table.runtime !== RUNTIME_INTERFACE).length, 0);
    const older = Object.fromEntries(Object.entries(LIBRARIES)
      .map(([key, table]) => [key, { ...table, runtime: 'an older runtime' }]));
    const outcome = withPrivateLibraries({ resolver: bundledResolver }, () => {
      const { interpreter, env } = createInterpreter();
      let result = null;
      setLibraryLoadHook((name, libraryEnv) => {
        if (libraryNameToKey(name) === 'srfi.1') {
          result = installLibraryTable(older, name, libraryEnv, (file) => BUNDLED_SOURCES[file]);
        }
      });
      loadLibrarySync(['srfi', '1'], analyze, interpreter, env);
      return result;
    });
    assert(logger, 'a table generated against another runtime interface installs nothing',
      [outcome.stale, outcome.installed.length], [true, 0]);
  }
  assert(logger, 'a library with no table is left alone',
    installLibraryTable(LIBRARIES, ['test', 'no-table'], new Environment(null), () => ''), null);

  logger.title('Prebuilt libraries - restored from their tables, without their source running');
  {
    // As the entry points load them: each library restored from its table
    // where the table matches its files, and then installed as before.
    const restoreBundled = (name, resolver = bundledResolver) => {
      const outcomes = new Map();
      const hook = (loaded, env) => {
        const outcome = installLibraryTable(LIBRARIES, loaded, env, (file) => resolver([file]));
        if (outcome !== null) outcomes.set(libraryNameToKey(loaded), outcome);
      };
      return withPrivateLibraries({ resolver, hook, restorer: libraryRestorer(LIBRARIES) }, () => {
        const { interpreter, env } = createInterpreter();
        const exports = loadLibrarySync(name, analyze, interpreter, env);
        const run = (text) => interpreter.run(analyze(parse(text)[0]), env, [], undefined, { jsAutoConvert: 'raw' });
        loadLibrarySync(['scheme', 'base'], analyze, interpreter, env);
        run('(import (scheme base) (srfi 128))');
        return { exports, outcomes, run, core: getLibraryEnv(['scheme', 'core']) };
      });
    };
    const { exports, outcomes, run, core } = restoreBundled(['srfi', '128']);
    const map = core.bindings.get('map');
    assert(logger, "(scheme core)'s procedures are restored, none installed over a closure",
      [outcomes.get('scheme.core').restored.length, outcomes.get('scheme.core').installed.length,
        outcomes.get('scheme.core').skipped.length],
      [Object.keys(LIBRARIES['scheme.core'].procedures).length, 0, 0]);
    assert(logger, 'a procedure restored is compiled, and was never a closure the debugger could run',
      [map.$compiled === true, isCompiledOver(map)], [true, false]);
    assert(logger, 'and knows where its source is', map.source?.filename, 'list.scm');
    assert(logger, "(scheme core)'s macros work, its forms having run", run("(cond ((assq 'b '((a 1) (b 2))) => cadr))"), 2n);
    assert(logger, 'what a library makes as it loads holds its restored procedures',
      run('(eq? (comparator-hash-function (make-default-comparator)) default-hash)'), true);
    assert(logger, 'and its exports are those procedures',
      [exports.get('default-hash').$compiled === true, isCompiledOver(exports.get('default-hash'))], [true, false]);

    const edited = (path) => {
      const file = path[path.length - 1];
      const source = bundledResolver(path);
      return file === 'list.scm' ? `${source}\n` : source;
    };
    const stale = restoreBundled(['srfi', '128'], edited);
    assert(logger, 'a library whose files changed since the build loads from its source, interpreted',
      [stale.outcomes.get('scheme.core').stale, stale.core.bindings.get('map').$compiled === true], [true, false]);
    assert(logger, 'while the libraries that import it are restored still',
      stale.outcomes.get('srfi.128').restored.length > 20, true);
  }

  logger.title('Prebuilt libraries - what a library made as it loaded holds its compiled procedures');
  {
    // Loaded from source, a library makes values as it loads that hold its
    // closures -- SRFI 128's comparators, the current ports' parameter cells
    // -- and its table is installed afterwards. Each closure the table
    // compiles is made to run compiled in place, so those values hold
    // procedures that run compiled, with nothing searched. Every library with
    // a table is loaded so, as one is when its table is stale.
    const libraryEnvs = [];
    const hook = (loaded, env) => {
      if (!env) return;
      libraryEnvs.push([libraryNameToKey(loaded), env]);
      installLibraryTable(LIBRARIES, loaded, env, (file) => BUNDLED_SOURCES[file]);
    };
    const { core, srfi128 } = withPrivateLibraries({ resolver: bundledResolver, hook }, () => {
      const { interpreter, env } = createInterpreter();
      for (const key of Object.keys(LIBRARIES)) loadLibrarySync(key.split('.'), analyze, interpreter, env);
      return { core: getLibraryEnv(['scheme', 'core']), srfi128: getLibraryEnv(['srfi', '128']) };
    });
    assert(logger, 'every library with a table was loaded and installed',
      Object.keys(LIBRARIES).filter((key) => !libraryEnvs.some(([loaded]) => loaded === key)), []);
    const interpreted = libraryEnvs.filter(([key]) => LIBRARIES[key] !== undefined)
      .flatMap(([key, env]) => Object.keys(LIBRARIES[key].procedures)
      .filter((name) => env.bindings.get(name)?.$compiled !== true).map((name) => `(${key}) ${name}`));
    assert(logger, 'every procedure a table holds runs compiled where its library binds it', interpreted, []);
    assert(logger, "so a current port's parameter cell holds its compiled converter",
      core.bindings.get('current-output-port-cell').car.$compiled, true);
    const hash = srfi128.bindings.get('default-comparator').hash;
    assert(logger, "and SRFI 128's default comparator holds default-hash, the object bound, compiled",
      [hash === srfi128.bindings.get('default-hash'), hash.$compiled === true], [true, true]);
  }

  logger.title("Prebuilt libraries - a library's table holds only what it defines");
  {
    const sources = {
      'base-lib': `(define-library (test base-lib) (import (scheme base)) (export double)
                     (begin (define (double x) (* 2 x))))`,
      'user-lib': `(define-library (test user-lib) (import (scheme base) (test base-lib))
                     (export quadruple scaled)
                     (begin (define (quadruple x) (double (double x)))
                            (define scaled (let ((k 3)) (lambda (x) (* k x))))))`
    };
    const resolver = (name) => sources[name[name.length - 1]] ?? bundledResolver(name);
    const env = withPrivateLibraries({ resolver }, () => {
      const { interpreter, env: global } = createInterpreter();
      loadLibrarySync(['test', 'user-lib'], analyze, interpreter, global);
      return getLibraryEnv(['test', 'user-lib']);
    });
    const names = (options) => generateEnvironment(env, options).generated
      .map((entry) => entry.name).filter((name) => ['double', 'quadruple', 'scaled'].includes(name))
      .sort();
    assert(logger, 'without ownOnly, an imported procedure is compiled too',
      names({}), ['double', 'quadruple']);
    assert(logger, "with ownOnly, only the library's own procedures are",
      names({ ownOnly: true }), ['quadruple']);
    // `scaled` closes over the `let`'s `k`, which code generated in one run
    // would look up under the name that run's renaming gave it, and another
    // run, installing the table, gives it another.
    const declined = generateEnvironment(env, { ownOnly: true }).declined.find((d) => d.name === 'scaled');
    assert(logger, 'a procedure made inside another, closing over its locals, is declined, saying why',
      /made inside a procedure/.test(declined?.reason ?? ''), true);
  }

  logger.title('Private libraries - loading apart from the program');
  {
    const savedResolver = getFileResolver();
    const outsideHook = () => { throw new Error('the outside hook ran inside'); };
    const outsideResolver = () => { throw new Error('the outside resolver ran inside'); };
    registerLibrary('test.outside', new Map([['x', 1]]), new Environment(null));
    setFileResolver(outsideResolver);
    setLibraryLoadHook(outsideHook);
    try {
      const inner = { hooked: [] };
      withPrivateLibraries({
        // Each library it is asked for defines y, with the special forms the
        // bundle's library gives it.
        resolver: (name) => (name.join(' ') === 'scheme-js special-forms'
          ? BUNDLED_SOURCES['special-forms.sld']
          : `(define-library ${`(${name.join(' ')})`} (export y) (import (scheme-js special-forms)) (begin (define y 2)))`),
        hook: (name) => inner.hooked.push(name.join(' '))
      }, () => {
        inner.sawOutside = isLibraryLoaded('test.outside');
        const { interpreter, env } = createInterpreter();
        loadLibrarySync(['test', 'inside'], analyze, interpreter, env);
        inner.sawInside = isLibraryLoaded('test.inside');
      });
      assert(logger, 'inside, a library loaded outside is not found', inner.sawOutside, false);
      assert(logger, 'inside, the given resolver and hook are used',
        [inner.sawInside, inner.hooked.join(',')], [true, 'scheme-js special-forms,test inside']);
      assert(logger, 'outside again, a library loaded inside is not found',
        isLibraryLoaded('test.inside'), false);
      assert(logger, 'and the ones loaded outside are back', isLibraryLoaded('test.outside'), true);
      assert(logger, 'as are the resolver', getFileResolver() === outsideResolver, true);

      let thrown = null;
      try {
        withPrivateLibraries({ resolver: () => '' }, () => { throw new Error('inside'); });
      } catch (e) {
        thrown = e.message;
      }
      assert(logger, 'an exception inside passes through', thrown, 'inside');
      assert(logger, 'and everything is restored after it',
        [getFileResolver() === outsideResolver, isLibraryLoaded('test.outside')], [true, true]);
    } finally {
      setLibraryLoadHook(null);
      setFileResolver(savedResolver);
    }
  }

  logger.title('The compiler is a library of its own');
  {
    // Asking for its environment bootstraps the compiler, if nothing has yet.
    const { env } = compilerEnvironment();
    assert(logger, 'its Scheme runs in its library environment',
      env.lookup('lower-lambda') === env.bindings.get('lower-lambda'), true);
    assert(logger, 'which installed its prebuilt table',
      ['lower-lambda', 'generate-unit', 'js-name'].map((name) => env.lookup(name).$compiled),
      [true, true, true]);
    assert(logger, 'as did the libraries it imports',
      env.lookup('fold').$compiled && env.lookup('string-index').$compiled, true);
    assert(logger, "SRFI 1's private helpers stay private to SRFI 1",
      ['cars-of', 'check-procedure'].filter((name) => env.findEnv(name) !== null), []);
    assert(logger, "its SRFI 1 is not the program's",
      !isLibraryLoaded('srfi.1') || getLibraryExports('srfi.1').get('fold') !== env.lookup('fold'), true);
    assert(logger, "the compiler's library is not the program's to find",
      isLibraryLoaded('scheme-js.compiler'), false);

    // JavaScript reaches the compiler only by its exports, and calls them as
    // it calls any Scheme procedure: through the public interop.
    const exports = compilerExports();
    assert(logger, 'its entry points are its library\'s exports',
      ['compile-definition', 'make-tier', 'js-name'].map((name) => exports.get(name) === env.lookup(name)),
      [true, true, true]);
    assert(logger, 'called by a plain call, which converts its result for JavaScript',
      exports.get('js-name')(intern('x_$1')), callSchemeProcedure(exports.get('js-name'), [intern('x_$1')]).toString());
    assert(logger, 'and so a JavaScript string',
      typeof exports.get('js-name')(intern('x_$1')), 'string');
  }
}
