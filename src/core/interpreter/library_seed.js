/**
 * @fileoverview The library system's seed: what loads the library system,
 * which loads every other library.
 *
 * The library system is Scheme (src/core/scheme/library_system.scm), and is
 * itself a library, written with `(scheme core)` and `(scheme control)`, which
 * are libraries too; it reads every other library's files with the reader,
 * `(scheme-js reader)`, and every form is expanded by the expander,
 * `(scheme-js expander)`, two more. Something has to load those five before
 * there is a library system to do it, and this is that: a loader for exactly
 * the declarations they use -- `import` of libraries it has loaded already or
 * of the built-in `(scheme primitives)`, `include`, `begin` and `export` -- and
 * no more. Their files are the bundled sources
 * (src/packaging/bundled_libraries.js), which every host has at once, whatever
 * its resolver.
 *
 * Each library is restored from its prebuilt table where the table was built
 * from the bundled sources as they are, with no text read and no form
 * expanded: the table has its `define-library` form, as data; its procedures
 * are bound from their compiled code, and its other top-level forms are run,
 * in order, as the core forms they expanded into, a macro's definition as one
 * that binds the macro pending, its transformer made by the expander the
 * first time it is used (`libraryRestorer` in src/compiler/prebuilt.js).
 * Interpreted, the library system's loops -- binding each export a library
 * imports, finding each one it exports -- took a start from 159 ms to 246 on
 * the CLI. A library whose table is not current -- a seed library being
 * edited -- is read and expanded from its source, by the seed's own reader and
 * expander once they are loaded, and before them by the pinned ones
 * (src/packaging/pinned_seed.js): the libraries the reader and the expander are
 * made of, as core forms, run interpreted, which needs neither. A bundle,
 * built with its tables, carries no pinned seed. A table that does not match
 * leaves its procedures installed from what the table has once the source
 * has run; nothing else holds them, so nothing else is changed.
 *
 * They are loaded apart from every program, on an interpreter of their own,
 * and registered nowhere: the library system is a tool that runs Scheme on a
 * program's behalf, as the compiler is (src/compiler/lowering.js), and a
 * program that defined a procedure the library system uses, at its own top
 * level, must not change how its libraries load. A program's `(scheme core)`
 * is loaded for it by the library system, like any other library.
 */

import { Environment } from './environment.js';
import { Interpreter } from './interpreter.js';
import { analyze } from './expand.js';
import { assemble } from './assembler.js';
import { Executable } from './stepables_base.js';
import { globalContext } from './context.js';
import { globalMacroRegistry } from './macro_registry.js';
import { toArray, list } from './cons.js';
import { intern } from './symbol.js';
import { Symbol } from './symbol.js';
import { createGlobalEnvironment } from '../primitives/index.js';
import { BUNDLED_SOURCES } from '../../packaging/bundled_libraries.js';
import { createPrimitiveExports } from './library_loader.js';
import { SYNTAX_KEYWORDS } from './library_registry.js';
import { callSchemeProcedure } from './values.js';
import { SchemeLibraryError } from './errors.js';
import {
    installLibraryProcedures, libraryRestorer, fingerprintSources, decodeDatum, RUNTIME_INTERFACE
} from '../../compiler/prebuilt.js';
import prebuiltLibraries from '../../packaging/compiled_libraries.js';
import pinnedSeedImage from '../../packaging/pinned_seed.js';

/**
 * The libraries the seed loads, in the order they need each other: the reader
 * and the expander, which the library system reads and expands every other
 * library with, before it.
 */
const SEED_LIBRARIES = [['scheme', 'core'], ['scheme', 'control'], ['scheme-js', 'reader'],
    ['scheme-js', 'expander'], ['scheme-js', 'library-system']];

/** The libraries the reader and the expander are made of, which the pinned seed holds. */
const PINNED_LIBRARIES = SEED_LIBRARIES.slice(0, 4);

/**
 * A syntactic keyword one of the seed's libraries exports, as the expander
 * binds it where the keyword is imported.
 */
class SeedKeyword {
    /**
     * @param {string} keyword - The keyword's own name.
     * @param {Function|null} transformer - A macro's transformer, or null.
     */
    constructor(keyword, transformer) {
        this.keyword = keyword;
        this.transformer = transformer;
    }
}

/**
 * The seed made with the shipped tables, kept so that the system's other
 * libraries load beside the library system, on its interpreter
 * (`systemLibrary`).
 * @type {{interpreter: Interpreter, globalEnv: Environment, loaded: Map<string, Map<string, *>>,
 *   environments: Map<string, Environment>, tables: Object}|null}
 */
let shipped = null;

/**
 * Loads the library system.
 * @param {Object<string, Object>} [tables] - The prebuilt tables to install,
 *   by library key; the shipped ones by default, with which it is loaded
 *   once.
 * @returns {Map<string, Function>} The procedures `(scheme-js library-system)`
 *   exports, by name.
 */
export function seedLibrarySystem(tables = prebuiltLibraries) {
    if (tables !== prebuiltLibraries) return plant(tables).loaded.get('scheme-js.library-system');
    if (shipped === null) shipped = plant(tables);
    return shipped.loaded.get('scheme-js.library-system');
}

/**
 * An interpreter of the seed's own, with the seed's libraries loaded on it.
 * @param {Object<string, Object>} tables - The prebuilt tables to install.
 * @returns {{interpreter: Interpreter, globalEnv: Environment, loaded: Map<string, Map<string, *>>,
 *   environments: Map<string, Environment>, tables: Object}}
 *   It, its global environment, the exports of the libraries loaded and their
 *   environments, by key, and the tables.
 */
function plant(tables) {
    const interpreter = new Interpreter(globalContext);
    const globalEnv = createGlobalEnvironment(interpreter);
    interpreter.setGlobalEnv(globalEnv);
    const seed = {
        interpreter, globalEnv, tables,
        loaded: new Map([['scheme.primitives', createPrimitiveExports(globalEnv)]]),
        environments: new Map()
    };
    for (const name of SEED_LIBRARIES) seed.loaded.set(name.join('.'), seedLibrary(name, seed));
    return seed;
}

/**
 * One of the system's own libraries, loaded beside the library system, on its
 * interpreter, the first time it is asked for: one that runs Scheme on a
 * program's behalf, as the debugger's does, and so must not run where the
 * program's debugger could pause it. It is written with the seed's
 * libraries, and loaded as they are.
 * @param {string[]} name - The library's name.
 * @returns {Map<string, *>} Its exports, by name.
 */
export function systemLibrary(name) {
    if (shipped === null) shipped = plant(prebuiltLibraries);
    const key = name.join('.');
    if (!shipped.loaded.has(key)) shipped.loaded.set(key, seedLibrary(name, shipped));
    return shipped.loaded.get(key);
}

/**
 * Whether a library's prebuilt table is current: built, with its
 * `define-library` form, from the bundled sources as they are, against this
 * runtime.
 * @param {Object|undefined} table - The table.
 * @returns {boolean}
 */
function isCurrent(table) {
    return table !== undefined && table.declaration !== undefined && table.runtime === RUNTIME_INTERFACE
        && table.files.every((file) => typeof BUNDLED_SOURCES[file] === 'string')
        && fingerprintSources(table.files.map((file) => BUNDLED_SOURCES[file])) === table.fingerprint;
}

/**
 * Reads a text, for the seed: with the seed's reader once it is loaded, and
 * before that with the pinned one.
 * @param {Map<string, Map<string, *>>} loaded - The libraries loaded so far.
 * @param {string} text - The text.
 * @param {string} filename - The name its spans give it.
 * @returns {Array<*>} Its data.
 */
function seedRead(loaded, text, filename) {
    const reader = loaded.get('scheme-js.reader') ?? pinnedSeed().loaded.get('scheme-js.reader');
    return toArray(callSchemeProcedure(reader.get('read-source'), [text, filename, false, false]));
}

/**
 * Expands a form, for the seed: with the seed's expander once it is loaded,
 * and before that with the pinned one.
 * @param {Map<string, Map<string, *>>} loaded - The libraries loaded so far.
 * @param {*} form - The form.
 * @returns {Executable} The evaluator's node of it.
 */
function seedExpand(loaded, form) {
    const expander = loaded.get('scheme-js.expander') ?? pinnedSeed().loaded.get('scheme-js.expander');
    return assemble(callSchemeProcedure(expander.get('expand'), [form]), analyze);
}

/** The pinned seed, once made. @type {Object|null} */
let pinned = null;

/**
 * The pinned seed, made the first time it is needed.
 * @returns {{loaded: Map<string, Map<string, *>>}}
 * @throws {SchemeLibraryError} With none, as in a bundle whose tables are not
 *   current.
 */
function pinnedSeed() {
    if (pinned !== null) return pinned;
    if (pinnedSeedImage === null) {
        throw new SchemeLibraryError("a prebuilt table of the library system's own libraries is not "
            + 'current, and there is no reader or expander to load their source with: rebuild them '
            + '(npm run prebuild)');
    }
    pinned = loadPinnedSeed(pinnedSeedImage());
    return pinned;
}

/**
 * The libraries the pinned seed's core forms make, run interpreted, on an
 * interpreter of their own: a reader and an expander that need no text read
 * and no form expanded.
 * @param {Object<string, {declaration: string, items: Array<string>}>} image -
 *   Each library's `define-library` form and its top-level forms' core forms,
 *   in order, as JSON (`decodeDatum`), by key.
 * @returns {{loaded: Map<string, Map<string, *>>}} Their exports, by key.
 */
export function loadPinnedSeed(image) {
    const interpreter = new Interpreter(globalContext);
    const globalEnv = createGlobalEnvironment(interpreter);
    interpreter.setGlobalEnv(globalEnv);
    const seed = {
        interpreter, globalEnv,
        loaded: new Map([['scheme.primitives', createPrimitiveExports(globalEnv)]]),
        environments: new Map()
    };
    const libraryEnvironment = environmentIn(seed);
    for (const name of PINNED_LIBRARIES) {
        const { declaration, items } = image[name.join('.')];
        seed.loaded.set(name.join('.'), loadDeclared(name, decodeDatum(declaration), seed, {
            restoring: () => ({
                items: list(...items.map((core) => list(FORM, assemble(decodeDatum(core), analyze, libraryEnvironment))))
            }),
            formsOf: () => { throw new Error('the pinned seed reads no file'); },
            expand: () => { throw new Error('the pinned seed expands no form'); },
            install: () => {}
        }));
    }
    return { loaded: seed.loaded };
}

/**
 * What finds the environment of a library a table or the pinned image names
 * among a seed's own libraries, which are in no registry: a library's macro
 * refers to the library's own bindings, and to restore what it expanded into
 * is to refer to them where the seed loaded them.
 * @param {{environments: Map<string, Environment>}} seed - The seed.
 * @returns {function(Array<string>): Environment}
 */
const environmentIn = (seed) => (name) => {
    const env = seed.environments.get(name.join('.'));
    if (env === undefined) throw new Error(`the library system's seed has no library ${name.join('/')}`);
    return env;
};

/** The kind of item in a restore sequence that is a form to run. */
const FORM = intern('form');

/**
 * Loads one of the seed's libraries: from its prebuilt table, which has its
 * `define-library` form, when that is current, and otherwise from its source.
 * @param {string[]} name - The library's name.
 * @param {{interpreter: Interpreter, globalEnv: Environment, loaded: Map<string, Map<string, *>>,
 *   environments: Map<string, Environment>, tables: Object}} seed - The seed:
 *   its interpreter and global environment, the exports and environments of
 *   the libraries it has loaded so far, by key, and the prebuilt tables to
 *   install.
 * @returns {Map<string, *>} The library's exports.
 */
function seedLibrary(name, seed) {
    const { loaded, tables } = seed;
    const source = BUNDLED_SOURCES[`${name[name.length - 1]}.sld`];
    const table = tables[name.join('.')];
    const form = isCurrent(table) ? decodeDatum(table.declaration) : seedRead(loaded, source, name.join('/'))[0];
    const libraryEnvironment = environmentIn(seed);
    return loadDeclared(name, form, seed, {
        restoring: () => {
            const restorer = libraryRestorer(tables, libraryEnvironment);
            const files = restorer(name, null);
            return files === null ? null : restorer(name, files.map((file) => BUNDLED_SOURCES[file] ?? null));
        },
        formsOf: (file) => seedRead(loaded, BUNDLED_SOURCES[file], file),
        expand: (expr) => seedExpand(loaded, expr),
        install: (env) => installLibraryProcedures(tables, name, env, (file) => BUNDLED_SOURCES[file], libraryEnvironment)
    });
}

/**
 * Loads a library from its `define-library` form, recording its environment
 * among the seed's.
 * @param {string[]} name - The library's name.
 * @param {*} form - Its `define-library` form.
 * @param {{interpreter: Interpreter, globalEnv: Environment, loaded: Map<string, Map<string, *>>,
 *   environments: Map<string, Environment>}} seed - The seed loading it, as
 *   for `seedLibrary`.
 * @param {{restoring: function(Array<string>): (Object|null),
 *   formsOf: function(string): Array<*>, expand: function(*): Executable,
 *   install: function(Environment): void}} how -
 *   What restores it from a table, given the files it includes, or null if
 *   none can; the forms of a file it includes; what expands one of them into
 *   the evaluator's node; and what installs its table's procedures once it
 *   has loaded from source.
 * @returns {Map<string, *>} The library's exports.
 */
function loadDeclared(name, form, seed, how) {
    const { interpreter, globalEnv, loaded } = seed;
    const env = new Environment(globalEnv);
    env.libraryName = name;
    seed.environments.set(name.join('.'), env);
    const scope = globalContext.freshScope();
    globalContext.registerLibraryScope(scope, env);
    env.libraryScope = scope;

    const begins = [];
    const includes = [];
    const exports = [];
    for (const declaration of toArray(form).slice(2)) {
        const [keyword, ...parts] = toArray(declaration);
        switch (keyword.name) {
            case 'import':
                for (const library of parts) {
                    for (const [imported, value] of loaded.get(toArray(library).map(p => p.name).join('.'))) {
                        if (value instanceof SeedKeyword) globalContext.defineKeyword(scope, imported, value.keyword, value.transformer);
                        else env.define(imported, value);
                    }
                }
                break;
            case 'include':
                includes.push(...parts);
                break;
            case 'begin':
                begins.push(...parts);
                break;
            case 'export':
                exports.push(...parts);
                break;
            default:
                throw new Error(`the library system's seed cannot load (${keyword.name} ...), in ${name.join('/')}`);
        }
    }

    // A form a table restores is a node already; one read from a file is
    // expanded, with the library's scope the one it is expanded and run in.
    const evaluate = (expr) => {
        globalContext.pushDefiningScope(scope);
        try {
            interpreter.run(expr instanceof Executable ? expr : how.expand(expr), env);
        } finally {
            globalContext.popDefiningScope();
        }
    };
    // As the library system orders a library's forms: its `begin` forms, then
    // its included files'.
    const restoring = how.restoring(includes);
    if (restoring !== null) {
        for (const item of toArray(restoring.items)) {
            const [kind, part] = toArray(item);
            if (kind.name === 'procedure') restoring.bind(env, part.name);
            else evaluate(part);
        }
    } else {
        begins.forEach(evaluate);
        for (const file of includes) how.formsOf(file).forEach(evaluate);
    }
    how.install(env);

    return new Map(exports.map((spec) => {
        const [internal, external] = spec instanceof Symbol ? [spec.name, spec.name] : toArray(spec).slice(1).map(s => s.name);
        return [external, seedExport(env, scope, internal)];
    }));
}

/**
 * The value one of the seed's libraries exports for a name, found as the
 * library system finds it (`export-value` in library_system.scm).
 * @param {Environment} env - The library's environment.
 * @param {number} scope - Its scope.
 * @param {string} internal - The name, as the library binds it.
 * @returns {*}
 */
function seedExport(env, scope, internal) {
    const bound = globalContext.keywordBinding(scope, internal);
    const keyword = bound?.keyword ?? internal;
    if (env.findEnv(internal) !== null) return env.lookup(internal);
    if (bound?.transformer) return new SeedKeyword(keyword, bound.transformer);
    if (bound === undefined && globalMacroRegistry.isMacro(keyword)) {
        return new SeedKeyword(keyword, globalMacroRegistry.lookup(keyword));
    }
    if (SYNTAX_KEYWORDS.has(keyword)) return new SeedKeyword(keyword, null);
    return env.lookup(internal);
}
