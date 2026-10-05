/**
 * @fileoverview The library system's seed: what loads the library system,
 * which loads every other library.
 *
 * The library system is Scheme (src/core/scheme/library_system.scm), and is
 * itself a library, written with `(scheme core)` and `(scheme control)`, which
 * are libraries too. Something has to load those three before there is a
 * library system to do it, and this is that: a loader for exactly the
 * declarations they use -- `import` of libraries it has loaded already or of
 * the built-in `(scheme primitives)`, `include`, `begin` and `export` -- and no
 * more. Their files are the bundled sources (src/packaging/bundled_libraries.js),
 * which every host has at once, whatever its resolver.
 *
 * They are loaded apart from every program, on an interpreter of their own,
 * and registered nowhere: the library system is a tool that runs Scheme on a
 * program's behalf, as the compiler is (src/compiler/lowering.js), and a
 * program that defined a procedure the library system uses, at its own top
 * level, must not change how its libraries load. A program's `(scheme core)`
 * is loaded for it by the library system, like any other library.
 *
 * Each library is restored from its prebuilt table where the table was built
 * from the bundled sources as they are: its procedures bound from their
 * compiled code and its other top-level forms run, in order, so that its
 * files are read only to be fingerprinted, never parsed or run
 * (`libraryRestorer` in src/compiler/prebuilt.js). Interpreted, the library
 * system's loops -- binding each export a library imports, finding each one it
 * exports -- took a start from 159 ms to 246 on the CLI. A table that does not
 * match, as after editing a file without rebuilding, leaves its library to
 * load from source, its procedures installed from what the table has once it
 * has run; nothing else holds them, so nothing else is changed.
 */

import { parse } from './reader.js';
import { Environment } from './environment.js';
import { Interpreter } from './interpreter.js';
import { analyze } from './analyzer.js';
import { globalContext } from './context.js';
import { globalMacroRegistry } from './macro_registry.js';
import { toArray } from './cons.js';
import { Symbol } from './symbol.js';
import { createGlobalEnvironment } from '../primitives/index.js';
import { BUNDLED_SOURCES } from '../../packaging/bundled_libraries.js';
import { createPrimitiveExports } from './library_loader.js';
import { SYNTAX_KEYWORDS } from './library_registry.js';
import { installLibraryProcedures, libraryRestorer } from '../../compiler/prebuilt.js';
import prebuiltLibraries from '../../packaging/compiled_libraries.js';

/** The libraries the seed loads, in the order they need each other. */
const SEED_LIBRARIES = [['scheme', 'core'], ['scheme', 'control'], ['scheme-js', 'library-system']];

/**
 * A syntactic keyword one of the seed's libraries exports, as the analyzer
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
 * @type {{interpreter: Interpreter, globalEnv: Environment, loaded: Map<string, Map<string, *>>, tables: Object}|null}
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
 * @returns {{interpreter: Interpreter, globalEnv: Environment, loaded: Map<string, Map<string, *>>, tables: Object}}
 *   It, its global environment, the exports of the libraries loaded, by key,
 *   and the tables.
 */
function plant(tables) {
    const interpreter = new Interpreter(globalContext);
    const globalEnv = createGlobalEnvironment(interpreter);
    interpreter.setGlobalEnv(globalEnv);
    const loaded = new Map([['scheme.primitives', createPrimitiveExports(globalEnv)]]);
    for (const name of SEED_LIBRARIES) {
        loaded.set(name.join('.'), seedLibrary(name, loaded, interpreter, globalEnv, tables));
    }
    return { interpreter, globalEnv, loaded, tables };
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
    if (!shipped.loaded.has(key)) {
        shipped.loaded.set(key, seedLibrary(name, shipped.loaded, shipped.interpreter, shipped.globalEnv, shipped.tables));
    }
    return shipped.loaded.get(key);
}

/**
 * Loads one of the seed's libraries.
 * @param {string[]} name - The library's name.
 * @param {Map<string, Map<string, *>>} loaded - The exports of the libraries
 *   loaded so far, by key.
 * @param {Interpreter} interpreter - The seed's interpreter.
 * @param {Environment} globalEnv - Its global environment.
 * @param {Object<string, Object>} tables - The prebuilt tables to install.
 * @returns {Map<string, *>} The library's exports.
 */
function seedLibrary(name, loaded, interpreter, globalEnv, tables) {
    const source = BUNDLED_SOURCES[`${name[name.length - 1]}.sld`];
    const [form] = parse(source, { filename: name.join('/'), dotAccess: false });
    const env = new Environment(globalEnv);
    env.libraryName = name;
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

    const evaluate = (expr) => {
        globalContext.pushDefiningScope(scope);
        try {
            interpreter.run(analyze(expr), env);
        } finally {
            globalContext.popDefiningScope();
        }
    };
    // As the library system orders a library's forms: its `begin` forms, then
    // its included files'.
    const restoring = libraryRestorer(tables)(name, [source, ...includes.map((file) => BUNDLED_SOURCES[file])]);
    if (restoring !== null) {
        for (const item of toArray(restoring.items)) {
            const [kind, part] = toArray(item);
            if (kind.name === 'procedure') restoring.bind(env, part.name);
            else evaluate(part);
        }
    } else {
        begins.forEach(evaluate);
        for (const file of includes) parse(BUNDLED_SOURCES[file], { filename: file, dotAccess: false }).forEach(evaluate);
    }
    installLibraryProcedures(tables, name, env, (file) => BUNDLED_SOURCES[file]);

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
