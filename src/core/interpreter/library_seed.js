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
 * Once a library's prebuilt table can be installed without running its source,
 * the library system's will be, and this goes.
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
 * Loads the library system.
 * @returns {Map<string, Function>} The procedures `(scheme-js library-system)`
 *   exports, by name.
 */
export function seedLibrarySystem() {
    const interpreter = new Interpreter(globalContext);
    const globalEnv = createGlobalEnvironment(interpreter);
    interpreter.setGlobalEnv(globalEnv);
    const loaded = new Map([['scheme.primitives', createPrimitiveExports(globalEnv)]]);
    for (const name of SEED_LIBRARIES) {
        loaded.set(name.join('.'), seedLibrary(name, loaded, interpreter, globalEnv));
    }
    return loaded.get('scheme-js.library-system');
}

/**
 * Loads one of the seed's libraries.
 * @param {string[]} name - The library's name.
 * @param {Map<string, Map<string, *>>} loaded - The exports of the libraries
 *   loaded so far, by key.
 * @param {Interpreter} interpreter - The seed's interpreter.
 * @param {Environment} globalEnv - Its global environment.
 * @returns {Map<string, *>} The library's exports.
 */
function seedLibrary(name, loaded, interpreter, globalEnv) {
    const [form] = parse(BUNDLED_SOURCES[`${name[name.length - 1]}.sld`], { filename: name.join('/') });
    const env = new Environment(globalEnv);
    env.libraryName = name;
    const scope = globalContext.freshScope();
    globalContext.registerLibraryScope(scope, env);
    env.libraryScope = scope;

    const body = [];
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
                for (const file of parts) body.push(...parse(BUNDLED_SOURCES[file], { filename: file }));
                break;
            case 'begin':
                body.push(...parts);
                break;
            case 'export':
                exports.push(...parts);
                break;
            default:
                throw new Error(`the library system's seed cannot load (${keyword.name} ...), in ${name.join('/')}`);
        }
    }

    for (const expr of body) {
        globalContext.pushDefiningScope(scope);
        try {
            interpreter.run(analyze(expr), env);
        } finally {
            globalContext.popDefiningScope();
        }
    }

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
