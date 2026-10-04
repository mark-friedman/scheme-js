/**
 * @fileoverview What the library system's Scheme needs of the host.
 *
 * The library system is Scheme (src/core/scheme/library_system.scm). What it
 * cannot do in Scheme is here: calling the host's file resolver and load hook,
 * reading a file's text into forms (the reader), making a library's
 * environment and binding names in it (`Environment`), the analyzer's
 * tables of library scopes, syntactic keywords and macros (context.js,
 * macro_registry.js), which the analyzer, still JavaScript, reads as it
 * expands, and running a closure compiled or as itself (the value
 * representation).
 *
 * Names arrive as symbols and leave as the strings the JavaScript keys its
 * tables by. A resolver's path and a library's name go to the host as arrays
 * of strings, as the resolver and the load hook have always been given them.
 */

import { Environment } from '../interpreter/environment.js';
import { globalContext } from '../interpreter/context.js';
import { GLOBAL_SCOPE_ID } from '../interpreter/syntax_object.js';
import { globalMacroRegistry } from '../interpreter/macro_registry.js';
import { parse } from '../interpreter/reader.js';
import { cons, list, toArray } from '../interpreter/cons.js';
import { intern } from '../interpreter/symbol.js';
import { SYNTAX_KEYWORDS } from '../interpreter/library_registry.js';
import { shadowMacro } from '../interpreter/syntax_object.js';
import { stringValue } from './string_class.js';
import { runCompiled, runInterpreted } from '../interpreter/values.js';

/**
 * Calls a file resolver for a file it can return now.
 *
 * A resolver may answer with a promise, when it has to fetch the file; a
 * synchronous load cannot wait for one, so it is the same as no answer. The
 * promise is left to settle on its own, its failure no error: nothing waits
 * for it.
 *
 * @param {Function} resolver - The host's resolver, from an array of strings
 *   to a file's text or a promise of it.
 * @param {string[]} path - The path.
 * @returns {*} The resolver's answer, or false for a promise.
 */
export function resolveNow(resolver, path) {
    const source = resolver(path);
    if (source !== null && typeof source === 'object' && typeof source.then === 'function') {
        source.then(() => {}, () => {});
        return false;
    }
    return source;
}

/**
 * Strings in a Scheme list, as an array of JavaScript strings.
 * @param {Cons|null} strings - The list.
 * @returns {string[]}
 */
function stringsOf(strings) {
    return toArray(strings).map(stringValue);
}

/**
 * A new environment for a library, or for what `environment` makes, inside
 * the one given: with a fresh scope of its own, which the keywords imported
 * into it are bound under and the definitions made in it are noted in.
 * @param {Environment} base - The environment it is inside.
 * @returns {Environment}
 */
export function makeScopedEnvironment(base) {
    const env = new Environment(base);
    const scope = globalContext.freshScope();
    globalContext.registerLibraryScope(scope, env);
    env.libraryScope = scope;
    return env;
}

export const libraryPrimitives = {
    /**
     * The file a path names, from the host's resolver, or #f if the resolver
     * can answer only later (`resolveNow`).
     */
    '%resolve': (resolver, path) => resolveNow(resolver, stringsOf(path)),

    /**
     * Calls the host's load hook with a library just loaded: its name, as
     * strings, and its environment.
     */
    '%call-load-hook': (hook, name, env) => {
        hook(stringsOf(name), env);
        return undefined;
    },

    /**
     * The forms a file's text holds, read, each carrying its location in
     * the file named, if one is; folding case as `#!fold-case` does if asked.
     */
    '%read-forms': (source, filename, foldCase) => {
        const options = { caseFold: foldCase === true };
        if (filename !== false) options.filename = stringValue(filename);
        return list(...parse(stringValue(source), options));
    },

    /**
     * A new library's environment, inside the one given: named for the
     * compiler tier, which treats a library's top level as it does a
     * program's, and with a fresh scope of its own, which its imports record
     * the keywords they rename under and its body's definitions are made in.
     */
    '%make-library-environment': (base, name) => {
        const env = makeScopedEnvironment(base);
        env.libraryName = stringsOf(name);
        return env;
    },

    /**
     * The scope an environment's syntactic keywords are bound under: its
     * library's, or a program's top level's.
     */
    '%environment-scope': (env) => env.libraryScope ?? GLOBAL_SCOPE_ID,

    /**
     * Makes a closure run as a compiled procedure, staying the object every
     * holder of it has (`runCompiled` in values.js).
     */
    '%run-compiled!': (closure, procedure) => {
        runCompiled(closure, procedure);
        return undefined;
    },

    /** Makes a closure run compiled run as itself again (`runInterpreted`). */
    '%run-interpreted!': (closure) => {
        runInterpreted(closure);
        return undefined;
    },

    /**
     * A compiled procedure's resumable form, which the frames of its calls
     * saved and resumed carry; #f for any other value.
     */
    '%resumable-form': (procedure) =>
        typeof procedure === 'function' && procedure.$resume !== undefined ? procedure.$resume : false,

    /** Whether an environment, or one it is inside, binds a name. */
    '%environment-bound?': (env, name) => env.findEnv(name.name) !== null,

    /** The value of a name in an environment. */
    '%environment-ref': (env, name) => env.lookup(name.name),

    /**
     * Defines a name in an environment -- an import, as a rule -- which then
     * names the value, though a macro of the name was bound there
     * (`shadowMacro`).
     */
    '%environment-define!': (env, name, value) => {
        env.define(name.name, value);
        shadowMacro(env.libraryScope ?? GLOBAL_SCOPE_ID, name.name);
        return undefined;
    },

    /**
     * The syntactic keyword a name is bound to under a scope, as
     * `(keyword . transformer)`, the transformer #f for a special form or
     * auxiliary keyword; or #f if nothing binds the name there as a keyword --
     * a variable defined over a macro's name included.
     */
    '%keyword-binding': (scope, name) => {
        const bound = globalContext.keywordBinding(scope, name.name);
        return bound === undefined || bound.keyword === null
            ? false
            : cons(intern(bound.keyword), bound.transformer ?? false);
    },

    /** Binds a name under a scope to a syntactic keyword. */
    '%define-keyword!': (scope, name, keyword, transformer) => {
        globalContext.defineKeyword(scope, name.name, keyword.name, transformer === false ? null : transformer);
        return undefined;
    },

    /** The transformer of the macro a name is defined as by name, or #f. */
    '%global-macro': (name) =>
        globalMacroRegistry.isMacro(name.name) ? globalMacroRegistry.lookup(name.name) : false,

    /** Whether a name is a keyword the analyzer handles itself. */
    '%special-keyword?': (name) => SYNTAX_KEYWORDS.has(name.name)
};
