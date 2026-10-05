/**
 * @fileoverview What the expander, `(scheme-js expander)`
 * (src/core/scheme/expander.scm), needs of the host.
 *
 * Identifiers are a value representation, `SyntaxObject`s beside symbols,
 * interned by name and scopes so that one written twice is one object; what
 * reads and makes them is here. So are the expander's tables, which the
 * library system reaches too (src/core/primitives/library.js): the scopes
 * made so far and the library each belongs to, the library or program whose
 * forms are being expanded, the syntactic keywords bound in each -- held by
 * the library's environment, weakly, so that they go when it does -- and the
 * macros defined by name for the whole process. And the evaluator, which runs
 * a procedural macro's procedure as it is defined.
 *
 * Names arrive as symbols and leave as the strings the tables are keyed by.
 */

import { globalContext } from '../interpreter/context.js';
import {
  internSyntax, syntaxName, syntaxScopes, identifierEquals, libraryScopeOf,
  flipScopeInExpression, globalScopeRegistry
} from '../interpreter/syntax_object.js';
import { cons, list, toArray } from '../interpreter/cons.js';
import { intern } from '../interpreter/symbol.js';
import { Executable } from '../interpreter/stepables_base.js';
import { SchemeSyntaxError } from '../interpreter/errors.js';
import { evaluateFeatureRequirement } from '../interpreter/library_registry.js';
import { Interpreter } from '../interpreter/interpreter.js';
import { assemble } from '../interpreter/assembler.js';
import { analyze } from '../interpreter/expand.js';
import { createGlobalEnvironment } from './index.js';
import { Rational } from './rational.js';
import { Complex } from './complex.js';
import { Char } from './char_class.js';
import { stringValue } from './string_class.js';
import { displayString } from './io/printer.js';

/**
 * A scope as the tables key it: a scope is made a JavaScript number
 * (`freshScope`), and comes back from Scheme as one, or, written as a literal
 * there, as an exact integer.
 * @param {number|bigint} scope - The scope.
 * @returns {number}
 */
const scopeOf = (scope) => Number(scope);

/**
 * The expander's primitives.
 */
/** The interpreter procedural macros' procedures are made on, once made. */
let transformers = null;

/**
 * The interpreter procedural macros' procedures are made on, and run by: one
 * for the process, with no debug runtime, its global environment one where
 * only the primitives are bound, made the first time a procedure is. It does
 * not own the environments it evaluates in, a program's or a library's, so
 * that their code stays their interpreter's.
 * @returns {Interpreter}
 */
function transformerInterpreter() {
    if (transformers === null) {
        transformers = new Interpreter(globalContext);
        transformers.setGlobalEnv(createGlobalEnvironment(transformers));
    }
    return transformers;
}

export const expanderPrimitives = {
    // -- Identifiers ------------------------------------------------------------

    /** An identifier's name, as a symbol. */
    '%identifier-name': (id) => intern(syntaxName(id)),

    /** The scopes an identifier carries, as a list; none for a symbol. */
    '%identifier-scopes': (id) => list(...syntaxScopes(id)),

    /**
     * Whether two identifiers would bind the same where both were bound: the
     * same name and the same scopes, a symbol carrying none.
     */
    '%bound-identifier=?': (a, b) => identifierEquals(a, b),

    /** The identifier of a name and scopes, the one there is if one was made. */
    '%make-identifier': (name, scopes) => internSyntax(name.name, toArray(scopes).map(scopeOf)),

    /** An identifier with a scope added. */
    '%identifier-add-scope': (id, scope) => id.addScope(scopeOf(scope)),

    /**
     * An identifier with a scope flipped: added if it lacks it, else taken
     * away, leaving a symbol if it took the last.
     */
    '%identifier-flip-scope': (id, scope) => id.flipScope(scopeOf(scope)),

    /** The scope of the library whose macro introduced an identifier, or #f. */
    '%identifier-library-scope': (id) => libraryScopeOf(id) ?? false,

    /**
     * A datum with a scope flipped on every identifier in it, its pairs and
     * vectors copied as a graph where it may share structure.
     */
    '%flip-scope': (datum, scope) => flipScopeInExpression(datum, scopeOf(scope)),

    // -- Scopes, keywords and macros --------------------------------------------

    /** A scope no identifier carries yet. */
    '%fresh-scope': () => globalContext.freshScope(),

    /** The scope of the library or program whose forms are being expanded, or #f. */
    '%defining-scope': () => {
        const defining = globalContext.definingScopes;
        return defining.length > 0 ? defining[defining.length - 1] : false;
    },

    /** The environment of the library a scope is, or #f. */
    '%library-environment': (scope) => globalContext.lookupLibraryEnv(scopeOf(scope)) ?? false,

    /**
     * Enters a library's scope where it was taken away, with the registry it
     * was loaded in: by a macro that names the library and outlived it.
     */
    '%reregister-library!': (env) => {
        if (globalContext.lookupLibraryEnv(env.libraryScope) === undefined) {
            globalContext.registerLibraryScope(env.libraryScope, env);
        }
        return undefined;
    },

    /**
     * What a name is bound to under a scope, as `(keyword . transformer)`:
     * the keyword's own name and a macro's transformer, or #f for a special
     * form; the keyword #f for a variable defined over a macro's name. #f if
     * nothing is bound under the scope.
     */
    '%keyword-entry': (scope, name) => {
        const bound = globalContext.keywordBinding(scopeOf(scope), name.name);
        if (bound === undefined) return false;
        return cons(bound.keyword === null ? false : intern(bound.keyword), bound.transformer ?? false);
    },

    /** Binds a name under a scope: to a keyword, or with #f, to a variable. */
    '%bind-keyword!': (scope, name, keyword, transformer) => {
        globalContext.defineKeyword(scopeOf(scope), name.name, keyword === false ? null : keyword.name,
            transformer === false ? null : transformer);
        return undefined;
    },

    /** Unbinds a name under a scope. */
    '%forget-keyword!': (scope, name) => {
        globalContext.forgetKeyword(scopeOf(scope), name.name);
        return undefined;
    },

    /** The transformer of the macro defined under a name for the process, or #f. */
    '%process-macro': (name) => globalContext.macroRegistry.lookup(name.name) ?? false,

    /** Defines a macro under a name for the process. */
    '%define-process-macro!': (name, transformer) => {
        globalContext.macroRegistry.define(name.name, transformer);
        return undefined;
    },

    /**
     * Whether a definition made while a scope was being defined in is
     * registered for a name and scopes (`registerBindingWithCurrentScopes` in
     * frames.js).
     */
    '%scope-binding?': (name, scopes) =>
        globalScopeRegistry.resolve({ name: name.name, scopes: new Set(toArray(scopes).map(scopeOf)) }) !== null,

    /** A number no renamed variable has carried. */
    '%fresh-unique-id': () => BigInt(globalContext.freshUniqueId()),

    /**
     * Whether the features of the registry libraries are loaded into now meet
     * a requirement, as `cond-expand` writes it.
     */
    '%feature-requirement-met?': (requirement) => evaluateFeatureRequirement(requirement),

    // -- Environments -------------------------------------------------------------

    /**
     * Whether an environment holds what was imported into it and nothing
     * else.
     */
    '%environment-strict?': (env) => env.strict === true,

    /** Whether an environment binds a name itself. */
    '%environment-binds?': (env, name) => env.bindings.has(name.name),

    /** The environment that binds a name where an environment finds it, or #f. */
    '%environment-holder': (env, name) => env.findEnv(name.name) ?? false,

    /** What an environment binds a name to itself. */
    '%environment-own-value': (env, name) => env.bindings.get(name.name),

    /** The environment an environment is inside, or #f. */
    '%environment-parent': (env) => env.parent ?? false,

    /**
     * The names an environment's frames bind, as a list of frames, outermost
     * first, each a list of `(written . renamed)`, for an expression typed in
     * a paused frame to see its locals.
     */
    '%environment-renamings': (env) => {
        const frames = [];
        for (let scope = env; scope; scope = scope.parent) {
            frames.unshift(list(...[...(scope.nameMap ?? [])].map(([written, renamed]) =>
                cons(intern(written), intern(renamed)))));
        }
        return list(...frames);
    },

    // -- Data ---------------------------------------------------------------------

    /**
     * Whether a datum evaluates to itself as an expression, a vector aside: a
     * number, a string as a literal is, a boolean, a bytevector or a
     * character.
     */
    '%self-evaluating?': (x) => typeof x === 'number' || typeof x === 'bigint' || typeof x === 'string'
        || typeof x === 'boolean' || x instanceof Uint8Array || x instanceof Rational
        || x instanceof Complex || x instanceof Char,

    /**
     * Whether a datum is one that a pattern matches by being the same: not a
     * pair, a vector, an identifier, nor any other object.
     */
    '%atomic-datum?': (x) => x !== null && typeof x !== 'object',

    /** Whether a value is a node of the evaluator's. */
    '%node?': (x) => x instanceof Executable,

    // -- Errors -------------------------------------------------------------------

    /** A syntax error, about a form, in a keyword's use; #f for neither. */
    '%make-syntax-error': (message, form, keyword) => new SchemeSyntaxError(stringValue(message), form,
        keyword === false ? null : (keyword.name ?? stringValue(keyword))),

    /** Whether a value is a syntax error. */
    '%syntax-error?': (x) => x instanceof SchemeSyntaxError,

    /** What an error says, or else how a raised value displays. */
    '%error-message': (e) => (e instanceof Error ? e.message : displayString(e)),

    // -- Transformers ------------------------------------------------------------------

    /**
     * Gives a macro's transformer the procedure a debugger finds it by: a
     * procedural macro's -- `er-macro-transformer`'s or `define-macro`'s --
     * which runs the procedure the definition gave.
     */
    '%reflect-transformer!': (transformer, procedure) => {
        transformer.transformerProcedure = procedure;
        return undefined;
    },

    /**
     * A pending macro's definition and the scope of the library that defined
     * it, as `(form . scope)` (`DefineSyntaxNode` in ast_nodes.js); #f for
     * anything else.
     */
    '%pending-macro': (x) => (x !== null && typeof x === 'object' && x.pendingMacro !== undefined
        ? cons(x.pendingMacro, x.scope) : false),

    /** The transformer made of a pending macro's definition, or #f if none is yet. */
    '%realized-macro': (pending) => pending.realized ?? false,

    /**
     * Keeps on a pending macro the transformer made of its definition, and the
     * procedure a debugger finds a procedural macro by.
     */
    '%realize-pending-macro!': (pending, made) => {
        pending.realized = made;
        if (made.transformerProcedure !== undefined) pending.transformerProcedure = made.transformerProcedure;
        return undefined;
    },

    // -- Procedural macros -----------------------------------------------------------

    /**
     * The value of an expression, a core form, evaluated in an environment --
     * that of the library or program defining the macro whose procedure it
     * is (`defining-environment` in expander.scm) -- or, where none is known,
     * #f, in one where only the primitives are bound; on the interpreter
     * procedural macros' procedures are made on (`transformerInterpreter`): a
     * procedural macro's procedure, made as the macro is defined.
     *
     * That interpreter has no debug runtime, and the procedure it makes is run
     * by it. A procedure of the program's that the transformer calls is run
     * by the program's interpreter, as any call of it is. A transformer runs inside the
     * expander, which finishes before the interpreter runs a step of the code
     * being expanded, while the debugger can only make execution wait between
     * the steps of an asynchronous run: a breakpoint that fired inside a
     * transformer could stop nothing. The debugger reports such breakpoints as
     * never firing, finding the transformer by its `transformerProcedure`.
     */
    '%evaluate-transformer': (form, env) => {
        const interpreter = transformerInterpreter();
        return interpreter.run(assemble(form, analyze), env === false ? interpreter.globalEnv : env);
    }
};
