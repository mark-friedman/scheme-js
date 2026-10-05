/**
 * @fileoverview The evaluator's door: core forms into its nodes.
 *
 * The expander, `(scheme-js expander)` (src/core/scheme/expander.scm), turns
 * a form into a core form, a tagged list:
 *
 *     (lit datum)                    (var name)
 *     (library-var name env)         (scoped-var name scopes)
 *     (if test then else)            (seq forms)
 *     (lambda params rest name body original-params original-rest)
 *     (letrec names inits body original-names)
 *     (set name value)               (library-set name env value)
 *     (define name value)            (app operator operands)
 *     (import import-sets)           (define-library form)
 *     (define-syntax name definition)
 *     (node executable)
 *
 * and this builds the node the evaluator steps through for each, which keeps
 * the core form it was made of, as `core`, for the compiler, which lowers core
 * forms. The expander's header says what each form means; here they are only
 * read.
 * Names are symbols, read here as the strings the evaluator binds; a lambda's
 * own name is a string; a span, where a form has one, is its `source`
 * property, as on the data the reader makes.
 */

import {
  LiteralNode, VariableNode, ScopedVariable, LibraryVariableNode, LibrarySetNode, LambdaNode,
  LetRecNode, IfNode, SetNode, DefineNode, TailAppNode, BeginNode, ImportNode, DefineLibraryNode,
  DefineSyntaxNode
} from './ast_nodes.js';
import { globalScopeRegistry } from './syntax_object.js';
import { importLibraries, defineLibrary } from './library_loader.js';
import { getLibraryEnv } from './library_registry.js';
import { Executable, CTL } from './stepables_base.js';
import { stringValue } from '../primitives/string_class.js';

/**
 * A core form's fields, after its tag.
 * @param {Cons} form - The form.
 * @returns {Array<*>}
 */
function fields(form) {
  const out = [];
  for (let rest = form.cdr; rest !== null; rest = rest.cdr) out.push(rest.car);
  return out;
}

/**
 * A list's elements.
 * @param {Cons|null} items - A proper list.
 * @returns {Array<*>}
 */
function elements(items) {
  const out = [];
  for (let rest = items; rest !== null; rest = rest.cdr) out.push(rest.car);
  return out;
}

/** A name, a symbol, as the string the evaluator binds. */
const nameOf = (symbol) => symbol.name;

/** A name that may be #f, as a string or null. */
const optionalName = (symbol) => (symbol === false ? null : symbol.name);

/**
 * A library's environment, as a core form holds it: itself, or, restored from
 * a prebuilt table, `{library: name}`, the environment of the library of that
 * name -- in the registry libraries are loaded into now, or as whoever is
 * restoring the form finds it (`assemble`).
 * @param {Object} env - The environment, or the name of its library.
 * @param {function(Array<string>): Environment} libraryEnvironment - What
 *   finds a library's environment by its name.
 * @returns {Environment}
 */
const environmentOf = (env, libraryEnvironment) => (env.library !== undefined ? libraryEnvironment(env.library) : env);

/**
 * The node a core form denotes.
 * @param {Cons} form - The core form.
 * @param {Function} analyze - What an `import` or `define-library` it holds
 *   analyzes the libraries it loads with.
 * @param {function(Array<string>): Environment} [libraryEnvironment] - What
 *   finds the environment of a library a form restored from a table names:
 *   the current registry's by default; the library system's seed, whose
 *   libraries are in no registry, finds its own.
 * @returns {Executable}
 */
export function assemble(form, analyze, libraryEnvironment = getLibraryEnv) {
  const node = build(form, analyze, libraryEnvironment);
  if (form.source !== undefined && form.source !== null) node.source = form.source;
  node.core = form;
  return node;
}

/**
 * A core form a library's prebuilt table restores, made into its node when
 * it runs rather than when the table is read: by then the libraries the
 * library imports are loaded, and a library the form names
 * (`environmentOf`) is found.
 */
export class RestoredForm extends Executable {
  /**
   * @param {*} form - The core form.
   * @param {Function} analyze - As for `assemble`.
   * @param {function(Array<string>): Environment} [libraryEnvironment] - As
   *   for `assemble`.
   */
  constructor(form, analyze, libraryEnvironment = getLibraryEnv) {
    super();
    this.form = form;
    this.analyze = analyze;
    this.libraryEnvironment = libraryEnvironment;
  }

  step(registers) {
    registers[CTL] = assemble(this.form, this.analyze, this.libraryEnvironment);
    return true;
  }
}

/**
 * The node a core form denotes, without its span.
 * @param {Cons} form - The core form.
 * @param {Function} analyze - As for `assemble`.
 * @param {function(Array<string>): Environment} libraryEnvironment - As for
 *   `assemble`.
 * @returns {Executable}
 */
function build(form, analyze, libraryEnvironment) {
  const sub = (f) => assemble(f, analyze, libraryEnvironment);
  const parts = fields(form);
  switch (form.car.name) {
    case 'lit':
      return new LiteralNode(parts[0]);
    case 'var':
      return new VariableNode(nameOf(parts[0]));
    case 'library-var':
      return new LibraryVariableNode(nameOf(parts[0]), environmentOf(parts[1], libraryEnvironment));
    case 'scoped-var':
      return new ScopedVariable(nameOf(parts[0]), new Set(elements(parts[1])), globalScopeRegistry);
    case 'if':
      return new IfNode(sub(parts[0]), sub(parts[1]), sub(parts[2]));
    case 'seq':
      return new BeginNode(elements(parts[0]).map(sub));
    case 'lambda': {
      const [params, rest, name, body, originalParams, originalRest] = parts;
      return new LambdaNode(elements(params).map(nameOf), sub(body), optionalName(rest), stringValue(name),
        elements(originalParams).map(nameOf), optionalName(originalRest));
    }
    case 'letrec': {
      const [names, inits, body, originalNames] = parts;
      return new LetRecNode(elements(names).map(nameOf), elements(inits).map(sub), sub(body),
        elements(originalNames).map(nameOf));
    }
    case 'set':
      return new SetNode(nameOf(parts[0]), sub(parts[1]));
    case 'library-set':
      return new LibrarySetNode(nameOf(parts[0]), environmentOf(parts[1], libraryEnvironment), sub(parts[2]));
    case 'define':
      return new DefineNode(nameOf(parts[0]), sub(parts[1]));
    case 'app':
      return new TailAppNode(sub(parts[0]), elements(parts[1]).map(sub));
    case 'import':
      return new ImportNode(elements(parts[0]), importLibraries, analyze);
    case 'define-library':
      return new DefineLibraryNode(parts[0], defineLibrary, analyze);
    case 'define-syntax':
      return new DefineSyntaxNode(nameOf(parts[0]), parts[1]);
    case 'node':
      return parts[0];
    default:
      throw new Error(`not a core form: ${form.car.name}`);
  }
}
