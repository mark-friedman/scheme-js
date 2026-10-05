/**
 * @fileoverview The assembler (src/core/interpreter/assembler.js): a core
 * form, as `(scheme-js expander)` makes it, into the evaluator's nodes.
 *
 * JavaScript tests, since what is tested is JavaScript building JavaScript
 * objects: the nodes' classes and fields, and what they evaluate to.
 */

import { assert } from '../../harness/helpers.js';
import { assemble } from '../../../src/core/interpreter/assembler.js';
import { parse } from '../../../src/core/interpreter/reader.js';
import { list } from '../../../src/core/interpreter/cons.js';
import { intern } from '../../../src/core/interpreter/symbol.js';
import { createInterpreter } from '../../../src/core/interpreter/index.js';
import {
  LiteralNode, VariableNode, IfNode, BeginNode, LambdaNode, LetRecNode, SetNode, DefineNode,
  TailAppNode, LibraryVariableNode, LibrarySetNode, ScopedVariable, ImportNode, DefineLibraryNode
} from '../../../src/core/interpreter/ast_nodes.js';

/**
 * A core form written as text.
 * @param {string} text - The text.
 * @returns {*} The form.
 */
const core = (text) => parse(text, { dotAccess: false })[0];

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 */
export function runAssemblerTests(logger) {
  logger.title('The assembler: core forms into nodes');

  const analyze = () => { throw new Error('nothing here analyzes'); };
  const { interpreter, env } = createInterpreter();
  const run = (text) => interpreter.run(assemble(core(text), analyze), env);

  const lit = assemble(core('(lit 5)'), analyze);
  assert(logger, 'a literal', [lit instanceof LiteralNode, lit.value], [true, 5n]);
  const variable = assemble(core('(var car)'), analyze);
  assert(logger, 'a variable, its name a string', [variable instanceof VariableNode, variable.name], [true, 'car']);
  assert(logger, 'if', assemble(core('(if (lit #t) (lit 1) (lit 2))'), analyze) instanceof IfNode, true);
  assert(logger, 'and it runs', run('(if (lit #f) (lit 1) (lit 2))'), 2n);
  const seq = assemble(core('(seq ((lit 1) (lit 2)))'), analyze);
  assert(logger, 'a sequence', [seq instanceof BeginNode, seq.expressions.length], [true, 2]);

  const fixed = assemble(core('(lambda (a_1 b_2) #f "f" (var a_1) (a b) #f)'), analyze);
  assert(logger, 'a lambda',
    [fixed instanceof LambdaNode, fixed.params, fixed.restParam, fixed.name, fixed.originalParams, fixed.originalRestParam],
    [true, ['a_1', 'b_2'], null, 'f', ['a', 'b'], null]);
  const rest = assemble(core('(lambda () r_2 "anonymous" (var r_2) () r)'), analyze);
  assert(logger, 'a lambda with a rest parameter',
    [rest.params, rest.restParam, rest.originalParams, rest.originalRestParam], [[], 'r_2', [], 'r']);
  assert(logger, 'a lambda applied', run('(app (lambda (x_1) #f "let" (var x_1) (x) #f) ((lit 7)))'), 7n);

  const letrec = assemble(core('(letrec (f_1) ((lambda () #f "anonymous" (lit 1) () #f)) (var f_1) (f))'), analyze);
  assert(logger, 'a letrec of lambdas',
    [letrec instanceof LetRecNode, letrec.names, letrec.lambdaExprs[0] instanceof LambdaNode, letrec.originalNames],
    [true, ['f_1'], true, ['f']]);
  assert(logger, 'and it runs', run('(app (letrec (f_1) ((lambda () #f "anonymous" (lit 1) () #f)) (var f_1) (f)) ())'), 1n);

  const set = assemble(core('(set x (lit 1))'), analyze);
  assert(logger, 'set!', [set instanceof SetNode, set.name], [true, 'x']);
  const define = assemble(core('(define x (lit 1))'), analyze);
  assert(logger, 'define', [define instanceof DefineNode, define.name], [true, 'x']);
  const app = assemble(core('(app (var +) ((lit 1) (lit 2)))'), analyze);
  assert(logger, 'an application', [app instanceof TailAppNode, app.argExprs.length], [true, 2]);
  assert(logger, 'and it runs', run('(app (var +) ((lit 1) (lit 2)))'), 3n);

  const libraryVar = assemble(list(intern('library-var'), intern('x'), env), analyze);
  assert(logger, "a reference to a library's binding",
    [libraryVar instanceof LibraryVariableNode, libraryVar.name, libraryVar.env === env], [true, 'x', true]);
  const librarySet = assemble(list(intern('library-set'), intern('x'), env, core('(lit 1)')), analyze);
  assert(logger, "an assignment to one",
    [librarySet instanceof LibrarySetNode, librarySet.name, librarySet.env === env], [true, 'x', true]);
  // Restored from a table, the library is named, and found by whoever
  // restores the form: the library system's seed finds its own libraries.
  const asked = [];
  const named = { library: ['a', 'b'] };
  const finding = (name) => { asked.push(name.join('.')); return env; };
  const restoredVar = assemble(list(intern('library-var'), intern('x'), named), analyze, finding);
  const restoredSet = assemble(list(intern('library-set'), intern('x'), named, core('(lit 1)')), analyze, finding);
  assert(logger, "a library named in a table, found as the restorer finds it",
    [restoredVar.env === env, restoredSet.env === env, asked], [true, true, ['a.b', 'a.b']]);
  const nested = assemble(list(intern('if'), core('(lit #t)'), list(intern('library-var'), intern('x'), named),
    core('(lit #f)')), analyze, finding);
  assert(logger, 'and so inside another form', nested.consequent.env === env, true);
  const scoped = assemble(core('(scoped-var x (3 4))'), analyze);
  assert(logger, 'a scoped variable', [scoped instanceof ScopedVariable, [...scoped.scopes]], [true, [3, 4]]);

  const importNode = assemble(core('(import ((scheme base) (only (scheme write) display)))'), analyze);
  assert(logger, 'import, its sets as written',
    [importNode instanceof ImportNode, importNode.importSpecs.length, importNode.analyze === analyze], [true, 2, true]);
  const form = core('(define-library (a b) (export c))');
  const library = assemble(list(intern('define-library'), form), analyze);
  assert(logger, 'define-library, its form',
    [library instanceof DefineLibraryNode, library.form === form], [true, true]);
  assert(logger, 'a node already made', assemble(list(intern('node'), lit), analyze), lit);

  const spanned = core('(app (var f) ())');
  spanned.source = { filename: 'f.scm', line: 1, column: 1, endLine: 1, endColumn: 9 };
  assert(logger, "a node's span, its form's", assemble(spanned, analyze).source, spanned.source);
}
