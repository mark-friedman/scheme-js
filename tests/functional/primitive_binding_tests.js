/**
 * @fileoverview Compiled code trusts an inlined primitive's name for as long
 * as nothing has rebound it.
 *
 * An inlined `car` used to ask the environment, on every use, whether `car`
 * still denoted the primitive. Now every binding write reports whether it
 * rebinds a primitive's name (`src/core/interpreter/primitive_bindings.js`),
 * and compiled code reads that one flag instead. These tests check both ends:
 * that writes are seen however they happen, and that compiled code obeys a
 * redefinition made before it was compiled, after, and while it is running.
 *
 * The cells are shared by every interpreter in the process, and other suites
 * redefine primitives on purpose. So nothing here assumes a real primitive's
 * cell is intact on entry: the module is tested on names of its own, and
 * compiled code is tested on what it computes, which must be right whether or
 * not the shortcut is available.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { Environment } from '../../src/core/interpreter/environment.js';
import {
  registerPrimitive, noteBinding, primitiveCell
} from '../../src/core/interpreter/primitive_bindings.js';
import { tryCompileDefinition } from '../../src/compiler/index.js';
import { inlineExpansionNames } from '../../src/compiler/lowering.js';
import { settle } from '../../src/compiler/runtime.js';

/** @type {number} Makes each name from `freshName` distinct. */
let counter = 0;

/**
 * A name no real primitive has, unique to one test.
 * @param {string} label - What the test is about.
 * @returns {string} The name.
 */
function freshName(label) {
  counter += 1;
  return `%test-${label}-${counter}`;
}

/**
 * Runs Scheme source in an environment and returns the printed last value.
 * @param {Object} interpreter - The interpreter.
 * @param {Object} env - The environment.
 * @param {string} source - Scheme source.
 * @returns {string} The last value, printed.
 */
function run(interpreter, env, source) {
  let value;
  for (const form of parse(source)) {
    value = settle(interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' }));
  }
  return String(value);
}

/**
 * Compiles one definition and installs it.
 * @param {string} source - One `define` form.
 * @param {Object} env - The environment.
 * @returns {string} The generated JavaScript.
 */
function compile(source, env) {
  const result = tryCompileDefinition(analyze(parse(source)[0]), env);
  if (!result.compiled) throw new Error(`did not compile: ${result.reason}`);
  env.define(result.name, result.procedure);
  return result.source;
}

/**
 * Runs the primitive-binding tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runPrimitiveBindingTests(logger) {
  logger.title('Primitive Bindings - Whether a Primitive Name Was Ever Rebound');

  // --- The cells themselves --------------------------------------------------

  {
    const name = freshName('install');
    const primitive = () => 1;
    registerPrimitive(name, primitive);
    assert(logger, 'a newly installed primitive is intact', primitiveCell(name).intact, true);
    assert(logger, 'and its cell holds the primitive', primitiveCell(name).primitive === primitive, true);

    // Every interpreter installs the same module-level functions again.
    registerPrimitive(name, primitive);
    assert(logger, 'installing the same primitive again keeps it intact',
      primitiveCell(name).intact, true);

    noteBinding(name, primitive);
    assert(logger, 'binding the name to the primitive itself keeps it intact',
      primitiveCell(name).intact, true);

    noteBinding(name, 42);
    assert(logger, 'binding the name to anything else does not', primitiveCell(name).intact, false);
    noteBinding(name, primitive);
    assert(logger, 'and it never becomes intact again', primitiveCell(name).intact, false);
  }
  {
    // A primitive built per interpreter has no single function to compare
    // bindings against.
    const name = freshName('per-interpreter');
    registerPrimitive(name, () => 1);
    registerPrimitive(name, () => 2);
    assert(logger, 'a name installed with two different primitives is not intact',
      primitiveCell(name).intact, false);
  }
  assert(logger, 'a name no primitive was installed under is never intact',
    primitiveCell(freshName('none')).intact, false);
  assert(logger, 'and has no primitive', primitiveCell(freshName('none')).primitive, null);

  // --- Every write reaches them ----------------------------------------------

  {
    const name = freshName('define');
    const primitive = () => 1;
    registerPrimitive(name, primitive);
    const env = new Environment(null);
    env.define(name, primitive);
    assert(logger, 'defining the name as the primitive is not a rebinding',
      primitiveCell(name).intact, true);
    // A library's environment is a child of the global one, so a definition
    // there shadows the primitive rather than replacing it -- and is still a
    // rebinding, for any code that resolves its globals there.
    new Environment(env).define(name, 'shadow');
    assert(logger, 'defining it as something else in any frame is seen', primitiveCell(name).intact, false);
  }
  {
    const name = freshName('set');
    const primitive = () => 1;
    registerPrimitive(name, primitive);
    const env = new Environment(null);
    env.define(name, primitive);
    env.set(name, 'assigned');
    assert(logger, 'assigning the name is seen', primitiveCell(name).intact, false);
  }
  {
    // Starting an interpreter binds every primitive's name. None of those
    // bindings may count as a rebinding, or the shortcut would never be
    // available at all.
    const before = inlineExpansionNames().filter((name) => primitiveCell(name).intact);
    createInterpreter();
    const after = before.filter((name) => primitiveCell(name).intact);
    assert(logger, 'starting an interpreter rebinds no inlined primitive', after, before);
  }

  // --- Compiled code obeys them ------------------------------------------------

  logger.title('Primitive Bindings - Compiled Code Obeys a Redefinition');

  {
    const { env } = createInterpreter();
    const source = compile('(define (head l) (car l))', env);
    assert(logger, 'an inlined primitive reads its cell', /W\d+\.intact \|\|/.test(source), true);
    // The check that a redefinition captured a continuation belongs to the
    // slow path; the fast path has no statement for it.
    assert(logger, 'and makes no check of its own for a capture', source.includes('$UNWIND) R.capture'), false);
  }
  {
    // A program that redefined `car` before this was compiled must not have
    // its `car` compiled as the primitive's. Guarding on whatever the name was
    // bound to at compile time used to do exactly that.
    const { interpreter, env } = createInterpreter();
    run(interpreter, env, "(define (car x) 'mine)");
    compile('(define (head l) (car l))', env);
    assert(logger, 'a primitive redefined before compiling is not inlined',
      run(interpreter, env, "(head '(1 2))"), 'mine');
  }
  // Each of the next tests redefines a different primitive. The cells are
  // shared by the whole process and never become intact again, so a test that
  // reused a name an earlier one had rebound would find the shortcut already
  // off, and could not tell whether its own write had turned it off.
  {
    const { interpreter, env } = createInterpreter();
    compile("(define (empty? l) (null? l))", env);
    assert(logger, 'an inlined primitive before any redefinition', run(interpreter, env, "(empty? '())"), 'true');
    run(interpreter, env, "(set! null? (lambda (x) 'assigned))");
    assert(logger, 'a primitive assigned after compiling is obeyed',
      run(interpreter, env, "(empty? '())"), 'assigned');
  }
  {
    // The redefinition happens part-way through a compiled loop, inside a
    // procedure the loop calls, so the loop's own earlier uses of `eq?` have
    // already run the fast path.
    const { interpreter, env } = createInterpreter();
    compile("(define (walk l f) (if (null? l) '() (cons (f (eq? (car l) 'x)) (walk (cdr l) f))))", env);
    run(interpreter, env, "(define (keep v) (define (eq? a b) 'local) v)");
    assert(logger, 'a definition inside a nested procedure is local and changes nothing',
      run(interpreter, env, "(walk '(x y x) keep)"), '(true false true)');
    run(interpreter, env,
      "(define done #f) (define (swap v) (if (not done) (begin (set! done #t) (set! eq? (lambda (a b) 'mine)))) v)");
    assert(logger, 'a primitive redefined during a compiled loop is obeyed from then on',
      run(interpreter, env, "(walk '(x y x) swap)"), '(true mine mine)');
  }
  {
    // A redefinition that captures a continuation. An inline expansion is not
    // a point the procedure's resumable form can resume at, so the capture
    // cannot be completed; it must be refused, not answered with the sentinel
    // standing in for a value.
    const { interpreter, env } = createInterpreter();
    compile('(define (twice x) (* 2 (* x 1)))', env);
    run(interpreter, env, '(define saved #f) (set! * (lambda (a b) (call/cc (lambda (k) (set! saved k) 7))))');
    let message = '';
    try {
      run(interpreter, env, '(twice 3)');
    } catch (e) {
      message = e.message ?? String(e);
    }
    assert(logger, 'a capture beneath a redefined primitive is refused',
      /redefined primitive/.test(message), true);
  }
  {
    // Rebinding a primitive's name in one environment turns the shortcut off
    // everywhere, which must cost speed only: code in another environment
    // still gets the primitive.
    const { interpreter, env } = createInterpreter();
    compile('(define (second l) (car (cdr l)))', env);
    const other = createInterpreter();
    run(other.interpreter, other.env, "(define (cdr x) 'elsewhere)");
    assert(logger, 'a rebinding in another environment does not change this one',
      run(interpreter, env, "(second '(1 2 3))"), '2');
  }
}
