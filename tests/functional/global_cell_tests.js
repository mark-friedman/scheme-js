/**
 * @fileoverview Global value cells: how compiled code reads a global.
 *
 * Compiled code cannot take a global's value at compile time -- a definition
 * may be a forward reference, and any binding may be redefined or assigned
 * afterwards, by a later `define`, by `set!`, or by a REPL. It used to resolve
 * the frame holding the name once and then look the name up in that frame's
 * map on every reference: a hash lookup per global read, which the canonical
 * suite put at up to 1.5x of compiled run time. Now the frame hands out a cell
 * for the name, keeps it current on every write, and compiled code reads the
 * cell.
 *
 * These pin both halves: that a frame keeps its cells in step with its
 * bindings through every way a binding is written, and that compiled code
 * observes every change a Scheme program can make to a global.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { Environment } from '../../src/core/interpreter/environment.js';
import { registerLibrary, substituteLibraryValues } from '../../src/core/interpreter/library_registry.js';
import { compileProgram } from '../../src/compiler/index.js';
import { globalCell, settle } from '../../src/compiler/runtime.js';
import { interpretedLibrary, installStandardLibrary } from '../harness/standard_library.js';
import { writeString } from '../../src/core/primitives/io/printer.js';

/**
 * A fresh environment with the standard library, compiled.
 * @returns {{interpreter: Object, env: Object}} The pair.
 */
function freshCompiledLibrary() {
  const pair = interpretedLibrary();
  installStandardLibrary(pair.env);
  return pair;
}

/**
 * Evaluates Scheme source, compiling its definitions where they can be.
 * @param {Object} pair - The interpreter and environment.
 * @param {string} source - Scheme source.
 * @returns {{value: *, compiled: Array<string>}} The last form's value, and
 *   the definitions that compiled.
 */
function compileAndRun({ interpreter, env }, source) {
  const asts = parse(source).map((form) => analyze(form));
  const definitions = asts.filter((ast) => ast.constructor.name === 'DefineNode');
  const rest = asts.filter((ast) => ast.constructor.name !== 'DefineNode');
  const { compiled } = compileProgram(definitions, env, interpreter);
  let value;
  for (const ast of rest) {
    value = settle(interpreter.run(ast, env, [], undefined, { jsAutoConvert: 'raw' }));
  }
  return { value, compiled };
}

/**
 * Evaluates Scheme source with the interpreter only.
 * @param {Object} pair - The interpreter and environment.
 * @param {string} source - Scheme source.
 * @returns {*} The last form's value.
 */
function run({ interpreter, env }, source) {
  let value;
  for (const form of parse(source)) {
    value = settle(interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' }));
  }
  return value;
}

/**
 * Runs the global-cell tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runGlobalCellTests(logger) {
  logger.title('Global cells - a frame keeps its cells current');
  {
    const global = new Environment(null);
    global.define('x', 1);
    assert(logger, 'a frame has no cells until one is asked for', global.cells, null);

    const cell = global.cellFor('x');
    assert(logger, 'a cell holds the binding it was asked for', cell.v, 1);
    assert(logger, 'asking again gives the same cell', global.cellFor('x') === cell, true);

    global.define('x', 2);
    assert(logger, 'a define in the frame updates the cell', cell.v, 2);
    global.set('x', 3);
    assert(logger, 'a set! updates it', cell.v, 3);
    global.extend('y', 0).set('x', 4);
    assert(logger, 'a set! from an inner frame updates the frame holding the name', cell.v, 4);
    global.rebind('x', 5);
    assert(logger, 'rebinding updates it', cell.v, 5);

    const inner = global.extend('x', 'shadow');
    inner.set('x', 'changed');
    assert(logger, "a shadowing binding's writes leave the outer cell alone", cell.v, 5);

    global.define('z', 1);
    global.set('z', 2);
    assert(logger, 'writes to a name without a cell create none', global.cells.has('z'), false);
  }
  {
    // Installing compiled code replaces a library's copies of a procedure in
    // place; a cell over one of those copies must follow.
    const before = () => 'before';
    const after = () => 'after';
    const libraryEnv = new Environment(null);
    libraryEnv.define('p', before);
    registerLibrary('test.global-cells', new Map([['p', before]]), libraryEnv);
    const cell = libraryEnv.cellFor('p');
    substituteLibraryValues(new Map([[before, after]]));
    assert(logger, 'substituting a library value updates its cell', cell.v === after, true);
  }
  {
    const global = new Environment(null);
    global.define('x', 1);
    const inner = global.extend('y', 2);
    assert(logger, "globalCell finds the cell of the frame holding the name",
      globalCell(inner, 'x') === global.cellFor('x'), true);
    let unbound = null;
    try {
      globalCell(inner, 'no-such-name-anywhere');
    } catch (e) {
      unbound = e.constructor.name;
    }
    assert(logger, 'globalCell reports an unbound name as unbound', unbound, 'SchemeUnboundError');
  }

  logger.title('Global cells - compiled code sees every change to a global');
  {
    const pair = freshCompiledLibrary();
    const { compiled } = compileAndRun(pair, `
      (define scale 10)
      (define (scaled x) (* scale x))`);
    assert(logger, 'setup: the reader compiled', compiled.includes('scaled'), true);
    assert(logger, 'a compiled procedure reads a global', run(pair, '(scaled 2)'), 20n);
    run(pair, '(set! scale 100)');
    assert(logger, 'and sees it assigned', run(pair, '(scaled 2)'), 200n);
    run(pair, '(define scale 1000)');
    assert(logger, 'and redefined', run(pair, '(scaled 2)'), 2000n);
  }
  {
    // Compiled in order, so `ask` is compiled while `answer` is not yet bound.
    const pair = freshCompiledLibrary();
    const { compiled } = compileAndRun(pair, `
      (define (ask) (answer))
      (define (answer) 'first)`);
    assert(logger, 'setup: both compiled', compiled, ['ask', 'answer']);
    assert(logger, 'a forward reference resolves once the name is defined',
      run(pair, '(ask)').name, 'first');
    run(pair, "(define (answer) 'second)");
    assert(logger, 'and follows a redefinition after it resolved', run(pair, '(ask)').name, 'second');
  }
  {
    const pair = freshCompiledLibrary();
    compileAndRun(pair, '(define (read-later) later-defined)');
    let unbound = null;
    try {
      run(pair, '(read-later)');
    } catch (e) {
      unbound = e.message;
    }
    assert(logger, 'reading a global that is still unbound is an error',
      /later-defined/.test(unbound || ''), true);
    run(pair, '(define later-defined 7)');
    assert(logger, 'and once it is defined, the same procedure reads it', run(pair, '(read-later)'), 7n);
  }
  {
    // `'()` is JavaScript `null`, and `#f` and `0` are falsy: none of them may
    // be mistaken for a global that has not been read yet.
    const pair = freshCompiledLibrary();
    compileAndRun(pair, `
      (define nothing '())
      (define no #f)
      (define zero 0)
      (define (values-of) (list nothing no zero))`);
    assert(logger, "a global bound to '(), #f or 0 reads as itself",
      writeString(run(pair, '(values-of)')), '(() #f 0)');
    run(pair, "(set! nothing '(1))");
    assert(logger, "and one bound to '() is still seen when it changes",
      run(pair, '(car (car (values-of)))'), 1n);
  }
  {
    const pair = freshCompiledLibrary();
    const { compiled } = compileAndRun(pair, "(define (js-pi) (js-ref Math \"PI\"))");
    assert(logger, 'setup: the JavaScript-global reader compiled', compiled, ['js-pi']);
    assert(logger, 'a JavaScript global is read through the same path',
      Number(run(pair, '(js-pi)')), Math.PI);
  }
}
