/**
 * @fileoverview Compiled Scheme in a JavaScript stack trace.
 *
 * A compiled procedure runs in a JavaScript frame of its own, one for each
 * live Scheme frame, so a stack trace or a profile of compiled code is a
 * Scheme stack -- if its frames say which Scheme procedures they are. Each
 * generated function is named after its procedure, and code the compiler
 * generates as a program runs is named by a `scheme:` URL for the procedure
 * it holds, where an engine would otherwise show where `new Function` was
 * called. Only JavaScript can see either, so these are JavaScript tests.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { tryCompileDefinition } from '../../src/compiler/index.js';
import { callSchemeProcedure } from '../../src/core/interpreter/values.js';
import { withPrivateLibraries } from '../../src/core/interpreter/library_registry.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';
import { installLibraryTable, libraryRestorer } from '../../src/compiler/prebuilt.js';
import prebuiltLibraries from '../../src/packaging/compiled_libraries.js';

/**
 * Compiles one definition into an environment.
 * @param {string} source - One `define` form.
 * @param {Object} env - The environment to compile against and define into.
 * @param {string} [filename] - The file the source is to be read as from.
 * @returns {Function} The compiled procedure.
 */
function compile(source, env, filename) {
  const [form] = parse(source, filename === undefined ? {} : { filename });
  const result = tryCompileDefinition(analyze(form), env);
  if (!result.compiled) throw new Error(`did not compile: ${result.reason}`);
  env.define(result.name, result.procedure);
  return result.procedure;
}

/**
 * The stack trace of what calling a procedure raises.
 * @param {Function} procedure - A Scheme procedure.
 * @param {Array<*>} args - Its arguments, as Scheme values.
 * @returns {string} The trace, or '' if nothing was raised or it has none.
 */
function traceOf(procedure, args) {
  try {
    callSchemeProcedure(procedure, args);
  } catch (e) {
    return typeof e?.stack === 'string' ? e.stack : '';
  }
  return '';
}

/**
 * How many times a text occurs in another.
 * @param {string} text - The text searched.
 * @param {string} part - What is counted.
 * @returns {number}
 */
function occurrences(text, part) {
  return text.split(part).length - 1;
}

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runCompiledStackTests(logger) {
  logger.title('Compiled Scheme in a JavaScript stack trace');
  // The standard library loaded as a page loads it, its procedures restored
  // from their prebuilt tables, in a registry of the test's own.
  const bundled = (name) => BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] ?? BUNDLED_SOURCES[name[name.length - 1]];
  withPrivateLibraries({
    resolver: bundled,
    hook: (name, env) => installLibraryTable(prebuiltLibraries, name, env, (file) => BUNDLED_SOURCES[file]),
    restorer: libraryRestorer(prebuiltLibraries)
  }, () => {
    const { interpreter, env } = createInterpreter();
    interpreter.run(analyze(parse('(import (scheme base))')[0]), env, [], undefined, { jsAutoConvert: 'raw' });
    traces(logger, env);
  });
}

/**
 * The tests, in an environment that has imported `(scheme base)`.
 * @param {Object} logger - Test logger.
 * @param {Object} env - The environment.
 */
function traces(logger, env) {

  // A recursion that raises at the bottom: each level is a frame of its own.
  compile(`(define (count-down n)
             (if (= n 0) (vector-ref (vector) 0) (+ 1 (count-down (- n 1)))))`, env, 'stack.scm');
  const recursion = traceOf(env.lookup('count-down'), [3n]);
  assert(logger, 'a compiled procedure\'s frames are named after it, one for each level',
    occurrences(recursion, 'count-down') >= 4, true);
  assert(logger, 'generated as the program runs, its code is named for the procedure it holds',
    recursion.includes('scheme:///stack.scm/count-down'), true);

  // A loop inside a procedure is a procedure of its own, named after its loop.
  compile(`(define (sum-to n)
             (let walk ((i 0) (acc 0))
               (if (> i n) (vector-ref (vector) acc) (walk (+ i 1) (+ acc i)))))`, env);
  const loop = traceOf(env.lookup('sum-to'), [3n]);
  assert(logger, 'a procedure with no file is named by the program',
    loop.includes('scheme:///program/sum-to'), true);

  // A name a URL would read otherwise -- `?` begins a query -- is escaped in
  // the URL, and only there.
  compile('(define (empty-vector-head? v) (vector-ref v 0))', env);
  const escaped = traceOf(env.lookup('empty-vector-head?'), [[]]);
  assert(logger, 'a name is escaped in the URL',
    escaped.includes('scheme:///program/empty-vector-head%3F'), true);
  assert(logger, 'and not in the frame', escaped.includes('empty-vector-head?'), true);

  // A procedure of a shipped library is code the build generated, installed
  // from its table: its frame has its name too.
  assert(logger, 'setup: vector-map is compiled', env.lookup('vector-map').$compiled, true);
  const shipped = traceOf(env.lookup('vector-map'), [env.lookup('car'), [1n]]);
  assert(logger, 'a shipped library\'s procedure is named after itself',
    shipped.includes('vector-map'), true);
}
