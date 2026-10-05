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
import { analyze } from '../../src/core/interpreter/expand.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { tryCompileDefinition } from '../../src/compiler/index.js';
import { callSchemeProcedure } from '../../src/core/interpreter/values.js';
import { withPrivateLibraries } from '../../src/core/interpreter/library_registry.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';
import { installLibraryTable, libraryRestorer } from '../../src/compiler/prebuilt.js';
import prebuiltLibraries from '../../src/packaging/compiled_libraries.js';
import { rememberSourceText } from '../../src/core/interpreter/source_texts.js';

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
  return result;
}

const BASE64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

/**
 * The source map a script names in a `data:` URL, decoded: its sources and
 * their text where it has it, and for each generated line its segments as
 * absolute `[column, source, line, column]`, counted from zero.
 * @param {string} script - The script.
 * @returns {{sources: string[], sourcesContent: (Array<string|null>|undefined),
 *   lines: Array<Array<number[]>>}|null}
 */
function sourceMapOf(script) {
  const match = /\/\/# sourceMappingURL=data:application\/json;charset=utf-8,(\S+)/.exec(script);
  if (match === null) return null;
  const map = JSON.parse(decodeURIComponent(match[1]));
  const state = [0, 0, 0];
  const lines = map.mappings.split(';').map((text) => {
    let column = 0;
    return text === '' ? [] : text.split(',').map((segment) => {
      const fields = [];
      let value = 0;
      let shift = 0;
      for (const digit of segment) {
        const d = BASE64.indexOf(digit);
        value += (d & 31) << shift;
        shift += 5;
        if ((d & 32) === 0) {
          fields.push(value & 1 ? -(value >> 1) : value >> 1);
          value = 0;
          shift = 0;
        }
      }
      column += fields[0];
      for (let i = 0; i < 3; i++) state[i] += fields[i + 1];
      return [column, ...state];
    });
  });
  return { sources: map.sources, sourcesContent: map.sourcesContent, lines };
}

/**
 * Where a source map puts a position of its script: the last segment of its
 * line at or before its column.
 * @param {Object} map - What `sourceMapOf` returns.
 * @param {number} line - The script's line, from one, as a stack trace says.
 * @param {number} column - Its column, from one.
 * @returns {string|null} As `file:line:column`, counted from one, as a stack
 *   trace counts.
 */
function originalPosition(map, line, column) {
  const segments = (map.lines[line - 1] ?? []).filter((segment) => segment[0] <= column - 1);
  if (segments.length === 0) return null;
  const [, source, sourceLine, sourceColumn] = segments[segments.length - 1];
  return `${map.sources[source]}:${sourceLine + 1}:${sourceColumn + 1}`;
}

/**
 * The positions of a procedure's frames in a stack trace, innermost first.
 * @param {string} trace - The trace.
 * @param {string} url - The URL its code is named by.
 * @returns {Array<{line: number, column: number}>}
 */
function framesAt(trace, url) {
  const pattern = new RegExp(`${url.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}:(\\d+):(\\d+)`, 'g');
  return [...trace.matchAll(pattern)].map((m) => ({ line: Number(m[1]), column: Number(m[2]) }));
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
  const countDown = `(define (count-down n)
             (if (= n 0) (vector-ref (vector) 0) (+ 1 (count-down (- n 1)))))`;
  const { source: script } = compile(countDown, env, 'stack.scm');
  const recursion = traceOf(env.lookup('count-down'), [3n]);
  assert(logger, 'a compiled procedure\'s frames are named after it, one for each level',
    occurrences(recursion, 'count-down') >= 4, true);
  assert(logger, 'generated as the program runs, its code is named for the procedure it holds',
    recursion.includes('scheme:///stack.scm/count-down'), true);

  // Its source map puts each frame at the Scheme expression whose code holds
  // the call the frame is in: the innermost at the `vector-ref` that raised,
  // each other at the recursive call.
  const map = sourceMapOf(script);
  assert(logger, 'its code carries a source map naming its file', map?.sources, ['stack.scm']);
  const sourceLine = countDown.split('\n')[1];
  const at = (text) => `stack.scm:2:${sourceLine.indexOf(text) + 1}`;
  const frames = framesAt(recursion, 'scheme:///stack.scm/count-down');
  assert(logger, 'which puts the frame that raised at the expression that raised',
    map && frames.length > 0 ? originalPosition(map, frames[0].line, frames[0].column) : null,
    at('(vector-ref (vector) 0)'));
  assert(logger, 'and each frame beneath it at the recursive call',
    map ? frames.slice(1).map((f) => originalPosition(map, f.line, f.column)) : null,
    [at('(count-down (- n 1))'), at('(count-down (- n 1))'), at('(count-down (- n 1))')]);

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

  // A page's inline script has no file a debugger could fetch, so its text,
  // remembered under the name it was read under, goes into the map.
  const inline = '(define (inline-head v) (vector-ref v 0))';
  rememberSourceText('page.html#scheme-1', inline);
  const { source: inlineScript } = compile(inline, env, 'page.html#scheme-1');
  const inlineMap = sourceMapOf(inlineScript);
  assert(logger, "an inline script's map names it", inlineMap?.sources, ['page.html#scheme-1']);
  assert(logger, 'and holds its text', inlineMap?.sourcesContent, [inline]);
  assert(logger, 'which its URL escapes',
    traceOf(env.lookup('inline-head'), [[]]).includes('scheme:///page.html%23scheme-1/inline-head'), true);
  assert(logger, 'a file a debugger can fetch has no text in its map',
    sourceMapOf(script)?.sourcesContent, undefined);

  // A file named by an absolute URL, as a page's script with a `src` is: its
  // map names the URL, which a debugger fetches, and its code is named by the
  // URL's path, where the host would only repeat itself.
  compile('(define (fetched-head v) (vector-ref v 0))', env, 'http://example.test/app/main.scm');
  assert(logger, "a fetched file's code is named by its path",
    traceOf(env.lookup('fetched-head'), [[]]).includes('scheme:///app/main.scm/fetched-head'), true);

  // A procedure of a shipped library is code the build generated, installed
  // from its table: its frame has its name too.
  assert(logger, 'setup: vector-map is compiled', env.lookup('vector-map').$compiled, true);
  const shipped = traceOf(env.lookup('vector-map'), [env.lookup('car'), [1n]]);
  assert(logger, 'a shipped library\'s procedure is named after itself',
    shipped.includes('vector-map'), true);
}
