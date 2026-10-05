/**
 * @fileoverview Mutable strings at the JavaScript boundary.
 *
 * JavaScript has no mutable strings, so a Scheme string crosses into
 * JavaScript as the characters it holds at that moment, as a number crosses as
 * its value: JavaScript always sees a real string, and a later `string-set!` on
 * the Scheme side is not seen there. A string from JavaScript stays a
 * JavaScript string, immutable in Scheme like a literal.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { SchemeString } from '../../src/core/primitives/string_class.js';
import { schemeToJs, schemeToJsDeep } from '../../src/core/interpreter/js_interop.js';
import { attachTier } from '../../src/compiler/tiering.js';
import { interpretedLibrary, installStandardLibrary } from '../harness/standard_library.js';

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 */
export function runStringInteropTests(logger) {
  const { interpreter, env } = interpretedLibrary();
  const raw = (source) => {
    let value;
    for (const form of parse(source)) value = interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
    return value;
  };

  logger.title('Mutable Strings - Into JavaScript');
  {
    const made = raw('(string-copy "abc")');
    assert(logger, 'setup: a newly made string is a SchemeString', made instanceof SchemeString, true);
    assert(logger, 'converted, it is a JavaScript string', typeof schemeToJs(made), 'string');
    assert(logger, 'and so deep inside a vector', typeof schemeToJsDeep([made])[0], 'string');

    const seen = [];
    env.define('js-sees', (value) => { seen.push(value); return typeof value; });
    assert(logger, 'a JavaScript function called from Scheme receives a string',
      raw('(let ((s (string-copy "abc"))) (string-set! s 0 #\\x) (js-sees s))'), 'string');
    assert(logger, 'holding the characters it had then', seen[0], 'xbc');

    const holder = { name: null };
    env.define('holder', holder);
    raw('(define kept (string-copy "abc"))');
    raw('(js-set! holder "name" kept)');
    assert(logger, 'js-set! stores a JavaScript string', typeof holder.name, 'string');
    raw('(string-set! kept 0 #\\z)');
    assert(logger, 'and a later change on the Scheme side is not seen there', holder.name, 'abc');
    assert(logger, 'js-obj stores its values the same way', typeof raw('(js-obj "k" (string-copy "v"))').k, 'string');
    assert(logger, 'js-typeof says what JavaScript will see', raw('(js-typeof (string-copy "v"))'), 'string');

    const closure = raw('(lambda () (string-append "a" "b"))');
    assert(logger, 'a Scheme procedure called from JavaScript returns a string', typeof closure(), 'string');
    assert(logger, 'with its characters', closure(), 'ab');
  }

  logger.title('Mutable Strings - Into JavaScript From Compiled Code');
  {
    // Compiled code calls a JavaScript function itself, not through the
    // interpreter, and must convert as the interpreter does: a string as its
    // characters, an exact integer as a number.
    const pair = interpretedLibrary();
    installStandardLibrary(pair.env);
    const received = [];
    pair.env.define('js-sees', (...values) => { received.push(values.map((v) => typeof v).join(' ')); return 1; });
    const run = (source) => {
      let value;
      for (const form of parse(source)) value = pair.interpreter.runTopLevel(analyze(form), pair.env, { jsAutoConvert: 'raw' });
      return value;
    };
    run('(define (interpreted) (js-sees 5 (string-copy "a")))');
    run('(interpreted)');
    attachTier(pair.interpreter, pair.env);
    run('(define (calls-it) (let loop ((i 0)) (if (< i 1) (begin (js-sees 5 (string-copy "a")) (loop (+ i 1))) i)))');
    run('(define (tail-calls-it) (let loop ((i 0)) (if (< i 1) (loop (+ i 1)) (js-sees 5 (string-copy "a")))))');
    assert(logger, 'setup: both are compiled',
      [pair.env.lookup('calls-it').$compiled, pair.env.lookup('tail-calls-it').$compiled].join(' '), 'true true');
    run('(calls-it)');
    run('(tail-calls-it)');
    assert(logger, 'setup: the interpreter converts', received[0], 'number string');
    assert(logger, 'a call from compiled code converts as the interpreter does', received[1], 'number string');
    assert(logger, 'and so does a tail call', received[2], 'number string');
  }

  logger.title('Mutable Strings - From JavaScript');
  {
    env.define('from-js', { name: 'abc' });
    assert(logger, 'a string read from JavaScript is a JavaScript string',
      typeof raw('(js-ref from-js "name")'), 'string');
    let message = null;
    try {
      raw('(string-set! (js-ref from-js "name") 0 #\\x)');
    } catch (e) {
      message = e.message;
    }
    assert(logger, 'and changing it is refused, saying how to get one that can be changed',
      message !== null && message.includes('immutable') && message.includes('string-copy'), true);
    assert(logger, 'string-copy makes one',
      raw('(let ((s (string-copy (js-ref from-js "name")))) (string-set! s 0 #\\x) s)').toString(), 'xbc');
    assert(logger, 'and the JavaScript string is unchanged', raw('(js-ref from-js "name")'), 'abc');
  }
}
