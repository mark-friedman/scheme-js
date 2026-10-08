/**
 * @fileoverview Which way an unwind that reaches a run is finished.
 *
 * A capture made by compiled code, with only compiled frames between it and
 * the run beneath, is finished by a driver of the runtime's own, which keeps
 * the frames as a list and takes a continuation invoked from compiled code
 * running in it by a jump (`drive` in src/core/interpreter/unwind.js);
 * anything else is handed to the interpreter as before. The answers are
 * checked in both tiers by tests/tiers/continuation_tests.scm; these check,
 * by the counts the driver keeps, that each way is the one taken, which only
 * JavaScript can see.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { writeString } from '../../src/core/primitives/io/printer.js';
import { settle } from '../../src/compiler/runtime.js';
import { compileProgram } from '../../src/compiler/index.js';
import { nativeUnwinds } from '../../src/core/interpreter/unwind.js';
import { interpretedLibrary, installStandardLibrary } from '../harness/standard_library.js';

/**
 * An interpreter with the standard library compiled, a program's definitions
 * compiled into it, and what running more of it changes in the driver's
 * counts.
 * @param {string} definitions - The program's definitions.
 * @returns {{run: function(string): {value: string, finished: number, jumps: number, handedDown: number}}}
 */
function program(definitions) {
  const { interpreter, env } = interpretedLibrary();
  installStandardLibrary(env);
  const asts = parse(definitions).map((form) => analyze(form));
  compileProgram(asts, env, interpreter);
  return {
    run(source) {
      const before = { ...nativeUnwinds };
      let value;
      for (const form of parse(source)) {
        value = settle(interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' }));
      }
      return {
        value: writeString(value),
        finished: nativeUnwinds.finished - before.finished,
        jumps: nativeUnwinds.jumps - before.jumps,
        handedDown: nativeUnwinds.handedDown - before.handedDown
      };
    }
  };
}

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runNativeUnwindTests(logger) {
  logger.title('Unwinds finished by the driver, and handed to the interpreter');

  const escapes = program(`
    (define (find-first pred items)
      (call/cc (lambda (return)
                 (for-each (lambda (x) (if (pred x) (return x))) items)
                 #f)))`);
  const escape = escapes.run("(find-first even? '(1 3 4 5))");
  assert(logger, 'a capture made by compiled code is finished by the driver, and its escape is a jump',
    [escape.value, escape.finished, escape.jumps, escape.handedDown], ['4', 1, 1, 0]);
  const none = escapes.run("(find-first even? '(1 3 5))");
  assert(logger, 'one never invoked is finished by the driver too',
    [none.value, none.finished, none.jumps], ['#f', 1, 0]);

  const reentry = program(`
    (define saved #f)
    (define (capture-and-return x) (+ x (call/cc (lambda (k) (set! saved k) 0))))
    (define (re-enter)
      (let ((count 0) (results '()))
        (let ((r (capture-and-return 10)))
          (set! results (cons r results))
          (set! count (+ count 1))
          (if (< count 3) (saved count) (reverse results)))))`);
  const within = reentry.run('(re-enter)');
  assert(logger, 're-entered by compiled code while its driver runs, by jumps',
    [within.value, within.finished, within.jumps], ['(10 11 12)', 1, 2]);
  const after = reentry.run('(saved 5)');
  assert(logger, 'invoked after its driver has returned, the interpreter way, giving the same answer',
    [after.value, after.jumps], ['(10 11 12 15)', 0]);

  // `between` is interpreted: it names `dynamic-wind`, which the compiler
  // declines, so its frames are the interpreter's.
  const mixed = program(`
    (define (capture-here) (call/cc (lambda (k) (k 7))))
    (define (between f) (dynamic-wind (lambda () #f) f (lambda () #f)))
    (define (outer) (+ 1 (between capture-here)))`);
  const handed = mixed.run('(outer)');
  assert(logger, 'a capture beneath interpreter frames is handed to the interpreter',
    [handed.value, handed.finished], ['8', 0]);

  const deep = program(`
    (define (deep n) (if (= n 0) 0 (+ 1 (deep (- n 1)))))`);
  const moved = deep.run('(deep 100000)');
  assert(logger, 'frames moved to the heap are moved by the interpreter, as before',
    [moved.value, moved.finished, moved.jumps], ['100000', 0, 0]);
}
