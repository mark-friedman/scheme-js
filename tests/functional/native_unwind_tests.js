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
import { settle, runAhead } from '../../src/compiler/runtime.js';
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
    env,
    /**
     * Calls one of the program's procedures with no interpreter beneath,
     * as a program compiled ahead of time runs (`runAhead`).
     * @param {string} name - The procedure.
     * @param {Array<*>} args - Its arguments, Scheme values.
     * @returns {{value: string, finished: number, jumps: number}}
     */
    ahead(name, args) {
      const before = { ...nativeUnwinds };
      const value = writeString(runAhead(env.lookup(name), args));
      return { value, finished: nativeUnwinds.finished - before.finished, jumps: nativeUnwinds.jumps - before.jumps };
    },
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

  logger.title('With no interpreter beneath: a program compiled ahead of time');

  const tak = program(`
    (define (ctak x y z) (call/cc (lambda (k) (ctak-aux k x y z))))
    (define (ctak-aux k x y z)
      (if (not (< y x))
          (k z)
          (call/cc (lambda (k)
                     (ctak-aux k
                               (call/cc (lambda (k) (ctak-aux k (- x 1) y z)))
                               (call/cc (lambda (k) (ctak-aux k (- y 1) z x)))
                               (call/cc (lambda (k) (ctak-aux k (- z 1) x y))))))))`);
  const ctak = tak.ahead('ctak', [12, 8, 4]);
  assert(logger, 'a continuation taken at every call gives tak\'s answer, every capture finished by the driver',
    [ctak.value, ctak.finished > 0, ctak.jumps > 0], ['5', true, true]);

  assert(logger, 'an escape from for-each', escapes.ahead('find-first', [escapes.env.lookup('even?'), parse("(1 3 4 5)")[0]]).value, '4');
  const again = reentry.ahead('re-enter', []);
  assert(logger, 're-entered twice by jumps, after the procedure that took it returned',
    [again.value, again.jumps], ['(10 11 12)', 2]);
  assert(logger, 'and again after its driver has returned, through a driver of its own',
    writeString(runAhead(reentry.env.lookup('saved'), [5])), '(10 11 12 15)');
  const deepAhead = deep.ahead('deep', [100000]);
  assert(logger, 'a recursion 100,000 deep, its frames moved to the heap by the driver',
    [deepAhead.value, deepAhead.finished > 0], ['100000', true]);

  const values = program(`(define (two) (call/cc (lambda (k) (k 1 2))))
                          (define (sum-two) (call-with-values two +))`);
  assert(logger, 'a continuation given two values', values.ahead('sum-two', []).value, '3');
}

