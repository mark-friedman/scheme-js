/**
 * @fileoverview Errors raised inside compiled code arrive as the interpreter's do.
 *
 * `error` does not raise by itself: it returns a pending raise for whoever
 * called it to perform, which the interpreter does by running it. Compiled code
 * that called `error` where it wanted a value -- the argument checks in the
 * compiled standard library's `length`, `assv` and `member` -- tried to call the
 * pending raise as a procedure, so the program failed with JavaScript's
 * "args is not iterable" instead of "length: expected list", and a `guard`
 * received that message instead of the error `error` made.
 *
 * The differential cases in `compiler_tests.js` cover compiled user code. These
 * cover the compiled standard library, which those cases do not install; a raise
 * that reaches a JavaScript caller with no interpreter run beneath it; and the
 * debugger pausing on an uncaught error raised in compiled code.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { intern } from '../../src/core/interpreter/symbol.js';
import { SchemeError } from '../../src/core/interpreter/errors.js';
import { tryCompileDefinition } from '../../src/compiler/index.js';
import { settle } from '../../src/compiler/runtime.js';
import { SchemeDebugRuntime } from '../../src/debug/scheme_debug_runtime.js';
import { interpretedLibrary, installStandardLibrary } from '../harness/standard_library.js';
import { writeString } from '../../src/core/primitives/io/printer.js';

/**
 * Runs Scheme source and returns the last value written out, or the message
 * of the error it raised.
 * @param {Object} pair - The interpreter and environment.
 * @param {string} source - Scheme source.
 * @returns {string} The last value, or `error: ` and the message.
 */
function run({ interpreter, env }, source) {
  try {
    let value;
    for (const form of parse(source)) {
      value = settle(interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' }));
    }
    return writeString(value);
  } catch (e) {
    return `error: ${e.message}`;
  }
}

/**
 * Compiles one definition into an environment.
 * @param {string} source - One `define` form.
 * @param {Object} env - The environment.
 * @returns {Function} The compiled procedure.
 */
function compile(source, env) {
  const result = tryCompileDefinition(analyze(parse(source)[0]), env);
  if (!result.compiled) throw new Error(`did not compile: ${result.reason}`);
  env.define(result.name, result.procedure);
  return result.procedure;
}

/**
 * Calls a function and returns what it threw, or null.
 * @param {Function} thunk - The call to make.
 * @returns {*} The thrown value, or null if it returned.
 */
function thrownBy(thunk) {
  try {
    thunk();
  } catch (e) {
    return e;
  }
  return null;
}

/**
 * Runs a program under `runAsync` with a debugger that pauses on uncaught
 * exceptions, and returns the pause events it saw.
 * @param {Object} pair - The interpreter and environment.
 * @param {string} source - One expression.
 * @returns {Promise<Array<Object>>} The pause events.
 */
async function pausesOn({ interpreter, env }, source) {
  const events = [];
  const runtime = new SchemeDebugRuntime({
    onPause: (event) => {
      events.push(event);
      // Resume, or runAsync waits on the pause controller forever.
      setTimeout(() => runtime.resume(), 5);
    }
  });
  runtime.breakOnUncaughtException = true;
  interpreter.setDebugRuntime(runtime);
  runtime.enable();
  try {
    await interpreter.runAsync(analyze(parse(source)[0]), env, { stepsPerYield: 1000 });
  } catch (e) {
    // The error still propagates after the pause.
  } finally {
    interpreter.setDebugRuntime(null);
  }
  return events;
}

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runCompiledErrorTests(logger) {
  logger.title('Compiler - Errors Raised Inside Compiled Code');

  const interpreted = interpretedLibrary();
  const compiled = interpretedLibrary();
  const { installed } = installStandardLibrary(compiled.env);
  for (const name of ['length', 'assv', 'member', 'map']) {
    assert(logger, `setup: ${name} is compiled`, installed.includes(name), true);
  }

  // The compiled library's argument checks raise with `error` where the
  // procedure wants a value.
  for (const expr of [
    '(length 5)',
    '(assv 1 5)',
    '(member 1 5)',
    '(car (length 5))',
    // Compiled `map`, an interpreted procedure in a nested run, compiled `length`.
    '(map (lambda (x) (length x)) (list 5))'
  ]) {
    const expected = run(interpreted, expr);
    assert(logger, `setup: the interpreted library raises on ${expr}`, expected.startsWith('error:'), true);
    assert(logger, `the compiled library raises the same on ${expr}`, run(compiled, expr), expected);
    const guarded = `(guard (e ((error-object? e) (list (error-object-message e) (error-object-irritants e)))) ${expr})`;
    assert(logger, `a guard receives the same error from ${expr}`, run(compiled, guarded), run(interpreted, guarded));
  }

  // Compiled code called by JavaScript directly has no interpreter run beneath
  // it to perform the raise, so the caller receives what the interpreter would
  // have thrown: the error `error` made, not a stand-in for it.
  compile('(define (check x) (if (pair? x) x (error "check: not a pair" x)))', compiled.env);
  const first = compile('(define (first x) (car (check x)))', compiled.env);
  const fromError = thrownBy(() => settle(first(5n)));
  assert(logger, 'called from JavaScript, an error raised in compiled code reaches it',
    fromError instanceof SchemeError, true);
  assert(logger, 'as the error that error made, message and irritants',
    fromError instanceof SchemeError && `${fromError.message} ${fromError.irritants.length} ${fromError.irritants[0]}`,
    'check: not a pair 1 5');
  const app = compile('(define (app g x) (+ 1 (g x)))', compiled.env);
  run(interpreted, '(define (app g x) (+ 1 (g x)))');
  const boom = intern('boom');
  const fromRaise = thrownBy(() => settle(app(compiled.env.lookup('raise'), boom)));
  const fromRaiseInterpreted = thrownBy(() => interpreted.env.lookup('app')(interpreted.env.lookup('raise'), boom));
  assert(logger, 'a raise of a symbol reaching JavaScript says what the interpreter says',
    fromRaise && fromRaise.message, fromRaiseInterpreted && fromRaiseInterpreted.message);

  // The debugger pauses where the interpreter performs the raise, which for
  // compiled code is where the interpreter called it.
  const interpretedPauses = await pausesOn(interpretedLibrary(), '(length 5)');
  assert(logger, 'setup: the debugger pauses on an uncaught error from the interpreted library',
    interpretedPauses.length, 1);
  const pair = interpretedLibrary();
  installStandardLibrary(pair.env);
  const pauses = await pausesOn(pair, '(length 5)');
  assert(logger, 'it pauses on an uncaught error raised in compiled code', pauses.length, 1);
  assert(logger, 'with the error the compiled code raised',
    pauses[0] && pauses[0].reason === 'exception' && pauses[0].exception.message, 'length: expected list');
}
