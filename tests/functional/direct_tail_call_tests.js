/**
 * @fileoverview Tail calls between compiled procedures are made directly.
 *
 * A tail call used to return a `TailCall` to the nearest trampoline, which
 * allocated it and its argument array and then called the callee through a
 * spread -- on every tail call to anything but the procedure itself. Now a tail
 * call to a compiled procedure or a primitive is an ordinary JavaScript call,
 * while the stack those calls hold stays inside a budget; past it, and for any
 * other callee, the call is returned to the trampoline as before, so a chain of
 * tail calls still runs in bounded space.
 *
 * These pin the rule itself, which the differential cases in
 * `compiler_tests.js` cannot see: they only compare answers. Which callees are
 * called directly, that a spent budget falls back, and that the budget is
 * always given back -- including when an error passes through, which is the
 * case that would otherwise leave every later tail call on the slow path.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { tryCompileDefinition } from '../../src/compiler/index.js';
import { invoke, settle, tailStack, TailCall } from '../../src/compiler/runtime.js';

/**
 * Compiles one definition into an environment.
 * @param {string} source - One `define` form.
 * @param {Object} env - The environment to compile against and define into.
 * @returns {Function} The compiled procedure.
 */
function compile(source, env) {
  const result = tryCompileDefinition(analyze(parse(source)[0]), env);
  if (!result.compiled) throw new Error(`did not compile: ${result.reason}`);
  env.define(result.name, result.procedure);
  return result.procedure;
}

/**
 * Runs Scheme source with the interpreter and returns the last value.
 * @param {Object} interpreter - The interpreter.
 * @param {Object} env - The environment.
 * @param {string} source - Scheme source.
 * @returns {*} The value of the last form.
 */
function run(interpreter, env, source) {
  let value;
  for (const form of parse(source)) {
    value = settle(interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' }));
  }
  return value;
}

/**
 * Runs the direct tail call tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runDirectTailCallTests(logger) {
  logger.title('Compiler - Tail Calls Between Procedures Are Made Directly');

  const { interpreter, env } = createInterpreter();
  const callWithOne = compile('(define (call-with-one g) (g 1))', env);
  compile('(define (compiled-identity x) x)', env);
  // Left interpreted: defined by the interpreter, never compiled.
  run(interpreter, env, '(define (interpreted-identity x) x)');

  {
    const value = invoke(callWithOne, [env.lookup('compiled-identity')]);
    assert(logger, 'a tail call to a compiled procedure returns its value, not a TailCall', value, 1n);
    assert(logger, 'and gives back the stack it held', tailStack.used, 0);
  }
  {
    const value = invoke(callWithOne, [env.lookup('list')]);
    assert(logger, 'a tail call to a primitive is made directly too', value instanceof TailCall, false);
  }
  {
    // An interpreted closure is entered through the interpreter, and a
    // continuation throws to get where it is going; the trampoline is the
    // cheaper way to reach either.
    const value = invoke(callWithOne, [env.lookup('interpreted-identity')]);
    assert(logger, 'a tail call to an interpreted closure still goes to the trampoline',
      value instanceof TailCall, true);
    assert(logger, 'which runs it', settle(value), 1n);
  }
  {
    const saved = tailStack.used;
    tailStack.used = tailStack.limit;
    try {
      const value = invoke(callWithOne, [env.lookup('compiled-identity')]);
      assert(logger, 'with the budget spent, a tail call goes to the trampoline',
        value instanceof TailCall, true);
      assert(logger, 'and still gives the right answer', settle(value), 1n);
    } finally {
      tailStack.used = saved;
    }
  }

  // Every direct tail call gives back what it took on the way out, and an
  // error thrown through one is a way out.
  {
    compile("(define (fail-after n) (if (= n 0) (error \"stop\" n) (fail-again (- n 1))))", env);
    compile('(define (fail-again n) (fail-after n))', env);
    let caught = null;
    try {
      run(interpreter, env, '(fail-after 10)');
    } catch (e) {
      caught = e.message;
    }
    assert(logger, 'setup: the error reached the caller', /stop/.test(caught || ''), true);
    assert(logger, 'an error thrown through direct tail calls leaves the budget as it was',
      tailStack.used, 0);
  }
  {
    // The same through Scheme's own handler, many times over: had each one
    // kept what it held, the budget would be spent and every tail call after
    // it would go back to the trampoline.
    const caught = run(interpreter, env, `
      (let loop ((i 0) (caught 0))
        (if (= i 200)
            caught
            (loop (+ i 1) (+ caught (guard (e (#t 1)) (fail-after 50))))))`);
    assert(logger, 'setup: every error was caught', caught, 200n);
    assert(logger, 'errors caught by guard leave the budget as it was', tailStack.used, 0);
  }

  // The budget is in stack, not in calls: each call site adds its own frame's
  // size, so a procedure with many locals gets fewer direct calls before the
  // trampoline takes over. A limit counted in calls would let a chain of large
  // frames exhaust the JavaScript stack.
  {
    const locals = Array.from({ length: 150 }, (_, i) => `(a${i} (+ n ${i}))`).join(' ');
    const sum = Array.from({ length: 150 }, (_, i) => `a${i}`).join(' ');
    compile(`(define (wide n) (let* (${locals}) (if (= n 0) (+ ${sum}) (wide-again (- n 1)))))`, env);
    compile('(define (wide-again n) (wide n))', env);
    let outcome;
    try {
      outcome = settle(invoke(env.lookup('wide'), [20000n]));
    } catch (e) {
      outcome = e.message;
    }
    assert(logger, 'a long chain of tail calls between large frames runs in bounded stack',
      outcome, 11175n);
    assert(logger, 'and gives back everything it held', tailStack.used, 0);
  }
}
