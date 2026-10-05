/**
 * @fileoverview Calling a Scheme procedure from JavaScript that holds Scheme
 * values and wants a Scheme value back (`callSchemeProcedure` in
 * `src/core/interpreter/values.js`): how the evaluator calls the tier's Scheme
 * as a program runs, and how the compiler's door calls its entry points.
 *
 * What it promises: nothing converted either way, and otherwise what a plain
 * call does -- a pending tail call run to its value, a recursion deeper than
 * the JavaScript stack finished, a continuation captured inside working -- by
 * running a closure or a compiled procedure on its interpreter. Anything else
 * it calls directly, with compiled frames kept from moving to the heap
 * meanwhile, since the unwind that moves them would otherwise come back to the
 * JavaScript caller as the procedure's result. JavaScript tests, since only
 * JavaScript calls it.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { callSchemeProcedure, SCHEME_PRIMITIVE, SCHEME_RAW_CALL, TailCall } from '../../src/core/interpreter/values.js';
import { compiledStack, openCompiledSegment, restoreFlush } from '../../src/core/interpreter/unwind.js';
import { tryCompileDefinition } from '../../src/compiler/index.js';
import { Flonum } from '../../src/core/interpreter/number_representation.js';

/**
 * Runs Scheme source with the interpreter, with Scheme values.
 * @param {Object} interpreter - The interpreter.
 * @param {Object} env - The environment.
 * @param {string} source - Scheme source.
 */
function run(interpreter, env, source) {
  for (const form of parse(source)) {
    interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
  }
}

/**
 * A procedure that takes Scheme values and records whether compiled frames
 * could move to the heap while it ran.
 * @param {Array<boolean>} seen - Where to record it.
 * @param {*} [error] - What to throw after recording, if anything.
 * @returns {Function} The procedure, which returns its argument.
 */
function flushProbe(seen, error) {
  const probe = (x) => {
    seen.push(compiledStack.flushable);
    if (error !== undefined) throw error;
    return x;
  };
  probe[SCHEME_PRIMITIVE] = true;
  return probe;
}

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runSchemeCallTests(logger) {
  logger.title('Calling a Scheme Procedure From JavaScript, With Scheme Values');

  const { interpreter, env } = createInterpreter();
  run(interpreter, env, '(define (add1 x) (+ x 1))');
  const add1 = env.lookup('add1');
  assert(logger, 'setup: called as a plain function, a closure converts its result for JavaScript',
    typeof add1(41), 'number');
  // An inexact integer is boxed in Scheme and a double in JavaScript
  // (src/core/interpreter/number_representation.js), so it shows which a call
  // gives.
  assert(logger, 'an interpreted closure is given Scheme values and gives one back, unconverted',
    callSchemeProcedure(add1, [new Flonum(41)]) instanceof Flonum, true);

  const compiled = tryCompileDefinition(analyze(parse('(define (call-with f x) (f x))')[0]), env);
  assert(logger, 'setup: the procedure compiled', compiled.compiled, true);
  const callWith = compiled.procedure;
  assert(logger, 'setup: through its raw entry, a compiled procedure ending in a call to an interpreted closure returns the call pending',
    callWith[SCHEME_RAW_CALL](add1, 1n) instanceof TailCall, true);
  const result = callSchemeProcedure(callWith, [add1, new Flonum(1)]);
  assert(logger, 'a pending tail call is run to its value', result instanceof Flonum && result.value, 2);

  const depth = tryCompileDefinition(analyze(parse('(define (depth n) (if (= n 0) 0 (+ 1 (depth (- n 1)))))')[0]), env);
  env.define('depth', depth.procedure);
  assert(logger, 'a compiled recursion 100,000 deep finishes, its frames moved to the heap',
    String(callSchemeProcedure(depth.procedure, [100000n])), '100000');
  const escape = tryCompileDefinition(analyze(parse(
    '(define (escape-with x) (call-with-current-continuation (lambda (k) (+ 1 (k x)))))')[0]), env);
  assert(logger, 'a continuation captured in compiled code beneath it escapes',
    String(callSchemeProcedure(escape.procedure, [5n])), '5');

  // Scheme calling JavaScript calling Scheme, on one interpreter: the inner
  // run's continuations hold the outer run's frames beneath their own, and an
  // escape inside the inner run -- `guard` handling an error, say -- used to
  // unwind to the outer run, which dropped the JavaScript between them and
  // carried the escape's value on in the outer run. The library system loading
  // SRFI 135 ended that way, returning a `guard`'s answer as the exports.
  env.define('around', (thunk) => {
    const value = callSchemeProcedure(thunk, []);
    return ['returned to JavaScript', value];
  });
  env.lookup('around')[SCHEME_PRIMITIVE] = true;
  const valueOf = (source) => interpreter.run(analyze(parse(source)[0]), env, [], undefined, { jsAutoConvert: 'raw' });
  const nested = valueOf(
    "(around (lambda () (call-with-current-continuation (lambda (k) (k 'escaped) 'not-escaped))))");
  assert(logger, 'an escape inside a run JavaScript started stays in that run',
    Array.isArray(nested) ? nested.map(String) : String(nested), ['returned to JavaScript', 'escaped']);
  const guarded = valueOf(
    "(list (around (lambda () (call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k 'handled)) (lambda () (raise 'oops))))))) 'after)");
  assert(logger, 'as does an escape from an exception handler there, as guard makes',
    guarded ? [guarded.car.map(String), String(guarded.cdr.car)] : null, [['returned to JavaScript', 'handled'], 'after']);

  {
    // As beneath compiled code the interpreter called, where frames may move.
    const seen = [];
    const flush = openCompiledSegment();
    let after;
    try {
      callSchemeProcedure(flushProbe(seen), [1n]);
      after = compiledStack.flushable;
    } finally {
      restoreFlush(flush);
    }
    assert(logger, 'beneath a primitive it calls directly, compiled frames may not move to the heap', seen.join(' '), 'false');
    assert(logger, 'and may again once it returns', after, true);
  }
  {
    const seen = [];
    const flush = openCompiledSegment();
    let after;
    let thrown = null;
    try {
      try {
        callSchemeProcedure(flushProbe(seen, new Error('probe')), [1n]);
      } catch (e) {
        thrown = e.message;
      }
      after = compiledStack.flushable;
    } finally {
      restoreFlush(flush);
    }
    assert(logger, 'what the procedure throws reaches the caller', thrown, 'probe');
    assert(logger, 'and frames may move again after it', [seen.join(' '), after].join(' '), 'false true');
  }
}
