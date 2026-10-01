/**
 * @fileoverview Calling a Scheme procedure from JavaScript that holds Scheme
 * values and wants a Scheme value back (`callSchemeProcedure` in
 * `src/core/interpreter/values.js`): how the evaluator calls the tier's Scheme
 * as a program runs, and how the compiler's door calls its entry points.
 *
 * What it promises: nothing converted either way, a pending tail call run to
 * its value, and compiled frames kept from moving to the heap while it runs --
 * the unwind that moves them would otherwise come back to the JavaScript
 * caller as the procedure's result. JavaScript tests, since only JavaScript
 * calls it.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { callSchemeProcedure, SCHEME_PRIMITIVE, TailCall } from '../../src/core/interpreter/values.js';
import { compiledStack, openCompiledSegment, restoreFlush } from '../../src/core/interpreter/unwind.js';
import { tryCompileDefinition } from '../../src/compiler/index.js';

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
  assert(logger, 'an interpreted closure is given Scheme values and gives one back, unconverted',
    typeof callSchemeProcedure(add1, [41n]), 'bigint');

  const compiled = tryCompileDefinition(analyze(parse('(define (call-with f x) (f x))')[0]), env);
  assert(logger, 'setup: the procedure compiled', compiled.compiled, true);
  const callWith = compiled.procedure;
  assert(logger, 'setup: a compiled procedure ending in a call to an interpreted closure returns the call pending',
    callWith(add1, 1n) instanceof TailCall, true);
  const result = callSchemeProcedure(callWith, [add1, 1n]);
  assert(logger, 'a pending tail call is run to its value', [typeof result, String(result)].join(' '), 'bigint 2');

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
    assert(logger, 'compiled frames may not move to the heap while it runs', seen.join(' '), 'false');
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
