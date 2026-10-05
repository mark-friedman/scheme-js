/**
 * @fileoverview A closure run compiled.
 *
 * A closure the compiler compiles keeps being the object every holder of it
 * has -- a name bound to it, a list the program made, another closure's
 * environment -- and runs as its compiled procedure (`runCompiled` in
 * src/core/interpreter/values.js), until it is made to run interpreted again
 * (`runInterpreted`). These pin what that means at each way a closure is
 * called: by the interpreter, by JavaScript, by compiled code, by
 * `callSchemeProcedure`. A compiled procedure that answers differently from
 * the closure stands in for the compiler's, so that which ran shows.
 */

import { assert } from '../harness/helpers.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { writeString } from '../../src/core/primitives/io/printer.js';
import { list } from '../../src/core/interpreter/cons.js';
import { intern } from '../../src/core/interpreter/symbol.js';
import {
  runCompiled, runInterpreted, callSchemeProcedure, isSchemeClosure, SCHEME_RAW_CALL
} from '../../src/core/interpreter/values.js';
import { generateEnvironment, tryCompileClosure } from '../../src/compiler/index.js';
import * as R from '../../src/compiler/runtime.js';

/**
 * Runs the tests.
 * @param {Object} logger - The test logger.
 */
export async function runCompiledClosureTests(logger) {
  logger.title('A closure run compiled');
  const { interpreter, env } = createInterpreter();
  const run = (text) => {
    let value;
    for (const form of parse(text)) value = interpreter.run(analyze(form), env, [], undefined, { jsAutoConvert: 'raw' });
    return value;
  };
  const show = (text) => writeString(run(text));

  run("(define (f x) (list 'interpreted x))");
  run('(define kept (list f))');
  run('(define (call-f y) (f y))');
  const closure = env.lookup('f');
  const interpretedRaw = closure[SCHEME_RAW_CALL];
  const compiled = R.markProcedure(function (x) { return list(intern('compiled'), x); }, 'f', env);
  const offered = () => generateEnvironment(env).generated.some((entry) => entry.name === 'f');

  assert(logger, 'setup: interpreted, the closure runs its body', show('(f 0)'), '(interpreted 0)');
  const printed = show('f');
  runCompiled(closure, compiled);
  assert(logger, 'run compiled, the interpreter applies its compiled procedure', show('(f 1)'), '(compiled 1)');
  assert(logger, 'and so does every holder of it, which has the one object',
    show('(list ((car kept) 2) (eq? f (car kept)))'), '((compiled 2) #t)');
  assert(logger, 'in a tail call', show('(call-f 3)'), '(compiled 3)');
  assert(logger, 'through apply', show('(apply f (list 4))'), '(compiled 4)');
  assert(logger, 'JavaScript calling it calls the compiled procedure', closure(5), compiled(5));
  assert(logger, 'which the closure, interpreted, would not have answered',
    String(closure(5)) === String(compiled(5)) && !String(closure(5)).includes('interpreted'), true);
  assert(logger, 'compiled code calls the compiled code itself',
    closure[SCHEME_RAW_CALL] === compiled[SCHEME_RAW_CALL], true);
  assert(logger, 'callSchemeProcedure runs it compiled',
    writeString(callSchemeProcedure(closure, [6n])), '(compiled 6)');
  assert(logger, 'it answers as a compiled procedure, not a closure to enter',
    [isSchemeClosure(closure), closure.$compiled === true], [false, true]);
  assert(logger, 'and prints as it did', show('f'), printed);
  assert(logger, 'the compiler counts it compiled, not a closure to compile', offered(), false);

  runInterpreted(closure);
  assert(logger, 'run interpreted again, every holder runs its body',
    show('(list (f 7) ((car kept) 8) (call-f 9))'), '((interpreted 7) (interpreted 8) (interpreted 9))');
  assert(logger, 'and its entries are its own again',
    [closure[SCHEME_RAW_CALL] === interpretedRaw, isSchemeClosure(closure), closure.$compiled === true,
      writeString(callSchemeProcedure(closure, [10n]))],
    [true, true, false, '(interpreted 10)']);
  assert(logger, 'and the compiler counts it a closure to compile', offered(), true);

  logger.title('A closure run as what the compiler made of it');
  run('(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))');
  run('(define (depth n) (if (= n 0) 0 (+ 1 (depth (- n 1)))))');
  for (const name of ['fact', 'depth']) {
    const outcome = tryCompileClosure(env.lookup(name), name);
    assert(logger, `setup: ${name} compiles`, outcome.compiled, true);
    runCompiled(env.lookup(name), outcome.procedure);
  }
  assert(logger, 'it recurses through its name, which holds the closure, compiled',
    [show('(fact 20)'), env.lookup('fact').$compiled === true], ['2432902008176640000', true]);
  assert(logger, 'deep enough that its frames move to the heap and are resumed', show('(depth 100000)'), '100000');
}
