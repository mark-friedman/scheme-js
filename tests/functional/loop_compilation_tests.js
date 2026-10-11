/**
 * @fileoverview Tail calls to the procedure itself compile to JavaScript loops.
 *
 * A tail call normally returns a `TailCall` to the trampoline, which allocates
 * and round-trips on every iteration of every loop. When the callee is known to
 * be the procedure already running, the call can instead reassign the
 * parameters and jump back to the top.
 *
 * Which calls qualify is decided in Scheme, by the lowering pass
 * (`src/compiler/ir.scm`), which tags them on the IR; the emitter only reads
 * the tag. What the lowering tags is tested in Scheme, in
 * `tests/compiler/loop_tests.scm`. These check the other half, which needs the
 * generated code in hand: what it does with the tag, and that a redefined
 * global stops being looped to. That the looped code gives the same answers
 * as the interpreter is checked by the differential cases in
 * `compiler_tests.js`, including captures inside a loop.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { tryCompileDefinition } from '../../src/compiler/index.js';
import { settle } from '../../src/compiler/runtime.js';

/**
 * Compiles one definition into a fresh environment.
 * @param {string} source - One `define` form.
 * @param {Object} env - The environment to compile against and define into.
 * @returns {{procedure: Function, source: string}} The compiled procedure and
 *   its generated JavaScript.
 */
function compile(source, env) {
  const result = tryCompileDefinition(analyze(parse(source)[0]), env);
  if (!result.compiled) throw new Error(`did not compile: ${result.reason}`);
  env.define(result.name, result.procedure);
  return result;
}

/**
 * Runs Scheme source in an environment and returns the last value.
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
 * Runs the loop-compilation tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runLoopCompilationTests(logger) {
  logger.title('Compiler - Tail Calls to the Procedure Itself Are Loops');

  // --- What the emitter does with the tag -------------------------------------

  {
    const { env } = createInterpreter();
    const { source } = compile(
      '(define (sum n) (let loop ((i 0) (acc 0)) (if (> i n) acc (loop (+ i 1) (+ acc i)))))', env);
    assert(logger, 'an inlined loop allocates no TailCall', /new \$TailCall/.test(source), false);
    assert(logger, 'and makes no closure', source.includes('$mk'), false);
    assert(logger, 'it is a labelled loop in the fast form', /continue \$loop\d+;/.test(source), true);
    // Its head is a block of its own; block zero is the procedure's entry,
    // which the loop must not re-run.
    assert(logger, 'and never jumps back to the procedure entry in the resumable form',
      /\$pc = 0; continue;/.test(source), false);
  }
  {
    // A loop that is not inlined still loops, from inside its own procedure.
    const { env } = createInterpreter();
    const { source } = compile(
      '(define (f n) (+ 1 (let loop ((i 0)) (if (< i n) (loop (+ i 1)) i))))', env);
    const loopFactory = source.split('function $proc(')[0];
    assert(logger, 'a loop that is not inlined allocates no TailCall per iteration',
      /new \$TailCall/.test(loopFactory), false);
    assert(logger, 'it jumps back to the top of its own fast form', source.includes('continue $loop;'), true);
    assert(logger, 'and of its own resumable form', /\$pc = 0; continue;/.test(source), true);
  }

  // A tail call from an inlined loop to the loop it was entered from is a
  // jump to that loop's head, through the label of the loop it names.
  {
    const { interpreter, env } = createInterpreter();
    const { source } = compile(`(define (pairs n)
      (let outer ((i 0) (count 0))
        (if (< i n)
            (let inner ((j 0) (count count))
              (if (< j i) (inner (+ j 1) (+ count 1)) (outer (+ i 1) count)))
            count)))`, env);
    const fast = source.split('const $proc$r')[0];
    assert(logger, 'nested loops allocate no TailCall', /new \$TailCall|\$tailCall\(/.test(fast), false);
    assert(logger, 'and make no closure', source.includes('$mk'), false);
    assert(logger, 'the inner loop jumps to the outer loop\'s head by its label',
      /\$loop2: for \(;;\) \{[^]*continue \$loop1;/.test(fast), true);
    assert(logger, 'and gives the interpreter\'s answer', run(interpreter, env, '(pairs 30)'), 435);
  }
  {
    // Two loops out, and to the top-level procedure itself, whose jump is
    // guarded on its global as any is.
    const { interpreter, env } = createInterpreter();
    const { source } = compile(`(define (triples n)
      (let a ((i 0) (count 0))
        (if (< i n)
            (let b ((j 0) (count count))
              (if (< j i)
                  (let c ((k 0) (count count))
                    (if (< k j) (c (+ k 1) (+ count 1)) (b (+ j 1) count)))
                  (a (+ i 1) count)))
            (if (> count 1000) (triples (- n 1)) count))))`, env);
    const fast = source.split('const $proc$r')[0];
    assert(logger, 'the innermost of three loops jumps to the middle one\'s head',
      /\$loop3: for \(;;\) \{[^]*continue \$loop2;/.test(fast), true);
    assert(logger, 'and the middle one to the outer one\'s', /\$loop2: for \(;;\) \{[^]*continue \$loop1;/.test(fast), true);
    assert(logger, 'and the outer one to the procedure\'s, guarded on its global', /=== \$proc\$js\)/.test(fast), true);
    assert(logger, 'and give the interpreter\'s answer', run(interpreter, env, '(triples 12)'), 220);
    assert(logger, 'looping to the procedure too', run(interpreter, env, '(triples 30)'), 969);
  }

  // A global can be redefined after this code was compiled, and then the call
  // is no longer to itself. The loop is guarded on the binding for that reason,
  // and falls back to an ordinary tail call.
  {
    const { interpreter, env } = createInterpreter();
    const { source } = compile("(define (count-down n) (if (= n 0) 'done (count-down (- n 1))))", env);
    assert(logger, 'a global self-loop is guarded on the binding', /=== \$proc\$js\)/.test(source), true);
    assert(logger, 'it loops while the binding is unchanged',
      run(interpreter, env, '(count-down 100000)').name, 'done');

    run(interpreter, env, "(define old count-down) (define (count-down n) 'redefined)");
    assert(logger, 'once the global is redefined, the tail call goes to the new definition',
      run(interpreter, env, '(old 5)').name, 'redefined');
    assert(logger, 'and a call that ends before recursing is unaffected',
      run(interpreter, env, '(old 0)').name, 'done');
  }
}
