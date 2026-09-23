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
 * the tag. So these tests check both halves: what the lowering tags, and what
 * the generated code does with it. That the looped code gives the same answers
 * as the interpreter is checked by the differential cases in
 * `compiler_tests.js`, including captures inside a loop.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { tryCompileDefinition } from '../../src/compiler/index.js';
import { lowerLambda } from '../../src/compiler/lowering.js';
import { settle } from '../../src/compiler/runtime.js';

/**
 * The loop tags on every call in a definition's IR, in walk order.
 * @param {string} source - One `define` form.
 * @returns {Array<string>|string} Each tag, or the reason lowering failed.
 */
function loopTags(source) {
  const ast = analyze(parse(source)[0]);
  const lowered = lowerLambda(ast.valueExpr ?? ast.value);
  if (lowered.reason) return `failed: ${lowered.reason}`;
  const tags = [];
  const walk = (node) => {
    if (node === null || typeof node !== 'object') return;
    if (Array.isArray(node)) { node.forEach(walk); return; }
    if (node.k === 'call' && node.loop) tags.push(node.loop);
    for (const key of Object.keys(node)) walk(node[key]);
  };
  walk(lowered.ir);
  return tags;
}

/**
 * How many `letrec` groups in a definition's IR the lowering marked to be
 * emitted inline, as a loop in the enclosing procedure.
 * @param {string} source - One `define` form.
 * @returns {number|string} The count, or the reason lowering failed.
 */
function inlinedLoops(source) {
  const ast = analyze(parse(source)[0]);
  const lowered = lowerLambda(ast.valueExpr ?? ast.value);
  if (lowered.reason) return `failed: ${lowered.reason}`;
  let count = 0;
  const walk = (node) => {
    if (node === null || typeof node !== 'object') return;
    if (Array.isArray(node)) { node.forEach(walk); return; }
    if (node.k === 'letrec' && node.inline) count++;
    for (const key of Object.keys(node)) walk(node[key]);
  };
  walk(lowered.ir);
  return count;
}

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

  // --- What the lowering tags ------------------------------------------------

  assert(logger, 'a top-level procedure calling itself in tail position',
    loopTags("(define (f n) (if (= n 0) 'done (f (- n 1))))"), ['global']);
  assert(logger, 'a named let',
    loopTags('(define (sum n) (let loop ((i 0) (acc 0)) (if (> i n) acc (loop (+ i 1) (+ acc i)))))'),
    ['local']);
  assert(logger, 'a do loop',
    loopTags("(define (f n) (do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i n) acc)))"),
    ['local']);
  // The call from `f` to `loop` is a tail call too, but not from `loop` to
  // itself, so only the one inside `loop` is tagged.
  assert(logger, 'an internally defined procedure calling itself',
    loopTags('(define (f n) (define (loop i acc) (if (= i 0) acc (loop (- i 1) (+ acc i)))) (loop n 0))'),
    ['local']);

  // Each of these looks like a loop and is not one.
  assert(logger, 'not a call that is not in tail position',
    loopTags('(define (f n) (if (= n 0) 0 (+ 1 (f (- n 1)))))'), []);
  assert(logger, 'not a self-call with the wrong number of arguments',
    loopTags('(define (f n) (if (= n 0) 0 (f n n)))'), []);
  assert(logger, 'not a procedure with a rest parameter',
    loopTags('(define (f . xs) (if (null? xs) 0 (f)))'), []);
  assert(logger, 'not a call to a sibling in a letrec group',
    loopTags('(define (f n) (letrec ((e (lambda (n) (if (= n 0) #t (o (- n 1)))))'
      + ' (o (lambda (n) (if (= n 0) #f (e (- n 1)))))) (e n)))'),
    []);
  // The call is in tail position in the inner lambda, which is a different
  // procedure from `loop`: jumping to the top of the inner one would be wrong.
  assert(logger, 'not a call to the enclosing loop from a nested procedure',
    loopTags('(define (f g) (let loop ((i 0)) (if (< i 3) (g (lambda () (loop (+ i 1)))) i)))'), []);
  assert(logger, 'not a call to the global from a nested procedure',
    loopTags('(define (f g) (g (lambda () (f g))))'), []);
  // A loop name that is ever assigned may not name this procedure when the
  // call happens.
  assert(logger, 'not a loop whose name is assigned',
    loopTags('(define (f) (let loop ((i 0)) (if (< i 3) (begin (set! loop loop) (loop (+ i 1))) i)))'),
    []);

  // --- Loops emitted inline in the procedure that enters them ---------------
  //
  // A loop's own iterations can jump, but entering it still made a closure and
  // returned a `TailCall` -- on every call of a procedure like `assq`, whose
  // lists are usually two long, that entry was nearly the whole cost. A
  // `letrec` whose name is only ever called, entered once in tail position and
  // otherwise only by its own looping calls, can be the enclosing procedure's
  // own loop instead: no closure, no entry call.

  assert(logger, 'a named let in tail position is inlined',
    inlinedLoops('(define (sum n) (let loop ((i 0) (acc 0)) (if (> i n) acc (loop (+ i 1) (+ acc i)))))'), 1);
  assert(logger, 'a do loop is inlined',
    inlinedLoops("(define (f n) (do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i n) acc)))"), 1);
  assert(logger, 'the loop inside an assq is inlined',
    inlinedLoops('(define (my-assq x l) (letrec ((loop (lambda (l) (cond ((null? l) #f)'
      + ' ((eq? x (car (car l))) (car l)) (else (loop (cdr l))))))) (loop l)))'), 1);
  assert(logger, 'a loop entered from inside another inlined loop is inlined too',
    inlinedLoops('(define (f n) (let a ((i 0)) (if (< i n) (a (+ i 1))'
      + ' (let b ((j i)) (if (> j 0) (b (- j 1)) (list i j))))))'), 2);

  assert(logger, 'not a loop whose value is wanted by its caller',
    inlinedLoops('(define (f n) (+ 1 (let loop ((i 0)) (if (< i n) (loop (+ i 1)) i))))'), 0);
  assert(logger, 'not a loop that escapes as a value',
    inlinedLoops('(define (f) (let loop ((i 0)) (if (< i 3) (loop (+ i 1)) loop)))'), 0);
  assert(logger, 'not a loop that recurses rather than iterates',
    inlinedLoops('(define (f n) (let loop ((i n)) (if (= i 0) 0 (+ 1 (loop (- i 1))))))'), 0);
  assert(logger, 'not a loop called from a nested procedure',
    inlinedLoops('(define (f g) (let loop ((i 0)) (if (< i 3) (g (lambda () (loop (+ i 1)))) i)))'), 0);
  assert(logger, 'not a mutually recursive group',
    inlinedLoops('(define (f n) (letrec ((e (lambda (n) (if (= n 0) #t (o (- n 1)))))'
      + ' (o (lambda (n) (if (= n 0) #f (e (- n 1)))))) (e n)))'), 0);

  // --- What the emitter does with the tag -------------------------------------

  {
    const { env } = createInterpreter();
    const { source } = compile(
      '(define (sum n) (let loop ((i 0) (acc 0)) (if (> i n) acc (loop (+ i 1) (+ acc i)))))', env);
    assert(logger, 'an inlined loop allocates no TailCall', /new R\.TailCall/.test(source), false);
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
      /new R\.TailCall/.test(loopFactory), false);
    assert(logger, 'it jumps back to the top of its own fast form', source.includes('continue $loop;'), true);
    assert(logger, 'and of its own resumable form', /\$pc = 0; continue;/.test(source), true);
  }

  // A global can be redefined after this code was compiled, and then the call
  // is no longer to itself. The loop is guarded on the binding for that reason,
  // and falls back to an ordinary tail call.
  {
    const { interpreter, env } = createInterpreter();
    const { source } = compile("(define (count-down n) (if (= n 0) 'done (count-down (- n 1))))", env);
    assert(logger, 'a global self-loop is guarded on the binding', /=== \$proc\)/.test(source), true);
    assert(logger, 'it loops while the binding is unchanged',
      run(interpreter, env, '(count-down 100000)').name, 'done');

    run(interpreter, env, "(define old count-down) (define (count-down n) 'redefined)");
    assert(logger, 'once the global is redefined, the tail call goes to the new definition',
      run(interpreter, env, '(old 5)').name, 'redefined');
    assert(logger, 'and a call that ends before recursing is unaffected',
      run(interpreter, env, '(old 0)').name, 'done');
  }
}
