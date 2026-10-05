/**
 * @fileoverview Compiled code recurses as deeply as the interpreter.
 *
 * A compiled procedure's non-tail calls use the JavaScript stack, which holds
 * a few thousand frames; the interpreter keeps its frames on the heap. So past a
 * depth, compiled code moves its frames to the heap: it unwinds them into the
 * interpreter's frame stack, the way a continuation capture does, and the
 * interpreter makes the pending call from there. Before that, the standard
 * library the browser installs compiled failed `map`, `make-list`, `list-copy`
 * and `equal?` on a list of 10,000 elements, which the interpreter handles at
 * 100,000.
 *
 * The differential cases in `compiler_tests.js` cover recursion in compiled
 * user code. These cover the compiled standard library, which those cases do
 * not install, and where the move may happen: only where the unwind reaches the
 * interpreter, never where a JavaScript caller would receive it.
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/expand.js';
import { tryCompileDefinition } from '../../src/compiler/index.js';
import { invoke, settle, stack, SCHEME_PRIMITIVE } from '../../src/compiler/runtime.js';
import { interpretedLibrary, installStandardLibrary } from '../harness/standard_library.js';
import { writeString } from '../../src/core/primitives/io/printer.js';

/**
 * Runs Scheme source and returns the last value, written out.
 * @param {Object} pair - The interpreter and environment.
 * @param {string} source - Scheme source.
 * @returns {string} The last value, or the error it raised.
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
 * Runs the deep recursion tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runDeepRecursionTests(logger) {
  logger.title('Compiler - Compiled Code Recurses as Deeply as the Interpreter');

  const interpreted = interpretedLibrary();
  const compiled = interpretedLibrary();
  const { installed } = installStandardLibrary(compiled.env);
  assert(logger, 'setup: map is compiled', installed.includes('map'), true);

  const big = '(define big (let loop ((i 0) (acc (quote ()))) (if (= i 100000) acc (loop (+ i 1) (cons i acc)))))';
  run(interpreted, big);
  run(compiled, big);
  for (const expr of [
    '(length (make-list 100000 1))',
    '(length (map (lambda (x) (+ x 1)) big))',
    '(length (map + big big))',
    '(length (list-copy big))',
    '(equal? big (list-copy big))',
    '(length (append big (list 1)))',
    '(length (append big big big))',
    '(vector-length (list->vector big))',
    '(length (vector->list (make-vector 100000 0)))',
    '(let ((n 0)) (for-each (lambda (x) (set! n (+ n 1))) big) n)'
  ]) {
    const expected = run(interpreted, expr);
    assert(logger, `setup: the interpreted library answers ${expr}`, expected.startsWith('error:'), false);
    assert(logger, `the compiled library agrees on ${expr}`, run(compiled, expr), expected);
  }
  assert(logger, "append's error is still append's",
    run(compiled, "(guard (e ((error-object? e) (error-object-message e))) (append '(1 . 2) '(3)))"),
    run(interpreted, "(guard (e ((error-object? e) (error-object-message e))) (append '(1 . 2) '(3)))"));

  // Every call from compiled code into an interpreted procedure starts a
  // nested run with a copy of the interpreter's frame stack, so moved frames
  // must not make that stack deep: they are held a move to a frame, and moves
  // link. Here the stack is read from the bottom of a recursion 100,000 deep.
  compiled.env.define('frames-beneath', () => compiled.interpreter.getParentContext().length);
  compile('(define (dig n) (if (= n 0) (frames-beneath) (let ((d (dig (- n 1)))) d)))', compiled.env);
  const beneath = Number(run(compiled, '(dig 100000)'));
  assert(logger, 'setup: the recursion reached the bottom', Number.isInteger(beneath), true);
  assert(logger, 'frames moved to the heap leave the frame stack shallow', beneath < 20, true);

  // The move needs the interpreter to receive the unwind. A JavaScript caller
  // holding Scheme values, which calls a compiled procedure's raw entry -- a
  // primitive calling a procedure back, `js-invoke`, a test calling the raw
  // entry -- would take the unwind signal for a value, so beneath one compiled
  // code never moves its frames. JavaScript calling a compiled procedure as a
  // plain function goes through the procedure's JavaScript-facing entry, which
  // runs it on an interpreter that receives the unwind, so there it may.
  const { env } = compiled;
  // A primitive, so that compiled code calls it directly and it sees what the
  // compiled frame sees: a JavaScript function is called as the interpreter
  // calls one, with moves suspended while it runs.
  const flushDepth = () => (stack.flushable ? 'may' : 'never');
  flushDepth[SCHEME_PRIMITIVE] = true;
  env.define('flush-depth', flushDepth);
  // A plain JavaScript function, as host code would be: it calls back what it
  // is given.
  env.define('call-back', (f) => f());
  // Not in tail position: a tail call to plain JavaScript goes to the
  // trampoline, and so is made by the interpreter.
  compile('(define (depth-here) (let ((depth (flush-depth))) depth))', env);
  env.define('holder', { run: env.lookup('depth-here') });
  compile('(define (depth-in-method) (js-invoke holder "run"))', env);
  assert(logger, 'called from the interpreter, compiled code may move its frames',
    run(compiled, '(depth-here)'), '"may"');
  assert(logger, 'called back by JavaScript the interpreter called, it may, since its plain call runs it on an interpreter',
    run(compiled, '(call-back depth-here)'), '"may"');
  assert(logger, 'nor as a method js-invoke calls from compiled code',
    run(compiled, '(depth-in-method)'), '"never"');
  env.define('js-holder', { run: (f) => f() });
  compile('(define (depth-in-js-method) (js-invoke js-holder "run" depth-here))', env);
  assert(logger, 'and so beneath a JavaScript method js-invoke calls from compiled code, which calls it',
    run(compiled, '(depth-in-js-method)'), '"may"');
  // An interpreted procedure JavaScript calls runs in an interpreter of its
  // own, which can receive the unwind.
  assert(logger, 'but beneath an interpreted procedure JavaScript called, it may again',
    run(compiled, '(call-back (lambda () (depth-here)))'), '"may"');
  // An interpreter run gives back what it found however it ends. Here compiled
  // code throws out of a run that JavaScript started, the JavaScript catches it,
  // and reads what the run gave back: what the interpreter set while it called
  // the JavaScript.
  env.define('flush-after-error', (f) => {
    try { f(); } catch (e) { /* the error is the point */ }
    return stack.flushable ? 'may' : 'never';
  });
  compile('(define (throws) (vector-ref (vector) 0))', env);
  assert(logger, 'after an error thrown out of an interpreter JavaScript called, it may not',
    run(compiled, '(flush-after-error (lambda () (throws)))'), '"never"');
  assert(logger, 'and outside any run of the interpreter, it may not',
    settle(invoke(env.lookup('depth-here'), [])), 'never');
  assert(logger, 'which is where every run leaves it', stack.flushable, false);

  // Recursion alternating between compiled and interpreted code. Each call from
  // compiled code into an interpreted procedure starts a nested run of the
  // interpreter on the JavaScript stack, so a move of frames to the heap has to
  // pass through the nested runs to the outermost one: each adds its own frames
  // to the move on the way. This overflowed at about 575 levels.
  logger.title('Compiler - Recursion Alternating Between Compiled and Interpreted Code');
  const alternating = (pair) => {
    run(pair, '(define (i-step n) (if (= n 0) 0 (c-step i-step n)))');
    run(pair, '(define (tree n) (if (= n 0) (quote ()) (list (tree (- n 1)))))');
    run(pair, '(define (depth t) (if (pair? t) (+ 1 (apply max (map depth t))) 0))');
  };
  const reference = interpretedLibrary();
  alternating(reference);
  run(reference, '(define (c-step g n) (+ 1 (g (- n 1))))');
  const mixed = interpretedLibrary();
  installStandardLibrary(mixed.env);
  alternating(mixed);
  compile('(define (c-step g n) (+ 1 (g (- n 1))))', mixed.env);
  for (const expr of [
    '(i-step 100000)',
    // An interpreted tree walk through the compiled `map`.
    '(depth (tree 20000))',
    // A capture 30,000 levels down, resumed twice: the continuation holds
    // compiled frames, interpreted frames and moved frames alike.
    '(let ((k #f) (n 0)) (define (grab-at m) (if (= m 0) (call/cc (lambda (c) (set! k c) 0)) (c-step grab-at m)))'
      + ' (let ((v (grab-at 30000))) (set! n (+ n 1)) (if (< n 3) (k (* 10 n)) (list v n))))'
  ]) {
    const expected = run(reference, expr);
    assert(logger, `setup: the interpreter answers ${expr.slice(0, 40)}`, expected.startsWith('error:'), false);
    assert(logger, `alternating recursion agrees: ${expr.slice(0, 40)}`, run(mixed, expr), expected);
  }
  // Moved frames, nested runs' included, are one interpreter frame a move, so
  // the frame stack stays shallow however deep the alternation goes. Between
  // moves it grows by a sentinel a level, since each nested run starts on its
  // parent's stack and a sentinel of its own; a move leaves the sentinels
  // behind. So the bound is the distance between moves, about 250 levels of
  // this procedure, not the depth.
  mixed.env.define('frames-beneath', () => mixed.interpreter.getParentContext().length);
  run(mixed, '(define (i-dig n) (if (= n 0) (frames-beneath) (c-step i-dig n)))');
  // `c-step` adds one a level, so the depth comes off the answer.
  const alternatingBeneath = Number(run(mixed, '(- (i-dig 100000) 100000)'));
  assert(logger, 'setup: the alternating recursion reached the bottom', Number.isInteger(alternatingBeneath), true);
  assert(logger, 'frames moved through nested runs leave the frame stack shallow', alternatingBeneath < 400, true);
}
