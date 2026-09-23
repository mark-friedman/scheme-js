/**
 * @fileoverview Proper tail recursion for the last expression of a sequence.
 *
 * R7RS 3.5 puts the last expression of a `begin`, of a procedure body, and of
 * the forms that expand to them (`when`, `unless`, `cond` clauses, `do`) in
 * tail position, so a loop through any of them must run in constant space.
 * These tests measure the deepest the interpreter's frame stack gets while a
 * loop runs, at two iteration counts: in constant space the two are equal.
 */

import { assert, run } from '../harness/helpers.js';
import { instrumentInterpreter } from '../../src/debug/instrumentation.js';

/**
 * Runs the tail-position tests.
 * @param {Object} interpreter - The interpreter instance.
 * @param {Object} logger - Test logger.
 */
export async function runTailPositionTests(interpreter, logger) {
  logger.title('Tail Position in Sequences');

  /**
   * Measures the deepest the frame stack gets while running `code`. A nested
   * run -- JavaScript calling back into Scheme -- starts from a copy of the
   * stack beneath it, so growth there shows up too.
   * @param {string} code - Scheme source to evaluate.
   * @returns {{depth: number, result: *}} Max frame depth and the result.
   */
  const measure = (code) => {
    const probe = instrumentInterpreter(interpreter);
    let result;
    let stats;
    try {
      result = run(interpreter, code);
    } finally {
      stats = probe.stop();
    }
    return { depth: stats.maxStackDepth, result };
  };

  /**
   * Asserts that a loop runs in constant space: `makeCode(n)` reaches the
   * same frame depth at 50 iterations as at 800, and returns `expected`.
   * @param {string} label - The shape being tested.
   * @param {function(number): string} makeCode - Scheme source for n iterations.
   * @param {*} expected - The loop's result.
   */
  const assertConstantSpace = (label, makeCode, expected) => {
    const small = measure(makeCode(50));
    const large = measure(makeCode(800));
    assert(logger, `${label}: result`, large.result, expected);
    assert(logger, `${label}: frame depth does not grow with iterations`,
      large.depth, small.depth);
  };

  run(interpreter, `
    (define (tail-body-loop i n)
      (car '(1))
      (if (< i n) (tail-body-loop (+ i 1) n) 'done))
    (define (tail-three-body-loop i n)
      (car '(1))
      (car '(2))
      (if (< i n) (tail-three-body-loop (+ i 1) n) 'done))
    (define (tail-when-loop i n)
      (if (< i n)
          (when #t (car '(1)) (tail-when-loop (+ i 1) n))
          'done))
    (define (tail-unless-loop i n)
      (if (< i n)
          (unless #f (car '(1)) (tail-unless-loop (+ i 1) n))
          'done))
    (define (tail-cond-loop i n)
      (cond ((< i n) (car '(1)) (tail-cond-loop (+ i 1) n))
            (else 'done)))
    (define tail-identity (lambda (v) v))
    (define tail-callback (js-eval "(g) => g(1)"))
  `);

  assertConstantSpace('begin in a named let',
    (n) => `(let lp ((i 0)) (if (< i ${n}) (begin (car '(1)) (lp (+ i 1))) 'done))`,
    'done');
  assertConstantSpace('three-expression begin',
    (n) => `(let lp ((i 0)) (if (< i ${n}) (begin (car '(1)) (car '(2)) (lp (+ i 1))) 'done))`,
    'done');
  assertConstantSpace('two-expression procedure body',
    (n) => `(tail-body-loop 0 ${n})`, 'done');
  assertConstantSpace('three-expression procedure body',
    (n) => `(tail-three-body-loop 0 ${n})`, 'done');
  assertConstantSpace('when', (n) => `(tail-when-loop 0 ${n})`, 'done');
  assertConstantSpace('unless', (n) => `(tail-unless-loop 0 ${n})`, 'done');
  assertConstantSpace('cond clause with several expressions',
    (n) => `(tail-cond-loop 0 ${n})`, 'done');
  assertConstantSpace('do with a body',
    (n) => `(do ((i 0 (+ i 1))) ((= i ${n}) 'done) (car '(1)))`, 'done');
  assertConstantSpace('let body with several expressions',
    (n) => `(let lp ((i 0)) (let ((x 1)) (car '(1)) (if (< i ${n}) (lp (+ i 1)) 'done)))`,
    'done');
  assertConstantSpace('begin around a JavaScript callback that re-enters Scheme',
    (n) => `(let lp ((i 0)) (if (< i ${n}) (begin (tail-callback tail-identity) (lp (+ i 1))) 'done))`,
    'done');

  // A value that is not the last expression is not in tail position, so
  // genuine recursion through it must still grow the stack. This guards
  // against tail position being granted too eagerly.
  run(interpreter, `
    (define (non-tail-begin n)
      (if (= n 0)
          0
          (begin (car '(1)) (+ 1 (non-tail-begin (- n 1))))))
  `);
  const shallow = measure('(non-tail-begin 20)');
  const deep = measure('(non-tail-begin 40)');
  assert(logger, 'non-tail recursion through begin: result', deep.result, 40n);
  assert(logger, 'non-tail recursion through begin still grows the stack',
    deep.depth > shallow.depth, true);

  // A sequence's value is its last expression's, including when that
  // expression is reached without a frame beneath it.
  assert(logger, 'begin returns its last value', run(interpreter, "(begin 1 2 'last)"), 'last');
  assert(logger, 'body returns its last value',
    run(interpreter, "((lambda () 1 2 'last))"), 'last');
}

export default runTailPositionTests;
