/**
 * @fileoverview Which procedures that capture continuations stay compiled.
 *
 * A procedure that captures a continuation, or reaches one that does, is
 * compiled like any other: nearly every capture in real code is an escape,
 * taken now and then, and compiled code is several times faster on those
 * (`benchmarks/run_escapes.js`). What costs more compiled than interpreted is
 * a continuation re-entered over and over -- backtracking, as `btsearch` does
 * -- since every re-entry resumes each compiled frame in it through its
 * resumable form. So a procedure whose frames are resumed far more often than
 * they are saved is switched back to the interpreted closure it was compiled
 * from, for good, as the program runs (`noteResume` in
 * `src/core/interpreter/unwind.js`).
 */

import { assert } from '../harness/helpers.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { DefineNode } from '../../src/core/interpreter/ast_nodes.js';
import { writeString } from '../../src/core/primitives/io/printer.js';
import { settle } from '../../src/compiler/runtime.js';
import { attachTier } from '../../src/compiler/tiering.js';
import { compileProgram } from '../../src/compiler/index.js';
import { isCompiledOver } from '../../src/core/interpreter/library_registry.js';
import { interpretedLibrary, installStandardLibrary } from '../harness/standard_library.js';

/**
 * A program's interpreter with the tier attached.
 * @returns {{interpreter: Object, env: Object, run: Function, compiled: Function, interpreted: Function}}
 */
function tiered() {
  const { interpreter, env } = interpretedLibrary();
  installStandardLibrary(env);
  attachTier(interpreter, env);
  const run = (source) => {
    let value;
    for (const form of parse(source)) value = settle(interpreter.runTopLevel(analyze(form), env, { jsAutoConvert: 'raw' }));
    return writeString(value);
  };
  return {
    interpreter, env, run,
    compiled: (name) => env.lookup(name).$compiled === true,
    interpreted: (name) => env.lookup(name).body !== undefined
  };
}

/**
 * A backtracking search with `amb`: every failure re-enters a continuation
 * saved inside `amb`, and with it the frames beneath it -- `pick-pair`'s and
 * the loop's in `search-all`. `twice` returns before any failure, so no
 * continuation holds its frame.
 * @type {string}
 */
const BACKTRACKING = `
  (define fail-stack '())
  (define (fail)
    (let ((k (car fail-stack)))
      (set! fail-stack (cdr fail-stack))
      (k #f)))
  (define (amb choices)
    (call/cc
      (lambda (return)
        (for-each (lambda (choice)
                    (call/cc (lambda (next)
                               (set! fail-stack (cons next fail-stack))
                               (return choice))))
                  choices)
        (fail))))
  (define (twice x) (let loop ((i 0) (sum 0)) (if (= i 2) sum (loop (+ i 1) (+ sum x)))))
  (define (pick-pair n)
    (let* ((a (amb '(1 2 3 4 5 6 7 8 9)))
           (b (amb '(1 2 3 4 5 6 7 8 9))))
      (if (not (= (+ a (twice b)) n)) (fail))
      (list a b)))
  (define (search-all k)
    (let loop ((i 0) (last #f))
      (if (= i k) last (begin (set! fail-stack '()) (loop (+ i 1) (pick-pair 25))))))`;

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 * @returns {Promise<void>}
 */
export async function runCapturePolicyTests(logger) {
  logger.title('Capture Policy - Procedures That Capture Are Compiled');
  {
    const t = tiered();
    t.run(`(define (find-first pred lst)
             (call/cc (lambda (return)
               (for-each (lambda (x) (if (pred x) (return x))) lst)
               #f)))`);
    assert(logger, 'a procedure that escapes with call/cc is compiled', t.compiled('find-first'), true);
    t.run('(define (first-even lst) (find-first even? lst))');
    t.run('(first-even (list 1 2))');
    t.run('(first-even (list 1 2))');
    assert(logger, 'and so is one that only reaches it', t.compiled('first-even'), true);

    t.run(`(define (count-evens n)
             (let loop ((i 0) (found 0))
               (if (= i n) found (loop (+ i 1) (if (first-even (list 1 i 3)) (+ found 1) found)))))`);
    assert(logger, 'an escape taken 5,000 times answers', t.run('(count-evens 5000)'), '2500');
    assert(logger, 'and leaves the procedures that escape compiled',
      [t.compiled('find-first'), t.compiled('first-even'), t.compiled('count-evens')].join(' '), 'true true true');
  }

  logger.title('Capture Policy - Re-entered Continuations Switch a Procedure Back');
  {
    const t = tiered();
    t.run(BACKTRACKING);
    assert(logger, 'setup: the search is compiled when defined', t.compiled('pick-pair'), true);
    assert(logger, 'the search answers', t.run('(search-all 1)'), '(7 9)');
    assert(logger, 'setup: one search re-enters too little to switch anything', t.compiled('pick-pair'), true);
    assert(logger, 'many searches answer the same', t.run('(search-all 200)'), '(7 9)');
    assert(logger, 'and have switched the procedure whose frames were re-entered back to its closure',
      t.interpreted('pick-pair'), true);
    assert(logger, 'it answers the same interpreted', t.run('(search-all 5)'), '(7 9)');
    t.interpreter.interpretForDebugger(true);
    t.interpreter.interpretForDebugger(false);
    assert(logger, 'and is not compiled again when debugging ends', t.interpreted('pick-pair'), true);
    // `twice` returns before any failure, so no continuation holds its frame.
    assert(logger, 'a procedure no re-entered continuation holds stays compiled', t.compiled('twice'), true);
  }

  logger.title('Capture Policy - Frames Moved to the Heap Are Not Re-entry');
  {
    const t = tiered();
    t.run('(define (deep n) (if (= n 0) 0 (+ 1 (deep (- n 1)))))');
    for (let i = 0; i < 3; i++) t.run('(deep 50000)');
    assert(logger, 'a recursion deep enough to move its frames to the heap, many times over, stays compiled',
      t.compiled('deep'), true);
  }

  logger.title('Capture Policy - compileProgram Compiles Over Closures');
  {
    const { interpreter, env } = interpretedLibrary();
    installStandardLibrary(env);
    const asts = parse(BACKTRACKING).map((form) => analyze(form));
    const outcome = compileProgram(asts.filter((a) => a instanceof DefineNode), env, interpreter);
    assert(logger, 'compileProgram compiles the search', outcome.compiled.includes('pick-pair'), true);
    assert(logger, 'over its closure, so it can be switched back', isCompiledOver(env.lookup('pick-pair')), true);
    const answer = writeString(settle(interpreter.run(analyze(parse('(search-all 200)')[0]), env, [], undefined, { jsAutoConvert: 'raw' })));
    assert(logger, 'and it answers', answer, '(7 9)');
    assert(logger, 'and is switched back as the tier\'s are', env.lookup('pick-pair').body !== undefined, true);
  }
}
