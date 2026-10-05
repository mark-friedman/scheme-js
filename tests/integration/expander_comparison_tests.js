/**
 * @fileoverview The JavaScript analyzer and the Scheme expander,
 * `(scheme-js expander)`, compared while both are here: shipped libraries
 * loaded from their source, and programs that use every kind of form and
 * macro, each top-level form expanded by both and each `syntax-rules` macro's
 * use transformed by both (harness/expander_comparison.js).
 *
 * `npm run test:expanders` compares them on everything the suite analyzes;
 * this keeps a smaller comparison in the suite, in Node and the browser.
 *
 * JavaScript tests, since what is compared is the JavaScript analyzer with the
 * Scheme that replaces it.
 */

import { assert } from '../harness/helpers.js';
import { installExpanderComparison, comparisonReport } from '../harness/expander_comparison.js';
import { parse } from '../../src/core/interpreter/reader.js';
import { analyze } from '../../src/core/interpreter/analyzer.js';
import { createInterpreter } from '../../src/core/interpreter/index.js';
import { withPrivateLibraries } from '../../src/core/interpreter/library_registry.js';
import { loadLibrarySync, applyImports, programEnvironment, runProgramForm } from '../../src/core/interpreter/library_loader.js';
import { BUNDLED_SOURCES } from '../../src/packaging/bundled_libraries.js';

/** Libraries loaded from source, every form of each compared. */
const LIBRARIES = [['scheme', 'base'], ['scheme', 'char'], ['scheme', 'case-lambda'], ['scheme', 'lazy'],
  ['srfi', '1'], ['srfi', '125'], ['srfi', '152'], ['scheme-js', 'reader']];

/** Programs, each run from start to end: the first in an environment of its imports, the second where (scheme base) was imported. */
const PROGRAMS = [
  `(import (scheme base) (scheme write) (scheme char))
   (define-syntax swap!
     (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
   (define-syntax my-or
     (syntax-rules () ((_) #f) ((_ e) e) ((_ e r ...) (let ((t e)) (if t t (my-or r ...))))))
   (define-syntax for
     (syntax-rules (in from to)
       ((_ x in lst body ...) (for-each (lambda (x) body ...) lst))
       ((_ x from a to b body ...) (do ((x a (+ x 1))) ((> x b)) body ...))))
   (define-syntax tabulate
     (syntax-rules ::: () ((_ (k v :::) :::) '((k . (v :::)) :::))))
   (define-syntax vec (syntax-rules () ((_ #(a ...)) (list a ...))))
   (define-syntax escaped (syntax-rules () ((_ x) '(x (... ...)))))
   (define (f x . rest) (let loop ((i 0) (acc '())) (if (< i x) (loop (+ i 1) (cons i acc)) (append acc rest))))
   (define-record-type point (make-point x y) point? (x point-x set-point-x!) (y point-y))
   (define (g p)
     (define (inner q) (* q 2))
     (define-syntax twice (syntax-rules () ((_ e) (begin e e))))
     (twice (set-point-x! p (inner (point-x p))))
     (point-x p))
   (let ((tmp 1) (other 2) (t 5))
     (swap! tmp other)
     (list tmp other (my-or #f t) (g (make-point 3 4)) (f 3 'a 'b)))
   (let-syntax ((foo (syntax-rules () ((_ e) (+ e 1)))))
     (let ((+ *)) (foo 3)))
   (letrec-syntax ((ev? (syntax-rules () ((_ n) (if (= n 0) #t (od? (- n 1))))))
                   (od? (syntax-rules () ((_ n) (if (= n 0) #f #t)))))
     (ev? 2))
   (for x in '(1 2 3) (display x))
   (for i from 1 to 3 (display i))
   (list (tabulate (a 1 2) (b 3)) (vec #(1 2)) (escaped 7))
   (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) => (lambda (x) (list 'composite x))) (else 'other))
   (cond ((assv 'b '((a 1) (b 2))) => cadr) (else 'none))
   (guard (e ((symbol? e) (list 'caught e)) ((string? e) => string-length)) (raise 'oops))
   (let*-values (((a b) (values 1 2)) ((c) (values (+ a b)))) (list a b c))
   (define-values (q r) (floor/ 17 5))
   (parameterize () (list q r))
   \`(1 ,(+ 1 1) ,@(list 3 4) #(5 ,(* 2 3)) \`(nested ,(a ,(+ 1 2))))
   (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
            (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
            (k 10))
     (list (even? k) (odd? k)))
   (when (> 2 1) 'yes)
   (unless (> 2 1) 'no)
   (string-map char-upcase "abc")`,
  `(define-macro (unless-zero n . body) (list 'if (list 'zero? n) #f (cons 'begin body)))
   (define (h n) (unless-zero n (* n 10)))
   (define v (vector 1 2 3))
   (list (h 0) (h 3) (vector-ref v 1) '(a #(b c) . d))
   (define-syntax top-level-defined (syntax-rules () ((_) 'macro)))
   (define top-level-defined 1)
   top-level-defined`
];

/**
 * Runs the tests.
 * @param {Object} logger - Test logger.
 */
export function runExpanderComparisonTests(logger) {
  logger.title('The JavaScript analyzer and the Scheme expander, compared');

  const bundled = (name) => BUNDLED_SOURCES[`${name[name.length - 1]}.sld`] ?? BUNDLED_SOURCES[name[name.length - 1]];
  const record = installExpanderComparison();
  try {
    withPrivateLibraries({ resolver: bundled }, () => {
      const { interpreter, env } = createInterpreter();
      for (const name of LIBRARIES) loadLibrarySync(name, analyze, interpreter, env);
      // A program with no imports sees what its environment has: here, (scheme base).
      applyImports(env, loadLibrarySync(['scheme', 'base'], analyze, interpreter, env));
      for (const source of PROGRAMS) {
        const program = programEnvironment(parse(source), analyze, interpreter, env);
        for (const form of program.forms) runProgramForm(form, analyze, interpreter, program.env, { jsAutoConvert: 'raw' });
      }
    });
  } finally {
    record.uninstall();
  }

  if (record.count > 0) logger.log(comparisonReport(record));
  assert(logger, 'they agree on every form, and every macro use', record.count, 0);
  assert(logger, 'which were many forms', record.forms > 400, true);
  assert(logger, 'and many macro uses', record.macroUses > 1000, true);
}
