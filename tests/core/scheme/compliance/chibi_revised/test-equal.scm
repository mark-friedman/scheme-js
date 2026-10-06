;; test-equal.scm -- Chibi's comparison of a test's value, and the test forms
;; Chibi's tests use that the project's harness has not, for Chibi's tests.
;;
;; Run after the project's harness (tests/core/scheme/test.scm) and before the
;; sections of Chibi's R7RS tests, by compliance_suite.js. Chibi's `test`
;; passes a value that is `equal?` to the one expected, or, where the one
;; expected is inexact, a value within an epsilon of it, part by part for a
;; complex number (`test-equal?` in chibi_original/test.scm): its expected
;; values are written to 15 digits, (test 1.4142135623731 (sqrt 2)), and
;; JavaScript's Math gives more. The project's own tests compare exactly; only
;; Chibi's are compared as Chibi compares them, so that they can be run as
;; Chibi wrote them. Also Chibi's `test-values`, `test-assert`, `test-not` and
;; `test-error`, which the harness has not, or has with other arguments.

;; /**
;;  * The relative difference Chibi allows, its `current-test-epsilon`.
;;  */
(define chibi-test-epsilon 1e-5)

;; /**
;;  * Whether two real numbers differ by at most the epsilon, relative to the
;;  * larger in magnitude, or absolutely where that is zero.
;;  * @param {number} a - One.
;;  * @param {number} b - The other.
;;  * @returns {boolean}
;;  */
(define (chibi-approx-equal? a b)
  (if (> (abs a) (abs b))
      (chibi-approx-equal? b a)
      (if (zero? b)
          (< (abs a) chibi-test-epsilon)
          (< (abs (/ (- a b) b)) chibi-test-epsilon))))

;; /**
;;  * Chibi's `test-equal?`: `equal?`, or an inexact real expected and a real
;;  * within the epsilon of it, or complex numbers whose parts are so.
;;  * @param {*} expected - The value expected.
;;  * @param {*} actual - The value computed.
;;  * @returns {boolean}
;;  */
(define (chibi-test-equal? expected actual)
  (or (equal? expected actual)
      (and (number? expected) (number? actual)
           (if (real? expected)
               (and (inexact? expected) (real? actual)
                    (not (nan? expected)) (not (nan? actual))
                    (chibi-approx-equal? expected actual))
               (and (chibi-test-equal? (real-part expected) (real-part actual))
                    (chibi-test-equal? (imag-part expected) (imag-part actual)))))))

;; The harness's comparison, which every `test` calls, replaced by Chibi's.
(set! assert-equal
  (lambda (msg expected actual)
    (report-test-result msg (chibi-test-equal? expected actual) expected actual)))

;; Chibi's `test-values`: like `test`, but the expression and the expected
;; one may each return several values.
(define-syntax test-values
  (syntax-rules ()
    ((_ expected expr)
     (test (call-with-values (lambda () expected) list)
           (call-with-values (lambda () expr) list)))))

;; Chibi's `test-assert`: the expression is true.
(define-syntax test-assert
  (syntax-rules ()
    ((_ expr) (test-assert 'expr expr))
    ((_ name expr) (test name #t (if expr #t #f)))))

;; Chibi's `test-not`: the expression is false.
(define-syntax test-not
  (syntax-rules ()
    ((_ expr) (test-assert (not expr)))
    ((_ name expr) (test-assert name (not expr)))))

;; Chibi's `test-error`: the expression raises, and what it raises satisfies
;; the predicate, if there is one.
(define-syntax test-error
  (syntax-rules ()
    ((_ expr) (test-error 'expr expr))
    ((_ name expr) (chibi-test-error name (lambda () expr) (lambda (e) #t)))
    ((_ name pred expr) (chibi-test-error name (lambda () expr) pred))))

;; /**
;;  * Runs a test of an expression that should raise.
;;  * @param {*} name - The test's name.
;;  * @param {procedure} thunk - Evaluates the expression.
;;  * @param {procedure} pred - What is raised must satisfy it.
;;  */
(define (chibi-test-error name thunk pred)
  (let ((outcome (guard (e (#t (if (pred e) 'raised 'raised-something-else))) (thunk) 'nothing-raised)))
    (report-test-result name (eq? outcome 'raised) 'raised outcome)))

