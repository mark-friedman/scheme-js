;; test-equal.scm -- Chibi's comparison of a test's value, for Chibi's tests.
;;
;; Run after the project's harness (tests/core/scheme/test.scm) and before the
;; sections of Chibi's R7RS tests, by compliance_suite.js. Chibi's `test`
;; passes a value that is `equal?` to the one expected, or, where the one
;; expected is inexact, a value within an epsilon of it, part by part for a
;; complex number (`test-equal?` in chibi_original/test.scm): its expected
;; values are written to 15 digits, (test 1.4142135623731 (sqrt 2)), and
;; JavaScript's Math gives more. The project's own tests compare exactly; only
;; Chibi's are compared as Chibi compares them, so that they can be run as
;; Chibi wrote them. Also Chibi's `test-values`, which the harness has not.

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
