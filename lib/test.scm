; Simple Scheme Test Harness

;; /**
;;  * Counter for failed tests.
;;  * @type {number}
;;  */
(define *test-failures* 0)
;; /**
;;  * Counter for passed tests.
;;  * @type {number}
;;  */
(define *test-passes* 0)

;; /**
;;  * Prints a summary of test results.
;;  * Displays total passes and failures.
;;  *
;;  * @returns {boolean} #t if all tests passed, #f otherwise.
;;  */
(define (test-report)
  (display "Test Report:") (newline)
  (display "  Passes: ") (display *test-passes*) (newline)
  (display "  Failures: ") (display *test-failures*) (newline)
  (if (> *test-failures* 0)
      (begin
        (display "SOME TESTS FAILED") (newline)
        #f)
      (begin
        (display "ALL TESTS PASSED") (newline)
        #t)))

;; /**
;;  * Internal helper to report a single test result.
;;  * Updates counters, prints to stdout, and calls the native reporter.
;;  *
;;  * @param {string} name - Name/description of the test.
;;  * @param {boolean} passed - Whether the test passed.
;;  * @param {*} expected - Expected value (for reporting failures).
;;  * @param {*} actual - Actual value (for reporting failures).
;;  */
(define (report-test-result name passed expected actual)
  (if passed
      (begin
        (set! *test-passes* (+ *test-passes* 1))
        (display "✅ PASS: ") (display name) (newline))
      (begin
        (set! *test-failures* (+ *test-failures* 1))
        (display "❌ FAIL: ") (display name) (newline)
        (display "   Expected: ") (display expected) (newline)
        (display "   Got:      ") (display actual) (newline)))
  ;; Call native reporter (always defined in boot.scm)
  (native-report-test-result name passed (if passed "" expected) (if passed "" actual)))

;; /**
;;  * Asserts that two values are equal.
;;  * Uses `equal?` for comparison.
;;  *
;;  * @param {string} msg - Test description.
;;  * @param {*} expected - Expected value.
;;  * @param {*} actual - Actual value.
;;  */
(define (assert-equal msg expected actual)
  (if (equal? expected actual)
      (report-test-result msg #t expected actual)
      (report-test-result msg #f expected actual)))

;; /**
;;  * Test assertion macro.
;;  * Syntactic sugar for `assert-equal`.
;;  *
;;  * @param {string} msg - Test description.
;;  * @param {*} expected - Expected value.
;;  * @param {*} expr - Expression to evaluate and compare.
;;  */
(define-syntax test
  (syntax-rules ()
    ((test msg expected expr)
     (assert-equal msg expected expr))))

;; /**
;;  * Groups related tests together.
;;  * Prints a header before running the body.
;;  *
;;  * @param {string} name - Group name.
;;  * @param {...*} body - Test expressions to execute.
;;  */
(define-syntax test-group
  (syntax-rules ()
    ((test-group name body ...)
     (begin
       (display "=== ") (display name) (display " ===") (newline)
       body ...
       (newline)))))
