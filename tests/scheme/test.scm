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
  (if (> *test-failures* 0)
      #f
      #t))

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
      (set! *test-passes* (+ *test-passes* 1))
      (set! *test-failures* (+ *test-failures* 1)))
  ;; Call native reporter (always defined in boot.scm)
  (native-report-test-result name passed expected actual))

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
       ;; (display "=== ") (display name) (display " ===") (newline)
       body ...
       ;; (newline)
       ))))
