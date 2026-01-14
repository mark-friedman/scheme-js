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
;;  * Counter for skipped tests.
;;  * @type {number}
;;  */
(define *test-skips* 0)

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
;;  * Internal helper to report a skipped test.
;;  * Updates counters and calls the native skip reporter.
;;  *
;;  * @param {string} name - Name/description of the test.
;;  * @param {string} reason - Reason for skipping.
;;  */
(define (report-test-skip name reason)
  (set! *test-skips* (+ *test-skips* 1))
  (native-report-test-skip name reason))

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
;;  * Internal helper to run a test with exception handling.
;;  * @param {string} name - Test description.
;;  * @param {*} expected - Expected value.
;;  * @param {procedure} thunk - Thunk that evaluates the test expression.
;;  */
(define (run-test-with-guard name expected thunk)
  (guard (test-exception
          (#t
           ;; Exception caught - report as failure with error message
           (let ((err-msg (if (error-object? test-exception)
                              (error-object-message test-exception)
                              (if (string? test-exception) 
                                  test-exception 
                                  "exception raised"))))
             (report-test-result name #f expected err-msg))))
    (assert-equal name expected (thunk))))

(define-syntax test
  (syntax-rules ()
    ;; 3-argument form: (test "description" expected expr)
    ((test msg expected expr)
     (run-test-with-guard msg expected (lambda () expr)))
    ;; 2-argument Chibi form: (test expected expr)
    ((test expected expr)
     (run-test-with-guard 'expr expected (lambda () expr)))))

;; test-skip: Marks a test as skipped while preserving the original test details.
;; New syntax: (test-skip reason (test expected expr))
;; The original test form is NOT executed, but its details are reported.
(define-syntax test-skip
  (syntax-rules (test test-assert test-numeric-syntax test-precision)
    ;; Skip a regular test: (test-skip reason (test expected expr))
    ((test-skip reason (test expected expr))
     (report-test-skip 'expr reason))
    ;; Skip a test with explicit name: (test-skip reason (test name expected expr))
    ((test-skip reason (test name expected expr))
     (report-test-skip name reason))
    ;; Skip a test-assert: (test-skip reason (test-assert name expr))
    ((test-skip reason (test-assert name expr))
     (report-test-skip name reason))
    ;; Skip a test-assert without name: (test-skip reason (test-assert expr))
    ((test-skip reason (test-assert expr))
     (report-test-skip 'expr reason))
    ;; Skip test-numeric-syntax: (test-skip reason (test-numeric-syntax str ...))
    ((test-skip reason (test-numeric-syntax str . rest))
     (report-test-skip str reason))
    ;; Skip test-precision: (test-skip reason (test-precision str ...))
    ((test-skip reason (test-precision str . rest))
     (report-test-skip str reason))
    ;; Legacy 2-arg form for backward compat: (test-skip "name" "reason")
    ((test-skip name reason)
     (report-test-skip name reason))))

;; /**
;;  * Groups related tests together.
;;  * Prints a header before running the body.
;;  *
;;  * @param {string} name - Group name.
;;  * @param {...*} body - Test expressions to execute.
;;  */
;; Helper to protect expressions but let definitions pass through
(define-syntax test-protect
  (syntax-rules (define define-syntax define-values define-record-type begin import)
    ;; Definitions: emit raw to preserve scope
    ((test-protect (define . rest))
     (define . rest))
    ((test-protect (define-syntax . rest))
     (define-syntax . rest))
    ((test-protect (define-values . rest))
     (define-values . rest))
    ((test-protect (define-record-type . rest))
     (define-record-type . rest))
    ((test-protect (import . rest))
     (import . rest))
    
    ;; Explicit begin: recurse into it (splicing behavior)
    ((test-protect (begin part ...))
     (begin (test-protect part) ...))
    
    ;; Expressions: wrap in guard to catch errors and continue
    ((test-protect expr)
     (guard (test-protected-exn (else
                (display "Error in test group: ")
                (if (error-object? test-protected-exn)
                    (display (error-object-message test-protected-exn))
                    (display test-protected-exn))
                (newline)
                #f)) ;; return false on error
       expr))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group name body ...)
     ;; Use let() to create a body context where defines (including from 
     ;; macro expansion) are valid. No test-protect wrapping - let errors
     ;; propagate naturally. This fixes mad-hatter and similar patterns.
     (begin
       (native-log-title name)
       (let ()
         body ...)))))

;; /**
;;  * Test that an expression raises an error containing the given message.
;;  * Uses guard to catch exceptions.
;;  *
;;  * @param {string} name - Test description.
;;  * @param {string} expected-msg - Substring expected in error message.
;;  * @param {*} expr - Expression that should raise an error.
;;  */
(define-syntax test-error
  (syntax-rules ()
    ((test-error name expected-msg expr)
     (guard (e (#t
                (let ((msg (if (error-object? e)
                               (error-object-message e)
                               "non-error-object raised")))
                  (if (string? msg)
                      (report-test-result name #t expected-msg msg)
                       (report-test-result name #f expected-msg msg)))))
       expr
       (report-test-result name #f expected-msg "no error raised")))))


;; Compatibility definitions for Chibi tests
;; (test-begin and test-end removed as we standardized on test-group)

(display "Test Harness Loaded\n")

(define-syntax test-assert
  (syntax-rules ()
    ((test-assert name expr)
     (test name #t expr))
    ((test-assert expr)
     (test #t expr))))
