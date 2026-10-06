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
;;  * Why the tests now running are expected to fail, or #f if they are not;
;;  * set by `test-expect-fail`.
;;  * @type {parameter}
;;  */
(define expected-failure (make-parameter #f))

;; /**
;;  * A test's name as a string: the name it was given, or the expression it
;;  * tests, written out.
;;  * @param {*} name - The name.
;;  * @returns {string}
;;  */
(define (test-name->string name)
  (if (string? name)
      name
      (let ((port (open-output-string)))
        (display name port)
        (get-output-string port))))

;; /**
;;  * Internal helper to report a single test result.
;;  * Updates counters, prints to stdout, and calls the native reporter.
;;  *
;;  * A test expected to fail is reported as a skip while it fails, and as a
;;  * failure once it passes: the expectation is then wrong, and the change that
;;  * made the test pass has to remove it.
;;  *
;;  * @param {string} name - Name/description of the test.
;;  * @param {boolean} passed - Whether the test passed.
;;  * @param {*} expected - Expected value (for reporting failures).
;;  * @param {*} actual - Actual value (for reporting failures).
;;  */
(define (report-test-result name passed expected actual)
  (let ((reason (expected-failure)))
    (cond ((not reason)
           (if passed
               (set! *test-passes* (+ *test-passes* 1))
               (set! *test-failures* (+ *test-failures* 1)))
           ;; Call native reporter (always defined in boot.scm)
           (native-report-test-result name passed expected actual))
          (passed
           (set! *test-failures* (+ *test-failures* 1))
           (native-report-test-result
            (string-append (test-name->string name)
                           " -- passes, but is marked as expected to fail: " reason)
            #f expected actual))
          (else
           (report-test-skip name (string-append "expected to fail: " reason))))))

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
;;  * Runs tests that are expected to fail, for a stated reason: each is
;;  * reported as a skip while it fails, and as a failure once it passes. Unlike
;;  * `test-skip`, the tests run, so the change that fixes them is told so.
;;  *
;;  * The reason is an expression, and #f means the tests are not expected to
;;  * fail after all, so an expectation can hold in one configuration only:
;;  * `(test-expect-fail (and *tier-attached* "why") (test ...))`.
;;  *
;;  * @param {string|boolean} reason - Why they fail, or #f.
;;  * @param {...*} body - The tests.
;;  */
(define-syntax test-expect-fail
  (syntax-rules ()
    ((test-expect-fail reason body ...)
     (parameterize ((expected-failure reason)) body ...))))

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
;;  * Whether an error message contains a text, case counting. SRFI 152's
;;  * `string-contains` is not imported for it: the harness is loaded into the
;;  * environment every test file runs in, and the import would bind the SRFI's
;;  * names there too.
;;  * @param {string} message - The message.
;;  * @param {string} text - The text.
;;  * @returns {boolean}
;;  */
(define (message-contains? message text)
  (let ((end (- (string-length message) (string-length text))))
    (let loop ((start 0))
      (and (<= start end)
           (or (string=? (substring message start (+ start (string-length text))) text)
               (loop (+ start 1)))))))

;; /**
;;  * Reports a `test-error` test, given what its expression raised: passed if
;;  * it is an error object whose message contains the expected text, and
;;  * otherwise failed, with the message, or what was raised written, as what
;;  * the test got.
;;  * @param {*} name - The test's name.
;;  * @param {string} expected-msg - The text expected in the message.
;;  * @param {*} raised - What the expression raised.
;;  */
(define (report-error-test-result name expected-msg raised)
  (if (error-object? raised)
      (let ((msg (error-object-message raised)))
        (report-test-result name
                            (and (string? msg) (message-contains? msg expected-msg))
                            expected-msg
                            msg))
      (let ((port (open-output-string)))
        (write raised port)
        (report-test-result name #f expected-msg
                            (string-append "non-error object raised: " (get-output-string port))))))

;; /**
;;  * Why a `test-error` test's expected text cannot test a message, or #f if
;;  * it can. The empty string is refused because every message contains it,
;;  * so a test expecting it would pass on any error.
;;  * @param {*} text - The expected text.
;;  * @returns {string|boolean} The reason, or #f.
;;  */
(define (error-text-refusal text)
  (cond ((not (string? text)) "refused: the expected text is not a string")
        ((string=? text "") "refused: every message contains the empty text")
        (else #f)))

;; /**
;;  * Test that an expression raises an error whose message contains a text.
;;  * It fails if the expression returns, if its error's message does not
;;  * contain the text -- the irritants are not looked in -- or if what it
;;  * raises is not an error object, which has no message. A text that cannot
;;  * test a message, the empty string or one that is not a string, fails the
;;  * test without evaluating the expression.
;;  *
;;  * @param {string} name - Test description.
;;  * @param {string} expected-msg - Text expected in the error's message.
;;  * @param {*} expr - Expression that should raise an error.
;;  */
(define-syntax test-error
  (syntax-rules ()
    ((test-error name expected-msg expr)
     (let* ((text expected-msg)
            (refusal (error-text-refusal text)))
       (if refusal
           (report-test-result name #f text refusal)
           (guard (e (#t (report-error-test-result name text e)))
             expr
             (report-test-result name #f text "no error raised")))))))


;; Compatibility definitions for Chibi tests
;; (test-begin and test-end removed as we standardized on test-group)

(display "Test Harness Loaded\n")

(define-syntax test-assert
  (syntax-rules ()
    ((test-assert name expr)
     (test name #t expr))
    ((test-assert expr)
     (test #t expr))))
