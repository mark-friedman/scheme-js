; Simple Scheme Test Harness

(define *test-failures* 0)
(define *test-passes* 0)

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

(define (assert-equal msg expected actual)
  (if (equal? expected actual)
      (report-test-result msg #t expected actual)
      (report-test-result msg #f expected actual)))

(define-syntax test
  (syntax-rules ()
    ((test msg expected expr)
     (assert-equal msg expected expr))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group name body ...)
     (begin
       (display "=== ") (display name) (display " ===") (newline)
       body ...
       (newline)))))
