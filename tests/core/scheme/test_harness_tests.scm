(test-group "Test Harness"
  (test "Basic assertion" 1 1)
  (test "Nested assertion" #t #t)
)

;; /**
;;  * Runs some tests with what they report kept from the runner, and gives the
;;  * counters back as they were, so that tests of the harness's own reporting
;;  * can make it report a failure without failing this file.
;;  * @param {procedure} thunk - Runs the tests.
;;  * @returns {list} What was reported, in order -- `(result passed?)` or
;;  *   `(skip)` for each -- and how the pass, failure and skip counters moved.
;;  */
(define (reports-of thunk)
  (let ((report-result native-report-test-result)
        (report-skip native-report-test-skip)
        (passes *test-passes*)
        (failures *test-failures*)
        (skips *test-skips*)
        (reports '()))
    (set! native-report-test-result
          (lambda (name passed expected actual)
            (set! reports (cons (list 'result passed) reports))))
    (set! native-report-test-skip
          (lambda (name reason) (set! reports (cons '(skip) reports))))
    (thunk)
    (set! native-report-test-result report-result)
    (set! native-report-test-skip report-skip)
    (let ((moved (list (- *test-passes* passes)
                       (- *test-failures* failures)
                       (- *test-skips* skips))))
      (set! *test-passes* passes)
      (set! *test-failures* failures)
      (set! *test-skips* skips)
      (list (reverse reports) moved))))

(test-group "Expected failures"
  (test "a test expected to fail that fails is reported as a skip"
        '(((skip)) (0 0 1))
        (reports-of (lambda ()
                      (test-expect-fail "one is not two" (test "one is two" 1 2)))))
  (test "and so is one that raises"
        '(((skip)) (0 0 1))
        (reports-of (lambda ()
                      (test-expect-fail "the list is empty" (test "the empty list's car" 1 (car '()))))))
  (test "a test expected to fail that passes is reported as a failure, so a fix has to say so"
        '(((result #f)) (0 1 0))
        (reports-of (lambda ()
                      (test-expect-fail "one is not one" (test "one is one" 1 1)))))
  (test "a reason of #f expects nothing"
        '(((result #t)) (1 0 0))
        (reports-of (lambda ()
                      (test-expect-fail #f (test "one is one" 1 1)))))
  (test "the expectation covers every test in its body, and ends with it"
        '(((skip) (skip) (result #t)) (1 0 2))
        (reports-of (lambda ()
                      (test-expect-fail "one is not two"
                        (test "one is two" 1 2)
                        (test "two is three" 2 3))
                      (test "one is one" 1 1)))))
