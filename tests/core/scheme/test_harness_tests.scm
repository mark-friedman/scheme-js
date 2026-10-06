(test-group "Test Harness"
  (test "Basic assertion" 1 1)
  (test "Nested assertion" #t #t)
)

;; /**
;;  * Runs some tests with what they report kept from the runner, and gives the
;;  * counters back as they were, so that tests of the harness's own reporting
;;  * can make it report a failure without failing this file.
;;  * @param {procedure} thunk - Runs the tests.
;;  * @param {procedure} [record] - What to keep of a result, from its name,
;;  *   whether it passed, and the expected and actual values it reports;
;;  *   `(result passed?)` if not given.
;;  * @returns {list} What was reported, in order -- a result's record, or
;;  *   `(skip)` -- and how the pass, failure and skip counters moved.
;;  */
(define (reports-of thunk . record)
  (let ((record (if (pair? record)
                    (car record)
                    (lambda (name passed expected actual) (list 'result passed))))
        (report-result native-report-test-result)
        (report-skip native-report-test-skip)
        (passes *test-passes*)
        (failures *test-failures*)
        (skips *test-skips*)
        (reports '()))
    (set! native-report-test-result
          (lambda (name passed expected actual)
            (set! reports (cons (record name passed expected actual) reports))))
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

;; /**
;;  * What a test reports as its actual value, with the report kept from the
;;  * runner.
;;  * @param {procedure} thunk - Runs the test.
;;  * @returns {*} The actual value reported.
;;  */
(define (actual-reported thunk)
  (cadr (car (car (reports-of thunk (lambda (name passed expected actual) (list 'result actual)))))))

;; `test-error` passes when the expression raises an error object whose message
;; contains the text the test gives, and fails otherwise. It passed on any
;; error at all, so a test could not tell which error a call raised: one
;; expecting an arity error passed on the type error a missing argument raised.
(test-group "test-error"
  (test "an error whose message contains the text passes"
        '(((result #t)) (1 0 0))
        (reports-of (lambda () (test-error "an error" "the message" (error "with the message in it" 1)))))
  (test "so does one whose message is the text"
        '(((result #t)) (1 0 0))
        (reports-of (lambda () (test-error "an error" "the message" (error "the message")))))
  (test "an error of a primitive passes on its message"
        '(((result #t)) (1 0 0))
        (reports-of (lambda () (test-error "car of a number" "car" (car 1)))))
  (test "an error whose message does not contain the text fails"
        '(((result #f)) (0 1 0))
        (reports-of (lambda () (test-error "an error" "another message" (error "the message")))))
  (test "and reports the message"
        "the message"
        (actual-reported (lambda () (test-error "an error" "another message" (error "the message")))))
  (test "the text is looked for in the message, not the irritants"
        '(((result #f)) (0 1 0))
        (reports-of (lambda () (test-error "an error" "irritant" (error "the message" 'irritant)))))
  (test "case counts"
        '(((result #f)) (0 1 0))
        (reports-of (lambda () (test-error "an error" "The message" (error "the message")))))
  (test "no error fails"
        '(((result #f)) (0 1 0))
        (reports-of (lambda () (test-error "no error" "the message" 'no-error))))
  (test "and says so"
        "no error raised"
        (actual-reported (lambda () (test-error "no error" "the message" 'no-error))))
  (test "an object that is not an error, raised, fails: it has no message"
        '(((result #f)) (0 1 0))
        (reports-of (lambda () (test-error "a symbol raised" "boom" (raise 'boom)))))
  (test "and is reported, written"
        "non-error object raised: \"boom\""
        (actual-reported (lambda () (test-error "a string raised" "boom" (raise "boom")))))
  (test "an empty text is refused: every message contains it, so it would pass on any error"
        '(((result #f)) (0 1 0))
        (reports-of (lambda () (test-error "an empty text" "" (error "the message")))))
  (test "and says why"
        "refused: every message contains the empty text"
        (actual-reported (lambda () (test-error "an empty text" "" (error "the message")))))
  (test "a text that is not a string is refused, and says so"
        '(((result #f "refused: the expected text is not a string")) (0 1 0))
        (reports-of (lambda () (test-error "a symbol for a text" 'message (error "the message")))
                    (lambda (name passed expected actual) (list 'result passed actual)))))
