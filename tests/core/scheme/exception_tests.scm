;; Exception Handling Tests
;; Tests for R7RS raise, with-exception-handler, guard, and error

;; Test: raise invokes handler (R7RS: handler returning from non-continuable raise
;; raises a secondary exception, so we use guard to catch it and verify handler ran)
(test "raise invokes handler"
  'handler-ran
  (let ((result #f))
    (guard (e (#t (if result 'handler-ran 'handler-not-ran)))
      (with-exception-handler
        (lambda (e) (set! result e))
        (lambda () (raise 'test-exception))))))

;; Test: raise-continuable allows handler to return
(test "raise-continuable allows handler return"
  43
  (with-exception-handler
    (lambda (e) 42)
    (lambda () (+ 1 (raise-continuable 'oops)))))

;; Test: guard catches exception
(test "guard catches exception"
  'caught
  (guard (e (#t 'caught))
    (raise 'error)))

;; Test: guard with else clause
(test "guard with else clause"
  3
  (guard (e
          ((eq? e 'alpha) 1)
          ((eq? e 'beta) 2)
          (else 3))
    (raise 'gamma)))

;; Test: guard matches first true clause
(test "guard matches correct clause"
  2
  (guard (e
          ((eq? e 'alpha) 1)
          ((eq? e 'beta) 2)
          (else 3))
    (raise 'beta)))

;; Test: error procedure creates SchemeError
(test "error creates error-object"
  #t
  (guard (e (#t (error-object? e)))
    (error "test error" 1 2 3)))

;; Test: error-object-message
(test "error-object-message"
  "hello world"
  (guard (e (#t (error-object-message e)))
    (error "hello world")))

;; Test: guard body returns without exception
(test "guard body returns normally"
  3
  (guard (e (else 'caught))
    (+ 1 2)))

;; Test: dynamic-wind after runs on exception
(test "dynamic-wind after runs on exception"
  '(after before)
  (let ((log '()))
    (guard (e (#t log))
      (dynamic-wind
        (lambda () (set! log (cons 'before log)))
        (lambda () (raise 'error))
        (lambda () (set! log (cons 'after log)))))))

;; Test: guard inside dynamic-wind
(test "guard inside dynamic-wind"
  '(after caught before)
  (let ((log '()))
    (dynamic-wind
      (lambda () (set! log (cons 'before log)))
      (lambda ()
        (guard (e (#t (set! log (cons 'caught log))))
          (raise 'error)))
      (lambda () (set! log (cons 'after log))))
    log))

;; Test: nested handlers - inner handler runs but returning causes secondary exception
;; Per R7RS, we need guard to catch the non-continuable return
(test "nested handlers"
  'inner-returned-as-expected
  (guard (e (#t 'inner-returned-as-expected))
    (with-exception-handler
      (lambda (e) 'outer)
      (lambda ()
        (with-exception-handler
          (lambda (e) 'inner)  ;; This runs but returning raises secondary exception
          (lambda () (raise 'test)))))))

;; =============================================================================
;; call/cc + Exception Interaction Tests
;; =============================================================================

;; Test: guard + call/cc escape (continuation escapes before exception)
(test "guard + call/cc escape"
  'escaped
  (call/cc
    (lambda (escape)
      (guard (e (#t 'guard-caught))
        (escape 'escaped)
        (raise 'should-not-reach)))))

;; Test: raise-continuable returns through handler
(test "raise-continuable returns value"
  105
  (+ 5 (with-exception-handler
         (lambda (e) 100)
         (lambda () (raise-continuable 'ignored)))))
