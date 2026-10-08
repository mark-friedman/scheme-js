;; handlers_tests.scm -- (scheme-js handlers), interpreted.
;;
;; A program compiled ahead of time has these handlers in the primitives'
;; place, and there a continuation travels the winds they are installed with
;; and the driver hands them what JavaScript throws, which
;; tests/functional/ahead_program_tests.js checks by building programs.
;; Interpreted, as these run, neither happens, so these check what needs
;; neither: what a handler is called with, which handlers are in force when,
;; what a continuable raise returns, the extents a raise leaves, and what a raise
;; with no handler of theirs, or a handler's return, raises -- which, as the
;; library raises it with the primitives, the interpreter's `guard` catches.

(import (prefix (scheme-js handlers) h:)
        (prefix (scheme-js winds) w:)
        (only (scheme primitives) %handlers %winds %set-handlers! %set-error-raiser!))

(test-group "(scheme-js handlers)"
  (test "a continuable raise is its handler's value" 42
        (h:with-exception-handler (lambda (c) (* c 2)) (lambda () (h:raise-continuable 21))))
  (test "twice in one extent" 30
        (h:with-exception-handler (lambda (c) (* c 10))
          (lambda () (+ (h:raise-continuable 1) (h:raise-continuable 2)))))
  (test "a handler runs with the handler outside its own in force" '(outer 1 inner)
        (h:with-exception-handler
          (lambda (c) 'outer)
          (lambda ()
            (h:with-exception-handler
              (lambda (c) (if (eq? c 'first) (h:raise-continuable 'nested) 1))
              (lambda ()
                (let* ((a (h:raise-continuable 'first))
                       (b (h:raise-continuable 'second)))
                  (list a b 'inner)))))))
  (test "the thunk's values are with-exception-handler's" '(1 2)
        (call-with-values (lambda () (h:with-exception-handler (lambda (c) c) (lambda () (values 1 2))))
          list))
  (let ((before (%handlers)))
    (test "a handler is in force within its extent, before those outside it" #t
          (h:with-exception-handler (lambda (c) c)
            (lambda () (and (pair? (%handlers)) (eq? (cdr (%handlers)) before)))))
    (test "and the handlers outside it after it" #t
          (begin (h:with-exception-handler (lambda (c) c) (lambda () 'done))
                 (eq? (%handlers) before))))
  (let ((log '()))
    (define (note x) (set! log (cons x log)))
    (test "a continuable raise leaves no extent" '(6 (in (handler x) out))
          (let ((value (h:with-exception-handler
                         (lambda (c) (note (list 'handler c)) 5)
                         (lambda ()
                           (w:dynamic-wind (lambda () (note 'in))
                                           (lambda () (+ 1 (h:raise-continuable 'x)))
                                           (lambda () (note 'out)))))))
            (list value (reverse log)))))
  (let ((log '()))
    (define (note x) (set! log (cons x log)))
    (test "a raise leaves the extents between it and its handler before calling it, and a handler's return is an error"
          '("non-continuable exception: handler returned" (in out (handler x)) #t)
          (let ((before (%handlers))
                (message (guard (e ((error-object? e) (error-object-message e)))
                           (h:with-exception-handler
                             (lambda (c) (note (list 'handler c)) 'returned)
                             (lambda ()
                               (w:dynamic-wind (lambda () (note 'in))
                                               (lambda () (h:raise 'x))
                                               (lambda () (note 'out))))))))
            (list message (reverse log) (eq? (%handlers) before)))))
  (test "a raise with no handler of these goes on as a raise nobody handles" "Unhandled exception: nobody"
        (guard (e ((error-object? e) (error-object-message e))) (h:raise 'nobody)))
  (test "an error object is raised as itself" #t
        (let ((object (guard (e (#t e)) (error "made" 1))))
          (eq? object (guard (e (#t e)) (h:raise object)))))
  (test-error "the handler must be a procedure" "with-exception-handler"
              (h:with-exception-handler 1 (lambda () 1)))
  (test-error "and the thunk" "with-exception-handler"
              (h:with-exception-handler (lambda (c) c) 1))
  (test-error "the handlers set must be a list" "%set-handlers!" (%set-handlers! 5))
  (test-error "what errors are handed to must be a procedure" "%set-error-raiser!" (%set-error-raiser! 5)))
