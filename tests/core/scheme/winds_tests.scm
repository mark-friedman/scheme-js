;; winds_tests.scm -- (scheme-js winds)'s `dynamic-wind`, returning.
;;
;; A program compiled ahead of time has this `dynamic-wind` in the
;; primitive's place, and there a continuation travels the winds it keeps,
;; which tests/functional/ahead_program_tests.js checks by building programs.
;; Interpreted, as these run, continuations do not travel them, so these
;; check what needs none: the order the thunks run in, every value the thunk
;; returns, the winds in force within the extent and after it, and the
;; checks of the arguments.

(import (prefix (scheme-js winds) winds:)
        (only (scheme primitives) %winds %set-winds!))

(test-group "(scheme-js winds) dynamic-wind"
  (let ((log '()))
    (define (note x) (set! log (cons x log)))
    (test "is the thunk's value" 42
          (winds:dynamic-wind (lambda () (note 'before))
                              (lambda () (note 'thunk) 42)
                              (lambda () (note 'after))))
    (test "having run before, the thunk and after, in that order"
          '(before thunk after) (reverse log)))
  (test "every value the thunk returns" '(1 2 3)
        (call-with-values
          (lambda () (winds:dynamic-wind (lambda () #f) (lambda () (values 1 2 3)) (lambda () #f)))
          list))
  (let ((before (lambda () #f))
        (after (lambda () #f))
        (outside (%winds)))
    (test "within the extent, its wind is the innermost"
          #t (winds:dynamic-wind before
                                 (lambda () (let ((w (%winds))) (and (eq? (caar w) before) (eq? (cdar w) after)
                                                                      (eq? (cdr w) outside))))
                                 after))
    (test "nested, each wind is in force within its own extent" 2
          (winds:dynamic-wind before
                              (lambda () (winds:dynamic-wind before (lambda () (length (%winds))) after))
                              after))
    (test "after it, the winds are those before it" #t
          (begin (winds:dynamic-wind before (lambda () 'done) after)
                 (eq? (%winds) outside))))
  (test-error "before must be a procedure" "dynamic-wind" (winds:dynamic-wind 1 (lambda () 1) (lambda () 1)))
  (test-error "the thunk must be a procedure" "dynamic-wind" (winds:dynamic-wind (lambda () 1) 1 (lambda () 1)))
  (test-error "after must be a procedure" "dynamic-wind" (winds:dynamic-wind (lambda () 1) (lambda () 1) 1))
  (test-error "the winds set must be a list" "%set-winds!" (%set-winds! 5)))
