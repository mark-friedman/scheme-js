;; js_callee_tests.scm -- Scheme calling JavaScript functions, in either tier.
;;
;; What a JavaScript function returns arrives in Scheme converted as `js-invoke`
;; converts it, one level deep (`jsToScheme`): an integral number is an exact
;; integer, and an array or object is JavaScript's own, its contents as they
;; are. The interpreter and compiled code call a JavaScript function in
;; different places -- the interpreter's application, and `callForeign` -- so
;; the file runs twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js), and each procedure is called twice
;; before it is tested, since the tier compiles a procedure on its second call.

(define js-two (js-eval "() => 2"))
(define js-half (js-eval "() => 0.5"))
(define js-echo (js-eval "(x) => x"))
(define js-pair (js-eval "() => [1, 2]"))
(define holder (js-eval "({ two: () => 2 })"))

;; /**
;;  * Whether a procedure is compiled.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {boolean}
;;  */
(define (compiled? procedure)
  (eq? #t (js-ref procedure "$compiled")))

(define (two-directly) (js-two))
(define (half-directly) (js-half))
(define (two-through-js-invoke) (js-invoke holder "two"))
(define (echoed n) (js-echo n))
(define (pair-from-js) (js-pair))
;; Not in tail position: a tail call to a JavaScript function is returned to
;; the interpreter, which makes it.
(define (two-not-in-tail) (let ((n (js-two))) n))

(define (twice thunk) (thunk) (thunk))
(twice two-directly)
(twice half-directly)
(twice two-through-js-invoke)
(echoed 1) (echoed 1)
(twice pair-from-js)
(twice two-not-in-tail)

(test-group "A JavaScript function's result"
  (test "the tier compiled the callers, and only in the run with it attached"
        *tier-attached*
        (and (compiled? two-directly) (compiled? two-not-in-tail) (compiled? pair-from-js)))
  (test "an integral number arrives exact, called directly" #t (exact? (two-directly)))
  (test "and called from compiled code not in tail position" #t (exact? (two-not-in-tail)))
  (test "as it does through js-invoke" #t (exact? (two-through-js-invoke)))
  (test "a number that is not integral arrives inexact" #f (exact? (half-directly)))
  (test "an exact integer passed to JavaScript and returned is exact again" #t (exact? (echoed 3)))
  (test "an array arrives as it is, its elements unconverted" '(#f #f)
        (map exact? (vector->list (pair-from-js)))))
