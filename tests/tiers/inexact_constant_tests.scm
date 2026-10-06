;; inexact_constant_tests.scm -- arithmetic against an inexact constant, in
;; either tier.
;;
;; An inexact real whose value is an integer is boxed, and any other real is a
;; JavaScript number or a BigInt (src/core/interpreter/number_representation.js).
;; Against an inexact constant, compiled code computes on the other operand's
;; double inline, whether it is a number or a box, and boxes an integral result,
;; since the result is inexact whatever the other operand is; anything else
;; goes to the runtime ("Arithmetic and comparison" in src/compiler/inline.scm).
;; These check that the answers, and their exactness, are the interpreter's for
;; every kind of operand -- a box, a fraction, an exact integer, a BigInt, a
;; rational, a complex number and something that is not a number -- and on
;; either side of the operator.
;;
;; The file runs twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js), and each procedure is called twice
;; before it is tested, since the tier compiles a procedure on its second call.

;; /**
;;  * The four arithmetic results and two comparisons of x against 2.
;;  * @param {*} x - The operand.
;;  * @returns {list}
;;  */
(define (against-two x)
  (list (+ x 2.) (- x 2.) (- 2. x) (* x 2.) (< x 2.) (= 2. x)))

;; /**
;;  * x plus 2., alone, for an operand the comparisons would refuse.
;;  * @param {*} x - The operand.
;;  * @returns {number}
;;  */
(define (plus-two x)
  (+ x 2.))

;; /**
;;  * The same against a fraction.
;;  * @param {*} x - The operand.
;;  * @returns {list}
;;  */
(define (against-half x)
  (list (+ x .5) (- .5 x) (* x .5) (> x .5)))

;; /**
;;  * Whether each of a list's numbers is exact.
;;  * @param {list} xs - The numbers.
;;  * @returns {list}
;;  */
(define (exactness xs)
  (map (lambda (x) (if (number? x) (exact? x) x)) xs))

;; /**
;;  * The Fibonacci number of n, recursively, in whatever exactness n has.
;;  * @param {number} n - The index.
;;  * @returns {number}
;;  */
(define (fib n)
  (if (< n 2.) n (+ (fib (- n 1.)) (fib (- n 2.)))))

(define (warm thunk) (thunk) (thunk))
(warm (lambda () (against-two 3.)))
(warm (lambda () (against-half 3)))
(warm (lambda () (plus-two 3)))
(warm (lambda () (fib 5.)))

;; /**
;;  * Whether a procedure is compiled.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {boolean}
;;  */
(define (compiled? procedure)
  (eq? #t (js-ref procedure "$compiled")))

(test-group "arithmetic against an inexact constant"
  (test "the tier compiled them, and only in the run with it attached"
        *tier-attached*
        (and (compiled? against-two) (compiled? against-half) (compiled? plus-two) (compiled? fib)))
  (test "a box" '(5. 1. -1. 6. #f #f) (against-two 3.))
  (test "a box's results are inexact" '(#f #f #f #f #f #f) (exactness (against-two 3.)))
  (test "a fraction" '(4.5 .5 -.5 5. #f #f) (against-two 2.5))
  (test "a fraction's results that land on an integer are boxed" '(#f #f #f #f #f #f)
        (exactness (against-two 2.5)))
  (test "an exact integer makes the results inexact" '(5. 1. -1. 6. #f #f) (against-two 3))
  (test "and so they are" '(#f #f #f #f #f #f) (exactness (against-two 3)))
  (test "an exact integer equal to it" '(4. 0. 0. 4. #f #t) (against-two 2))
  (test "-0.0 keeps its sign" "-0.0" (number->string (* -0. 2.)))
  (test "zero times it is inexact" '(#f "-0.0") (list (exact? (* 0 2.)) (number->string (* (- 0. 0.) -2.))))
  (test "a BigInt" (list (inexact (+ (expt 2 70) 2)) #f)
        (let ((r (against-two (expt 2 70)))) (list (car r) (list-ref r 4))))
  (test "a rational" '(2.5 -1.5 1.5 1. #t #f) (against-two 1/2))
  (test "a complex number" (make-rectangular 2. 1.) (plus-two (make-rectangular 0. 1.)))
  (test "against a fraction" '(3.5 -2.5 1.5 #t) (against-half 3))
  (test "and a box against it" '(3.5 -2.5 1.5 #t) (against-half 3.))
  (test "whose result lands on an integer" '(1. #f) (let ((x (car (against-half .5)))) (list x (exact? x))))
  (test "a recursion on boxes" '(55. #f) (let ((x (fib 10.))) (list x (exact? x))))
  (test "and from an exact entry, which the inexact constants make inexact"
        '(55. #f) (let ((x (fib 10))) (list x (exact? x))))
  (test-error "something that is not a number is the primitive's error" "number" (against-two 'a))
  (test-error "on either side" "number" (- 2. "a")))
