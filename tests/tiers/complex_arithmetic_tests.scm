;; complex_arithmetic_tests.scm -- complex arithmetic in compiled code.
;;
;; Compiled code does `+`, `-` and `*` inline when both operands are exact
;; integers or both flonums, and calls the primitive otherwise. These check
;; that a complex operand reaches the primitive from compiled code, and that
;; an operation on exact operands stays exact there as it does interpreted
;; (R7RS 6.2.2): (* +i 2) is 0+2i, not 0.0+2.0i.

;; /**
;;  * Whether a procedure is compiled.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {boolean}
;;  */
(define (compiled? procedure)
  (eq? #t (js-ref procedure "$compiled")))

;; /**
;;  * A number doubled n times; it loops, so the tier compiles it when it is
;;  * bound.
;;  * @param {number} z - The number.
;;  * @param {integer} n - How many times to double it.
;;  * @returns {number}
;;  */
(define (double-times z n)
  (let loop ((z z) (n n))
    (if (= n 0) z (loop (* z 2) (- n 1)))))

;; Each is compiled on its second call, so each is called twice here first.
(define (add a b) (+ a b))
(define (sub a b) (- a b))
(define (mul a b) (* a b))
(define (div a b) (/ a b))
(define (neg a) (- a))
(define (sq a) (square a))
(define (call-twice procedure . arguments)
  (apply procedure arguments)
  (apply procedure arguments))
(for-each (lambda (op) (call-twice op 1 2)) (list add sub mul div))
(call-twice neg 1)
(call-twice sq 1)

(test-group "complex arithmetic in compiled code"

  (test "the procedures are compiled when the tier is attached"
    *tier-attached*
    (and (compiled? double-times) (compiled? add) (compiled? sub)
         (compiled? mul) (compiled? div) (compiled? neg) (compiled? sq)))

  (test "doubled three times" 0+8i (double-times (make-rectangular 0 1) 3))
  (test "plus an integer" 1+i (add +i 1))
  (test "minus itself" 0 (sub +i +i))
  (test "divided by an integer" +i (div +2i 2))
  (test "two complex numbers multiplied" 11+2i (mul 1+2i 3-4i))
  (test "two complex numbers divided" +i (div 1+i 1-i))
  (test "fractional parts plus a fraction" 1+i (add 1/2+i 1/2))
  (test "negation" -3-4i (neg 3+4i))
  (test "the square of i" -1 (sq +i))
  (test "the square of a fraction" 1/4 (sq 1/2))
  (test "an inexact operand" (make-rectangular 0.0 2.0) (mul +i 2.0)))
