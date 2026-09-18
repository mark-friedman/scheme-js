;; tak.scm -- the Takeuchi function, a classic Gabriel benchmark.
;;
;; Like fib it is call-dominated, but with three arguments and a deeper
;; non-tail call graph, so it stresses argument passing and environment
;; construction more heavily than fib does.
;;
;; `bench-size` scales the first argument; the other two are derived so the
;; standard (18 12 6) shape is preserved.

;; /**
;;  * The Takeuchi function.
;;  * @param {exact-integer} x - First argument.
;;  * @param {exact-integer} y - Second argument.
;;  * @param {exact-integer} z - Third argument.
;;  * @returns {exact-integer} The Takeuchi value.
;;  */
(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (bench-run)
  (tak bench-size
       (quotient (* bench-size 2) 3)
       (quotient bench-size 3)))
