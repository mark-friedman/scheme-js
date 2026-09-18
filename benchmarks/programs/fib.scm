;; fib.scm -- naive doubly-recursive Fibonacci.
;;
;; Measures raw procedure call throughput: two non-tail calls, one comparison
;; and two subtractions per node, with no allocation. This is the single most
;; diagnostic benchmark for interpretive dispatch overhead.
;;
;; Canonical size 35, matching Thivierge & Feeley (SFP 2012) `fib35`, so results
;; are directly comparable to the published Gambit-JS / Scheme2JS / Spock numbers.
;;
;; Note: returns 1 for n < 2 (not n), matching the published program.

;; /**
;;  * Computes the nth Fibonacci number by naive double recursion.
;;  * @param {exact-integer} n - The index.
;;  * @returns {exact-integer} The nth Fibonacci number (fib(0) = fib(1) = 1).
;;  */
(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (bench-run) (fib bench-size))
