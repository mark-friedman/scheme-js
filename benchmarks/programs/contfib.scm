;; contfib.scm -- Fibonacci that captures a continuation at every leaf.
;;
;; CONTINUATION BENCHMARK. Unlike ctak the captured continuation is shallow --
;; only a few frames deep -- but capture happens extremely often. This separates
;; "cost per capture" from "cost proportional to stack depth": a design whose
;; capture cost is O(depth) looks fine here and bad on ctak, while a design with
;; a high fixed cost per capture looks bad here.
;;
;; Canonical size 30, matching Thivierge & Feeley `contfib30`.

;; /**
;;  * Fibonacci that reifies the current continuation at each base case.
;;  * @param {exact-integer} n - The index.
;;  * @returns {exact-integer} The nth Fibonacci number (contfib(0) = contfib(1) = 1).
;;  */
(define (contfib n)
  (if (< n 2)
      (call/cc
        (lambda (k)
          (k 1)))
      (+ (contfib (- n 1))
         (contfib (- n 2)))))

(define (bench-run) (contfib bench-size))
