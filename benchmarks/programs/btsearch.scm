;; btsearch.scm -- backtracking search built on re-entrant continuations.
;;
;; CONTINUATION BENCHMARK, and the one that genuinely requires MULTI-SHOT
;; semantics: `cont` is invoked again after it has already returned, so a design
;; that consumes frames on invocation rather than copying them will produce wrong
;; answers here rather than merely slow ones. Treat a wrong result from this
;; program as a correctness failure, not a performance data point.
;;
;; Canonical size 2000, matching Thivierge & Feeley `btsearch2000`.

(define fail (lambda () #f))

;; /**
;;  * Enumerates a..b, delivering each value to `cont` and recording a backtrack
;;  * point in the global `fail` thunk.
;;  * @param {exact-integer} a - Current value.
;;  * @param {exact-integer} b - Upper bound.
;;  * @param {procedure} cont - Continuation receiving each candidate value.
;;  * @returns {*} Does not return normally.
;;  */
(define (enumerate a b cont)
  (if (> a b)
      (fail)
      (let ((save fail))
        (set! fail
          (lambda ()
            (set! fail save)
            (enumerate (+ a 1) b cont)))
        (cont a))))

;; /**
;;  * Non-deterministically yields some value in the range a..b, backtrackable
;;  * by invoking the current `fail` thunk.
;;  * @param {exact-integer} a - Lower bound.
;;  * @param {exact-integer} b - Upper bound.
;;  * @returns {exact-integer} A candidate value.
;;  */
(define (in-range a b)
  (call/cc
    (lambda (cont)
      (enumerate a b cont))))

;; /**
;;  * Searches for a pair (x . y) in 0..n whose sum reaches 2n, by backtracking.
;;  * @param {exact-integer} n - Search bound.
;;  * @returns {pair} The pair (n . n).
;;  */
(define (btsearch n)
  (let* ((n*2 (* n 2))
         (x (in-range 0 n))
         (y (in-range 0 n)))
    (if (< (+ x y) n*2)
        (fail)
        (cons x y))))

(define (bench-run)
  (set! fail (lambda () #f))
  (btsearch bench-size))
