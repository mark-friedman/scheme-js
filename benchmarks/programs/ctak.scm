;; ctak.scm -- the Takeuchi function rewritten to use call/cc for every return.
;;
;; CONTINUATION BENCHMARK. Reifies a continuation at each step of the recursion,
;; at moderate depth, and invokes each one exactly once as an escape. This is the
;; workload where an explicit-frame-stack design pays a copy proportional to stack
;; depth on every capture, and it is where Thivierge & Feeley measured Scheme2JS
;; at 95x slower than Gambit-JS. It is therefore the primary discriminator for the
;; calling-convention comparison.
;;
;; Canonical size (22 12 6), matching Thivierge & Feeley `ctak`.

;; /**
;;  * Takeuchi function whose result is delivered to an explicit continuation.
;;  * @param {procedure} k - Continuation receiving the result.
;;  * @param {exact-integer} x - First argument.
;;  * @param {exact-integer} y - Second argument.
;;  * @param {exact-integer} z - Third argument.
;;  * @returns {*} Does not return normally; invokes k.
;;  */
(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (ctak-aux
        k
        (call/cc (lambda (k) (ctak-aux k (- x 1) y z)))
        (call/cc (lambda (k) (ctak-aux k (- y 1) z x)))
        (call/cc (lambda (k) (ctak-aux k (- z 1) x y))))))

;; /**
;;  * Continuation-based Takeuchi function.
;;  * @param {exact-integer} x - First argument.
;;  * @param {exact-integer} y - Second argument.
;;  * @param {exact-integer} z - Third argument.
;;  * @returns {exact-integer} The Takeuchi value.
;;  */
(define (ctak x y z)
  (call/cc
    (lambda (k) (ctak-aux k x y z))))

(define (bench-run)
  (ctak bench-size
        (quotient (* bench-size 6) 11)
        (quotient (* bench-size 3) 11)))
