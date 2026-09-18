;; oddeven.scm -- mutually recursive parity test.
;;
;; Every call is in tail position, so this measures pure tail-call throughput
;; with no continuation frames created at all. It is the benchmark that most
;; directly exposes trampoline cost, and the one where a compiler that turns
;; tail calls into loops or trampoline returns should win most cleanly.
;;
;; Canonical size 100000000, matching Thivierge & Feeley `oddeven`.

;; /**
;;  * Tail-recursive odd test, mutually recursive with `evn`.
;;  * @param {exact-integer} n - A non-negative integer.
;;  * @returns {boolean} #t if n is odd.
;;  */
(define (odd n)
  (if (= n 0) #f (evn (- n 1))))

;; /**
;;  * Tail-recursive even test, mutually recursive with `odd`.
;;  * @param {exact-integer} n - A non-negative integer.
;;  * @returns {boolean} #t if n is even.
;;  */
(define (evn n)
  (if (= n 0) #t (odd (- n 1))))

(define (bench-run) (odd bench-size))
