;; Scheme that calls JavaScript, and that JavaScript calls: what the DevTools
;; tests step between (tests/devtools/stepping_tests.js). The tests set
;; breakpoints by line, so lines are not to move. The call to JavaScript is not
;; a tail call, so that a step out of it has a Scheme frame to come back to.

(define (scheme-calls-js n)
  (let ((m (* n 10)))
    (let ((r (user.jsAddOne m)))
      (* r 2))))

(define (scheme-called-from-js x)
  (+ x 100))

(define (scheme-round-trip n)
  (user.jsCallsScheme scheme-called-from-js n))
