;; A program built ahead of time (`node repl.js --build`), which the DevTools
;; tests build, set breakpoints in and step through
;; (tests/devtools/stepping_tests.js): its own procedures, called from the
;; page's JavaScript, and the system's `map`, which the program carries
;; compiled. The tests find each line by its text.

(import (scheme base) (scheme-js interop))

(define (built-step n)
  (let ((m (* n 10)))
    (+ m 1)))

(define (built-double items)
  (map (lambda (x)
         (+ x x))
       items))

(js-set! (js-eval "window") "builtStep" built-step)
(js-set! (js-eval "window") "builtDouble" (lambda (n) (car (built-double (list n)))))
(js-set! (js-eval "window") "ready" #t)
