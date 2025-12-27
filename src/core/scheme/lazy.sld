;; (scheme lazy) library
;;
;; R7RS lazy evaluation procedures.

(define-library (scheme lazy)
  (import (scheme base))
  (export delay force make-promise promise? delay-force)
  (include "lazy.scm")
  (begin
    ;; /**
    ;;  * delay - Create a promise that will evaluate expr when forced.
    ;;  * @syntax (delay expr)
    ;;  * @returns {promise} A promise.
    ;;  */
    ;; Note: Inline promise construction to avoid hygiene issues with internal binding
    (define-syntax delay
      (syntax-rules ()
        ((delay expr)
         (cons (cons 'promise-tag #f) (lambda () expr)))))
    
    ;; /**
    ;;  * delay-force - Create a promise that forces another promise when forced.
    ;;  * Used for iterative lazy algorithms.
    ;;  * @syntax (delay-force expr)
    ;;  * @returns {promise} A promise.
    ;;  */
    (define-syntax delay-force
      (syntax-rules ()
        ((delay-force expr)
         (cons (cons 'promise-tag #f) (lambda () (force expr))))))))

