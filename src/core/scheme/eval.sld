;; (scheme eval) library
;;
;; R7RS evaluation procedures.
;; Note: environment procedure not fully supported - returns interaction environment.

(define-library (scheme eval)
  (import (scheme base))
  (export eval environment)
  (begin
    ;; eval and interaction-environment are already primitives
    ;; environment returns the interaction-environment for now
    ;; (R7RS allows implementation-defined behavior for environment)
    (define (environment . import-specs)
      (interaction-environment))))
