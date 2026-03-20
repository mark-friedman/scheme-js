;; Library for define-macro support
;; This library exports the define-macro special form.
;; It is loaded by default in the REPL environment.

(define-library (scheme-js define-macro)
  (export define-macro)
  (import (scheme base))
)
