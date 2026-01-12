;; (scheme-js interop) library
;;
;; JavaScript interoperability primitives for Scheme.
;; Provides js-eval, js-ref (property access), and js-set! (property mutation).

(define-library (scheme-js interop)
  (import (scheme base))
  (export
    js-eval
    js-ref
    js-set!
    js-invoke
    js-obj
    js-obj-merge))
