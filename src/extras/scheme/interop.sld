;; (scheme-js interop) library
;;
;; Provides primitives for JavaScript interoperability.

(define-library (scheme-js interop)
  (import (scheme primitives))
  (export
    js-eval
    js-ref
    js-set!)
  (begin
    ;; Primitives are injected into the global environment
    ;; and inherited, or imported via (scheme primitives).
  ))
