;; (scheme process-context) library
;;
;; R7RS process context procedures.
;; Some features are environment-specific (Node.js vs browser).

(define-library (scheme process-context)
  (import (scheme base))
  (export command-line exit get-environment-variable get-environment-variables
          emergency-exit)
  (begin
    ;; These are implemented as JS primitives that detect the environment
    ;; and provide appropriate implementations.
    
    ;; Note: In browser environment, many of these will return
    ;; empty/default values or be no-ops.
    ))
