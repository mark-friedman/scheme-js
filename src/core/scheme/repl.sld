;; (scheme repl) library - R7RS standard
;;
;; Provides the interaction environment for REPL use.

(define-library (scheme repl)
  (import (only (scheme primitives) interaction-environment))
  (export interaction-environment)
  (begin
    ;; The runtime's primitive, re-exported.
  ))
