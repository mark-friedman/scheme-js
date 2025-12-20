(define-library (scheme repl)
  (import (scheme base))
  (export interaction-environment)
  (begin
    ;; interaction-environment is already a primitive in our system
    ;; so we just re-export it.
  ))
