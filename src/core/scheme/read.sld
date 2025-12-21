;; R7RS (scheme read) library
;;
;; Provides the read procedure for parsing S-expressions.
;; Per R7RS §6.13.2.

(define-library (scheme read)
  (import (scheme primitives))
  
  (export read)
  
  (begin
    ;; read is implemented as a primitive
  ))
