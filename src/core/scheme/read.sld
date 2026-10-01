;; R7RS (scheme read) library
;;
;; Provides the read procedure for parsing S-expressions.
;; Per R7RS §6.13.2.

(define-library (scheme read)
  ;; Scheme, over the runtime's reader, in ports.scm.
  (import (only (scheme core) read))
  
  (export read))
