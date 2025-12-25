;; (scheme case-lambda) library
;;
;; R7RS library providing case-lambda for multi-arity procedures.

(define-library (scheme case-lambda)
  (import (scheme base))
  (export case-lambda)
  (include "case_lambda.scm"))
