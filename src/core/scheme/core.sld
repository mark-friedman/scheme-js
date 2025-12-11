(define-library (scheme core)
  (import (scheme primitives))
  
  (include "core.scm")
  
  (export
    ;; Macros
    and let letrec cond
    define-record-type define-record-field
    
    ;; Procedures
    equal? map
    native-report-test-result
  )
)
