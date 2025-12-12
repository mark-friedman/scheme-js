(define-library (scheme control)
  (import (scheme primitives))
  (import (scheme core)) ;; for let, if, begin, etc.

  (include "control.scm")

  (export
    when unless or let* do case
  )
)
