;; (scheme-js winds) library
;;
;; `dynamic-wind` over the winds the runtime keeps, for a program compiled
;; ahead of time, which has no interpreter to keep them as frames on its stack.
;; The build compiles it with a program that reaches `dynamic-wind` -- through
;; `parameterize`, say -- in the primitive's place (`library-primitives` in
;; scripts/lib/ahead.scm). Under the interpreter `dynamic-wind` is the
;; interpreter's, and a program it runs does not import this: the
;; interpreter's continuations do not travel these winds. Its procedures are
;; in winds.scm.

(define-library (scheme-js winds)
  (import (scheme primitives)
          (scheme core))
  (export dynamic-wind)
  (include "winds.scm"))
