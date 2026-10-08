;; (scheme-js handlers) library
;;
;; `with-exception-handler`, `raise` and `raise-continuable` over the handlers
;; the runtime keeps, for a program compiled ahead of time, which has no
;; interpreter to keep them as frames on its stack. The build compiles them
;; with a program that reaches them -- through `guard`, say -- in the
;; primitives' place (`library-primitives` in scripts/lib/ahead.scm). Under
;; the interpreter they are the interpreter's, and a program it runs does not
;; import this: the interpreter's continuations do not travel the winds these
;; install handlers with. Its procedures are in handlers.scm.

(define-library (scheme-js handlers)
  ;; The runtime's procedures first, so that (scheme-js winds)'s
  ;; `dynamic-wind` is the one bound.
  (import (scheme primitives)
          (scheme core)
          (scheme-js winds))
  (export with-exception-handler raise raise-continuable)
  (include "handlers.scm"))
