;; (scheme-js promise) library
;;
;; JavaScript Promise interoperability for Scheme.
;; Provides primitives and macros for working with JavaScript Promises
;; in a CPS-style that preserves TCO within callback segments.

(define-library (scheme-js promise)
  (import (scheme base)
          (only (scheme primitives) js-promise? make-js-promise js-promise-resolve
                js-promise-reject js-promise-then js-promise-catch js-promise-finally
                js-promise-all js-promise-race js-promise-all-settled))
  (export 
    ;; Predicates
    js-promise?
    ;; Constructors
    make-js-promise
    js-promise-resolve
    js-promise-reject
    ;; Combinators
    js-promise-then
    js-promise-catch
    js-promise-finally
    js-promise-all
    js-promise-race
    js-promise-all-settled
    ;; Scheme utilities
    js-promise-map
    js-promise-chain
    ;; Macro for CPS transformation
    async-lambda)
  (include "promise.scm"))
