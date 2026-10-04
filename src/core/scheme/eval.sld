;; (scheme eval) library
;;
;; R7RS evaluation procedures (6.12).

(define-library (scheme eval)
  (import (scheme base) (only (scheme primitives) %import-environment))
  (export eval environment)
  (begin
    ;; /**
    ;;  * An environment holding what some import sets import, for `eval`
    ;;  * (R7RS 6.12): `(eval '(+ 1 2) (environment '(scheme base)))`.
    ;;  * @param {...list} sets - The import sets.
    ;;  * @returns {environment}
    ;;  */
    (define (environment . sets)
      (%import-environment sets))))
