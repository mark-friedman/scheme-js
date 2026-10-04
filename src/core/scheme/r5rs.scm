;;; r5rs.scm -- the environments of (scheme r5rs)

;; /**
;;  * An environment holding R5RS's bindings (R5RS 6.5), for `eval`.
;;  * @param {integer} version - 5, the only version known.
;;  * @returns {environment}
;;  */
(define (scheme-report-environment version)
  (if (not (eqv? version 5)) (error "scheme-report-environment: unsupported version" version))
  (environment '(scheme r5rs)))

;; /**
;;  * An environment holding R5RS's syntactic keywords and nothing else
;;  * (R5RS 6.5), for `eval`.
;;  * @param {integer} version - 5, the only version known.
;;  * @returns {environment}
;;  */
(define (null-environment version)
  (if (not (eqv? version 5)) (error "null-environment: unsupported version" version))
  (environment '(only (scheme r5rs)
                      and begin case cond define define-syntax delay do else if lambda
                      let let* let-syntax letrec letrec-syntax or quasiquote quote set!
                      syntax-rules => ...)))
