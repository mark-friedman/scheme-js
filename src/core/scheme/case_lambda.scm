;; case-lambda - Multi-arity procedure definition
;;
;; R7RS (scheme case-lambda) library
;;
;; case-lambda creates a procedure that dispatches based on argument count.
;; Example:
;;   (define add
;;     (case-lambda
;;       (() 0)
;;       ((x) x)
;;       ((x y) (+ x y))))

;; /**
;;  * case-lambda macro
;;  *
;;  * Creates a procedure that selects a clause based on argument count.
;;  * Uses a simple nested-if approach for dispatching.
;;  *
;;  * @syntax (case-lambda clause ...)
;;  * @param clause - (formals body ...) where formals is a lambda parameter list
;;  * @returns A procedure that dispatches on arity
;;  */

;; Simple implementation: expand to cond-based dispatch on length
;; For (case-lambda (()  body0) ((x) body1) ((x y) body2))
;; expands to:
;; (lambda args
;;   (let ((n (length args)))
;;     (cond
;;       ((= n 0) body0)
;;       ((= n 1) (let ((x (car args))) body1))
;;       ((= n 2) (let ((x (car args)) (y (cadr args))) body2))
;;       (else (error "no matching clause")))))

(define-syntax case-lambda
  (syntax-rules ()
    ;; Single clause - optimize to simple lambda
    ((case-lambda (formals body ...))
     (lambda formals body ...))
    
    ;; Multiple clauses - build generic dispatcher
    ((case-lambda clause ...)
     (lambda args
       (case-lambda-clauses args clause ...)))))

;; Helper: expand clauses to cond-like dispatch
(define-syntax case-lambda-clauses
  (syntax-rules ()
    ;; Base: no more clauses, error
    ((case-lambda-clauses args)
     (error "case-lambda: no matching clause" (length args)))
    
    ;; Empty formals: 0 args
    ((case-lambda-clauses args (() body ...) rest ...)
     (if (null? args)
         (begin body ...)
         (case-lambda-clauses args rest ...)))
    
    ;; One param
    ((case-lambda-clauses args ((a) body ...) rest ...)
     (if (and (pair? args) (null? (cdr args)))
         (let ((a (car args)))
           body ...)
         (case-lambda-clauses args rest ...)))
    
    ;; Two params
    ((case-lambda-clauses args ((a b) body ...) rest ...)
     (if (and (pair? args) (pair? (cdr args)) (null? (cddr args)))
         (let ((a (car args)) (b (cadr args)))
           body ...)
         (case-lambda-clauses args rest ...)))
    
    ;; Three params
    ((case-lambda-clauses args ((a b c) body ...) rest ...)
     (if (and (pair? args) (pair? (cdr args)) (pair? (cddr args)) (null? (cdddr args)))
         (let ((a (car args)) (b (cadr args)) (c (caddr args)))
           body ...)
         (case-lambda-clauses args rest ...)))
    
    ;; Four params
    ((case-lambda-clauses args ((a b c d) body ...) rest ...)
     (if (= (length args) 4)
         (let ((a (car args)) (b (cadr args)) (c (caddr args)) (d (cadddr args)))
           body ...)
         (case-lambda-clauses args rest ...)))
    
    ;; One or more (rest param)
    ((case-lambda-clauses args ((a . rest) body ...) more ...)
     (if (pair? args)
         (let ((a (car args)) (rest (cdr args)))
           body ...)
         (case-lambda-clauses args more ...)))
    
    ;; Two or more (rest param)
    ((case-lambda-clauses args ((a b . rest) body ...) more ...)
     (if (and (pair? args) (pair? (cdr args)))
         (let ((a (car args)) (b (cadr args)) (rest (cddr args)))
           body ...)
         (case-lambda-clauses args more ...)))
    
    ;; Three or more (rest param)
    ((case-lambda-clauses args ((a b c . rest) body ...) more ...)
     (if (and (pair? args) (pair? (cdr args)) (pair? (cddr args)))
         (let ((a (car args)) (b (cadr args)) (c (caddr args)) (rest (cdddr args)))
           body ...)
         (case-lambda-clauses args more ...)))
    
    ;; Pure rest param (matches anything) - must be last
    ((case-lambda-clauses args (rest body ...) more ...)
     (let ((rest args))
       body ...))))
