;; R7RS Parameter Objects (§4.2.6)
;;
;; Implementation following SRFI-39 / R7RS section 7.3.
;; Uses dynamic-wind for proper unwinding on control flow exit.

;; =============================================================================
;; Dynamic Environment
;; =============================================================================

;; objects to cells (pairs where the cdr holds the value).
;; The global binding is stored directly in each parameter object.

;; Use a box (list) to hold the environment so we can mutate the contents
;; without changing the binding. This ensures multiple closures see the update.
(define *param-dynamic-env-box* (list '()))

;; /**
;;  * Looks up a parameter in the dynamic environment.
;;  * Returns the cell bound to the parameter, or the global cell if not found.
;;  *
;;  * @param {procedure} parameter - The parameter object.
;;  * @param {pair} global-cell - The parameter's global cell.
;;  * @returns {pair} The cell containing the current value.
;;  */
(define (param-dynamic-lookup parameter global-cell)
  (let ((env (car *param-dynamic-env-box*)))
    (let loop ((env env))
      (cond ((null? env) global-cell)
            ((eq? (caar env) parameter) (cdar env))
            (else (loop (cdr env)))))))

;; =============================================================================
;; make-parameter
;; =============================================================================

;; /**
;;  * Creates a new parameter object.
;;  *
;;  * The parameter object is a procedure that:
;;  * - Called with no arguments: returns the current value
;;  * - Called with one argument: sets the value (through converter) and returns unspecified
;;  * - Called with two arguments: internal use for parameterize (returns converted value)
;;  *
;;  * @param {*} init - Initial value.
;;  * @param {procedure} [converter] - Optional conversion procedure.
;;  * @returns {procedure} The parameter object.
;;  */
(define (make-parameter init . conv)
  (let ((converter (if (null? conv) 
                       (lambda (x) x) 
                       (car conv))))
    ;; Global cell: (parameter . value)
    ;; The car is set to the parameter itself for identity
    (let ((global-cell (cons #f (converter init))))
      (letrec ((parameter
                (lambda args
                  (let ((cell (param-dynamic-lookup parameter global-cell)))
                    (cond 
                      ;; No arguments: return current value
                      ((null? args) 
                       (cdr cell))
                      ;; One argument: set the value
                      ((null? (cdr args))
                       (set-cdr! cell (converter (car args))))
                      ;; Two arguments (internal): return converted value for parameterize
                      (else 
                       (converter (car args))))))))
        ;; Store parameter in car for potential debugging
        (set-car! global-cell parameter)
        parameter))))

;; =============================================================================
;; parameterize
;; =============================================================================

;; /**
;;  * Binds parameters to values for the dynamic extent of body.
;;  * Uses dynamic-wind to ensure proper restoration on exit.
;;  *
;;  * NOTE: This procedure is called by the parameterize macro and must be
;;  * exported from the library. This is an implementation detail that will
;;  * be unnecessary once we have proper referential transparency in macros.
;;  *
;;  * @param {list} params - List of parameter objects.
;;  * @param {list} values - List of values to bind.
;;  * @param {procedure} body - Thunk to execute.
;;  * @returns {*} Result of body.
;;  */
(define (param-dynamic-bind params values body)
  (let ((old-env (car *param-dynamic-env-box*)))
    ;; Create new cells for each parameter with converted values
    (let ((new-cells 
           (let loop ((ps params) (vs values) (cells '()))
             (if (null? ps)
                 (reverse cells)
                 (loop (cdr ps) 
                       (cdr vs)
                       ;; Call parameter with 2 args to get converted value
                       (cons (cons (car ps) 
                                   (cons (car ps) ((car ps) (car vs) #f)))
                             cells))))))
      ;; Extend environment and use dynamic-wind for proper unwinding
      (let ((new-env (append new-cells old-env)))
        (dynamic-wind
          (lambda () (set-car! *param-dynamic-env-box* new-env))
          body
          (lambda () (set-car! *param-dynamic-env-box* old-env)))))))

;; /**
;;  * Syntax for dynamic parameter binding.
;;  *
;;  * (parameterize ((param1 val1) (param2 val2) ...) body ...)
;;  *
;;  * Temporarily binds each parameter to its corresponding value
;;  * for the dynamic extent of the body expressions.
;;  */
(define-syntax parameterize
  (syntax-rules ()
    ((parameterize () body ...)
     (begin body ...))
    ((parameterize ((param val) ...) body ...)
     (param-dynamic-bind (list param ...)
                         (list val ...)
                         (lambda () body ...)))))
