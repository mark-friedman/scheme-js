;; R7RS Parameter Objects (§4.2.6)
;;
;; Implementation following SRFI-39 / R7RS section 7.3.
;; Uses dynamic-wind for proper unwinding on control flow exit.

;; =============================================================================
;; Dynamic Environment
;; =============================================================================

;; A parameter is known by its global cell, a pair of its converter and its
;; global value: the procedure a parameter object is may be replaced -- by its
;; compiled form, or by its closure again while a program is debugged -- but
;; its cell stays. The dynamic environment is a list of (global-cell .
;; binding), innermost first, where a binding is a cell of the same shape.

;; Use a box (list) to hold the environment so we can mutate the contents
;; without changing the binding. This ensures multiple closures see the update.
(define *param-dynamic-env-box* (list '()))

;; /**
;;  * The cell holding a parameter's value now: the innermost `parameterize`
;;  * binding of it, or else its global cell.
;;  *
;;  * @param {pair} global-cell - The parameter's global cell.
;;  * @returns {pair} The cell containing the current value.
;;  */
(define (param-dynamic-lookup global-cell)
  (let loop ((env (car *param-dynamic-env-box*)))
    (cond ((null? env) global-cell)
          ((eq? (caar env) global-cell) (cdar env))
          (else (loop (cdr env))))))

;; /**
;;  * What a parameter object does when called, given its global cell and the
;;  * arguments it was called with:
;;  * - with none, returns the current value;
;;  * - with one, sets the current value, through the converter, and returns
;;  *   unspecified;
;;  * - with two, which only `parameterize` does, returns the global cell
;;  *   paired with the first converted: what it binds, and to what.
;;  *
;;  * A top-level procedure, so that a parameter defined as one -- the current
;;  * ports, in ports.scm -- is compiled with its library, where the closure
;;  * `make-parameter` makes while a library loads, interpreted, would not be.
;;  *
;;  * @param {pair} global-cell - The parameter's global cell.
;;  * @param {list} args - The arguments.
;;  * @returns {*}
;;  */
(define (parameter-dispatch global-cell args)
  (cond ((null? args) (cdr (param-dynamic-lookup global-cell)))
        ((null? (cdr args))
         (set-cdr! (param-dynamic-lookup global-cell) ((car global-cell) (car args))))
        (else (cons global-cell ((car global-cell) (car args))))))

;; /**
;;  * A parameter's global cell, holding its converter and its initial value
;;  * converted.
;;  * @param {procedure} converter - The converter.
;;  * @param {*} init - The initial value.
;;  * @returns {pair} The cell.
;;  */
(define (parameter-cell converter init)
  (cons converter (converter init)))

;; =============================================================================
;; make-parameter
;; =============================================================================

;; /**
;;  * Creates a new parameter object: a procedure doing what
;;  * `parameter-dispatch` says.
;;  *
;;  * @param {*} init - Initial value.
;;  * @param {procedure} [converter] - Optional conversion procedure.
;;  * @returns {procedure} The parameter object.
;;  */
(define (make-parameter init . conv)
  (let ((global-cell (parameter-cell (if (null? conv) (lambda (x) x) (car conv)) init)))
    (lambda args (parameter-dispatch global-cell args))))

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
  (let* ((old-env (car *param-dynamic-env-box*))
         ;; Each parameter, asked in order, gives its global cell and the value
         ;; converted; each is bound to a fresh cell of its own.
         (new-env (let loop ((ps params) (vs values) (bound '()))
                    (if (null? ps)
                        (append (reverse bound) old-env)
                        (let ((answer ((car ps) (car vs) #f)))
                          (loop (cdr ps) (cdr vs)
                                (cons (cons (car answer) (cons (caar answer) (cdr answer)))
                                      bound)))))))
    (dynamic-wind
      (lambda () (set-car! *param-dynamic-env-box* new-env))
      body
      (lambda () (set-car! *param-dynamic-env-box* old-env)))))

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
