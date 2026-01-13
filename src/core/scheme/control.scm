;; Standard Control Macros
;; (when, unless, do, case, guard, let-values, let*-values, define-values)
;; Note: `or` and `let*` are defined in macros.scm

;; /**
;;  * One-armed conditional.
;;  * Evaluates body if test is true.
;;  *
;;  * @param {expression} test - Conditional test.
;;  * @param {...expression} body - Expressions to evaluate if test is true.
;;  */
(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))

;; /**
;;  * One-armed conditional (negated).
;;  * Evaluates body if test is false.
;;  *
;;  * @param {expression} test - Conditional test.
;;  * @param {...expression} body - Expressions to evaluate if test is false.
;;  */
(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if test
         #f ;; unspecified
         (begin result1 result2 ...)))))

;; /**
;;  * Iteration construct.
;;  *
;;  * @param {list} ((var init [step]) ...) - Variable bindings.
;;  * @param {list} (test expression ...) - Termination condition and return value.
;;  * @param {...expression} commands - Body expressions.
;;  */

(define-syntax do
  (syntax-rules ()
    ;; Normalizer: (var init step) -> keep as is
    ((do "normalize" ((var init step) . rest) (done ...) clause . body)
     (do "normalize" rest (done ... (var init step)) clause . body))
    
    ;; Normalizer: (var init) -> add var as step
    ((do "normalize" ((var init) . rest) (done ...) clause . body)
     (do "normalize" rest (done ... (var init var)) clause . body))

    ;; Base case: all normalized, generate loop
    ((do "normalize" () ((var init step) ...) (test expr ...) . body)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin
                 (if #f #f)
                 expr ...)
               (begin
                 (begin . body)
                 (loop step ...))))))
       (loop init ...)))

    ;; Entry point: start normalizing bindings
    ((do ((var init . step) ...) (test . exprs) . commands)
     (do "normalize" ((var init . step) ...) () (test . exprs) . commands))))

;; =============================================================================
;; Multiple Value Binding Forms
;; =============================================================================

;; /**
;;  * Let-values binding form.
;;  * Binds results from producers that return multiple values.
;;  *
;;  * @param {list} ((formals producer) ...) - Binding clauses.
;;  * @param {...expression} body - Body expressions.
;;  */
(define-syntax let-values
  (syntax-rules ()
    ;; Empty bindings
    ((let-values () body ...)
     (let () body ...))
    ;; Single binding
    ((let-values ((formals producer)) body ...)
     (call-with-values
       (lambda () producer)
       (lambda formals body ...)))
    ;; Multiple bindings - nest them
    ((let-values ((formals producer) rest ...) body ...)
     (call-with-values
       (lambda () producer)
       (lambda formals (let-values (rest ...) body ...))))))

;; /**
;;  * Sequential let-values binding form.
;;  * Like let-values but each clause sees the bindings of previous clauses.
;;  *
;;  * @param {list} ((formals producer) ...) - Binding clauses.
;;  * @param {...expression} body - Body expressions.
;;  */
(define-syntax let*-values
  (syntax-rules ()
    ;; Empty bindings
    ((let*-values () body ...)
     (let () body ...))
    ;; One or more bindings - expand sequentially
    ((let*-values ((formals producer) rest ...) body ...)
     (call-with-values
       (lambda () producer)
       (lambda formals (let*-values (rest ...) body ...))))))

;; /**
;;  * Define multiple variables from multiple values.
;;  * Supports various patterns: (define-values () expr), (define-values (a) expr),
;;  * (define-values (a b) expr), (define-values (a b . rest) expr), (define-values x expr).
;;  *
;;  * @param {list|symbol} formals - Variable(s) to define.
;;  * @param {expression} expr - Expression producing multiple values.
;;  */
(define-syntax define-values
  (syntax-rules ()
    ;; Empty formals - just evaluate for side effects
    ((define-values () expr)
     (call-with-values (lambda () expr) (lambda () (if #f #f))))
    ;; Single variable in parens
    ((define-values (var) expr)
     (define var (call-with-values (lambda () expr) (lambda (x) x))))
    ;; Two variables - use begin+define pattern with temp list
    ((define-values (var1 var2) expr)
     (begin
       (define temp-vals (call-with-values (lambda () expr) list))
       (define var1 (car temp-vals))
       (define var2 (cadr temp-vals))))
    ;; Three variables - use a temporary list to capture values
    ((define-values (var1 var2 var3) expr)
     (begin
       (define temp-vals (call-with-values (lambda () expr) list))
       (define var1 (car temp-vals))
       (define var2 (cadr temp-vals))
       (define var3 (caddr temp-vals))))
    ;; Two variables with rest - use a temporary list to capture values
    ((define-values (var1 var2 . rest) expr)
     (begin
       (define temp-vals (call-with-values (lambda () expr) list))
       (define var1 (car temp-vals))
       (define var2 (cadr temp-vals))
       (define rest (cddr temp-vals))))
    ;; Single variable without parens (gets all values as list)
    ;; NOTE: This pattern must be LAST as bare `var` matches any expression
    ((define-values var expr)
     (define var (call-with-values (lambda () expr) list)))))

;; /**
;;  * Case dispatch.
;;  * Dispatches based on value equality (using memv).
;;  * Supports => syntax to apply a procedure to the matched key.
;;  *
;;  * @param {expression} key - Value to match.
;;  * @param {...list} clauses - ((datum ...) result1 result2 ...) or ((datum ...) => proc).
;;  */
(define-syntax case
  (syntax-rules (else =>)
    ;; else with => - apply proc to key (must come before plain else)
    ((case "dispatch" key
       (else => proc))
     (proc key))
    ;; else clause - always matches
    ((case "dispatch" key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ;; Single clause with => - apply proc to key if match
    ((case "dispatch" key
       ((atoms ...) => proc))
     (if (memv key '(atoms ...))
         (proc key)))
    ;; => clause with more clauses following
    ((case "dispatch" key
       ((atoms ...) => proc)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (proc key)
         (case "dispatch" key clause clauses ...)))
    ;; Single clause with results
    ((case "dispatch" key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ;; Multiple clauses with results
    ((case "dispatch" key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case "dispatch" key clause clauses ...)))
    ;; No match case
    ((case "dispatch" key)
     (if #f #t))
    ;; Entry point - bind key once
    ((case key
       clauses ...)
     (let ((atom-key key))
       (case "dispatch" atom-key clauses ...)))))

;; =============================================================================
;; Exception Handling
;; =============================================================================

;; /**
;;  * Exception handling construct (R7RS).
;;  * Catches exceptions and dispatches based on condition clauses.
;;  *
;;  * @param {symbol} var - Variable to bind to exception.
;;  * @param {...clause} clauses - Exception handling clauses.
;;  * @param {...expression} body - Body expressions to protect.
;;  */
(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) body ...)
     (call/cc
       (lambda (guard-exit)
         (with-exception-handler
           (lambda (condition)
             (let ((var condition))
               (guard-clauses guard-exit var clause ...)))
           (lambda ()
             body ...)))))))

;; /**
;;  * Helper macro to process guard clauses.
;;  * Evaluates clauses in order, re-raising if none match.
;;  */
(define-syntax guard-clauses
  (syntax-rules (else =>)
    ;; else clause - always matches
    ((guard-clauses exit var (else result1 result2 ...))
     (exit (begin result1 result2 ...)))
    ;; => clause with more clauses following (must come before non-=> versions!)
    ((guard-clauses exit var (test => proc) clause ...)
     (let ((temp test))
       (if temp
           (exit (proc temp))
           (guard-clauses exit var clause ...))))
    ;; => clause - apply proc to result if test is true (single clause)
    ((guard-clauses exit var (test => proc))
     (let ((temp test))
       (if temp
           (exit (proc temp))
           (raise var))))
    ;; test with results and more clauses
    ((guard-clauses exit var (test result1 result2 ...) clause ...)
     (if test
         (exit (begin result1 result2 ...))
         (guard-clauses exit var clause ...)))
    ;; test with results - return results if test is true (single clause)
    ((guard-clauses exit var (test result1 result2 ...))
     (if test
         (exit (begin result1 result2 ...))
         (raise var)))
    ;; test only with more clauses
    ((guard-clauses exit var (test) clause ...)
     (let ((temp test))
       (if temp
           (exit temp)
           (guard-clauses exit var clause ...))))
    ;; test only - return result of test if true (single clause)
    ((guard-clauses exit var (test))
     (let ((temp test))
       (if temp
           (exit temp)
           (raise var))))
    ;; no clauses matched - re-raise
    ((guard-clauses exit var)
     (raise var))))
