;; Standard Control Macros
;; (when, unless, or, let*, do, case)

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
;;  * Short-circuit OR.
;;  * Returns the first true value, or #f if all are false.
;;  *
;;  * @param {...expression} test - Expressions to test.
;;  */
(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

;; /**
;;  * Sequential binding.
;;  * Binds variables sequentially, allowing later bindings to refer to earlier ones.
;;  *
;;  * @param {list} bindings - List of ((variable init) ...) bindings.
;;  * @param {...expression} body - Body to evaluate.
;;  */
(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))

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

;; /**
;;  * Case dispatch.
;;  * Dispatches based on value equality (using memv).
;;  *
;;  * @param {expression} key - Value to match.
;;  * @param {...list} clauses - ((datum ...) result1 result2 ...).
;;  */
(define-syntax case
  (syntax-rules (else =>)
    ((case "dispatch" key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case "dispatch" key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case "dispatch" key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case "dispatch" key clause clauses ...)))
    ((case "dispatch" key)
     (if #f #t)) ;; no match
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
    ;; => clause - apply proc to result if test is true
    ((guard-clauses exit var (test => proc))
     (let ((temp test))
       (if temp
           (exit (proc temp))
           (raise-continuable var))))
    ;; test with results - return results if test is true
    ((guard-clauses exit var (test result1 result2 ...))
     (if test
         (exit (begin result1 result2 ...))
         (raise-continuable var)))
    ((guard-clauses exit var (test result1 result2 ...) clause ...)
     (if test
         (exit (begin result1 result2 ...))
         (guard-clauses exit var clause ...)))
    
    ;; test only - return result of test if true
    ((guard-clauses exit var (test))
     (let ((temp test))
       (if temp
           (exit temp)
           (raise-continuable var))))
    ((guard-clauses exit var (test) clause ...)
     (let ((temp test))
       (if temp
           (exit temp)
           (guard-clauses exit var clause ...))))

    ;; no clauses matched - re-raise
    ((guard-clauses exit var)
     (raise-continuable var))))
