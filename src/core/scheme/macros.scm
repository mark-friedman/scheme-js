;; Core Macros
;; Fundamental macros for bootstrapping

;; /**
;;  * Logical AND macro.
;;  * Short-circuits evaluation: returns #f immediately if any expression evaluates to #f.
;;  * Returns the value of the last expression if all are true.
;;  *
;;  * @param {boolean} ...test - Expressions to evaluate.
;;  * @returns {boolean|*} #f if any test is false, otherwise the value of the last test.
;;  */
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

;; /**
;;  * Binds variables to values within a scope.
;;  * Supports both standard let (parallel binding) and named let (for recursion).
;;  *
;;  * @param {symbol|list} tag_or_bindings - Either a name for the loop (named let) or the list of bindings.
;;  * @param {list} [bindings] - List of ((name val) ...) bindings (if named let).
;;  * @param {...*} body - Body expressions to evaluate.
;;  * @returns {*} Result of the last expression in the body.
;;  */
(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...) val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...) body1 body2 ...)))
        tag)
      val ...))))

;; /**
;;  * Recursive binding construct.
;;  * Allows defining mutually recursive functions.
;;  *
;;  * @param {list} bindings - List of ((var init) ...) bindings.
;;  * @param {...*} body - Body expressions to evaluate.
;;  * @returns {*} Result of the last expression in the body.
;;  */
(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var init) ...) body ...)
     (let ((var 'undefined) ...)
       (set! var init) ...
       (let () body ...)))))

;; /**
;;  * Conditional expression.
;;  * Evaluates clauses sequentially until one's test evaluates to true.
;;  *
;;  * @param {...list} clauses - List of (test expression...) clauses.
;;  * @returns {*} Result of the evaluated clause, or undefined if no else clause matches.
;;  */
(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

;; /**
;;  * Internal helper for defining record fields.
;;  * Defines accessors and modifiers for a specific field of a record type.
;;  *
;;  * @param {symbol} type - The record type name.
;;  * @param {symbol} field-tag - The field name.
;;  * @param {symbol} accessor - The name of the accessor function.
;;  * @param {symbol} [modifier] - The name of the modifier function (optional).
;;  */
(define-syntax define-record-field
  (syntax-rules ()
    ((define-record-field type field-tag accessor)
     (define accessor (record-accessor type 'field-tag)))
    ((define-record-field type field-tag accessor modifier)
     (begin
       (define accessor (record-accessor type 'field-tag))
       (define modifier (record-modifier type 'field-tag))))))

;; /**
;;  * Defines a new record type.
;;  * Creates a constructor, a type predicate, and accessors/modifiers for fields.
;;  *
;;  * @param {symbol} type - The name of the new record type.
;;  * @param {list} constructor - (constructor tag ...) specification.
;;  * @param {symbol} predicate - Name for the type predicate (e.g. my-type?).
;;  * @param {...list} fields - Field specifications (tag accessor [modifier]).
;;  */
(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor constructor-tag ...)
       predicate
       (field-tag accessor . more) ...)
     (begin
       (define type (make-record-type 'type '(field-tag ...)))
       (define constructor (record-constructor type))
       (define predicate (record-predicate type))
       (define-record-field type field-tag accessor . more) ...))))
