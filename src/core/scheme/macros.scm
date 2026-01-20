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
;;  * Logical OR macro.
;;  * Short-circuits evaluation: returns the first true value, or #f if all are false.
;;  *
;;  * @param {boolean} ...test - Expressions to evaluate.
;;  * @returns {boolean|*} First true value, or #f.
;;  */
(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

;; /**
;;  * Sequential binding macro.
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
;;  * Sequential recursive binding construct.
;;  * Like letrec but guarantees left-to-right evaluation of inits.
;;  *
;;  * @param {list} bindings - List of ((var init) ...) bindings.
;;  * @param {...*} body - Body expressions to evaluate.
;;  * @returns {*} Result of the last expression in the body.
;;  */
(define-syntax letrec*
  (syntax-rules ()
    ;; Empty case
    ((letrec* () body ...)
     (let () body ...))
    ;; Use a helper to collect bindings, then expand
    ((letrec* ((var init) ...) body ...)
     (let ((var 'undefined) ...)
       (letrec* "init" ((var init) ...) body ...)))
    ;; Helper: initialize each binding sequentially, then run body
    ((letrec* "init" () body ...)
     (let () body ...))
    ((letrec* "init" ((var init) rest ...) body ...)
     (begin
       (set! var init)
       (letrec* "init" (rest ...) body ...)))))

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

;; /**
;;  * Internal helper for defining class fields.
;;  */
(define-syntax define-class-field
  (syntax-rules ()
    ((define-class-field type field-tag accessor)
     (define accessor (record-accessor type 'field-tag)))
    ((define-class-field type field-tag accessor modifier)
     (begin
       (define accessor (record-accessor type 'field-tag))
       (define modifier (record-modifier type 'field-tag))))))

;; /**
;;  * Internal helper for defining class methods.
;;  */
(define-syntax define-class-method
  (syntax-rules ()
    ;; With parent (ignored for now - super access via class-super-call primitive)
    ((define-class-method type parent (name (param ...) body ...))
     (class-method-set! type 'name (lambda (param ...) body ...)))
    ;; Without parent
    ((define-class-method type (name (param ...) body ...))
     (class-method-set! type 'name (lambda (param ...) body ...)))))

;; /**
;;  * Defines a new JS-compatible class.
;;  * Creates a constructor, a type predicate, accessors/modifiers for fields,
;;  * and methods on the class prototype.
;;  *
;;  * Syntax:
;;  * (define-class type [parent]
;;  *   constructor-name
;;  *   predicate
;;  *   (fields (field-tag accessor [modifier]) ...)
;;  *   [(constructor (params ...) body ...)]
;;  *   (methods (method-name (params ...) body ...) ...))
;;  *
;;  * If constructor clause is omitted:
;;  *   - Without parent: fields become constructor params, sets them automatically
;;  *   - With parent: passes all field params to super, then sets own fields
;;  *
;;  * If constructor clause is present:
;;  *   - With parent: must call (super args...) before using this
;;  *   - Body can initialize fields and perform other setup
;;  */
(define-syntax define-class
  (syntax-rules (fields methods constructor super)
    ;; Form with parent + constructor with ONLY (super ...) - no init body
    ((define-class type parent
       constructor-name
       predicate
       (fields (field-tag-spec accessor . more) ...)
       (constructor (param ...) (super super-arg ...))
       (methods (method-name method-params . method-body) ...))
     (begin
       (define type (make-class-with-init 'type parent '(field-tag-spec ...) '(param ...)
                      (lambda (param ...) (vector super-arg ...))  ;; superArgsFn
                      #f))                                         ;; no initFn
       (define constructor-name type)
       (define predicate (record-predicate type))
       (define-class-field type field-tag-spec accessor . more) ...
       (define-class-method type parent (method-name method-params . method-body)) ...))

    ;; Form with parent + constructor with (super ...) AND body
    ((define-class type parent
       constructor-name
       predicate
       (fields (field-tag-spec accessor . more) ...)
       (constructor (param ...) (super super-arg ...) body0 body ...)
       (methods (method-name method-params . method-body) ...))
     (begin
       (define type (make-class-with-init 'type parent '(field-tag-spec ...) '(param ...)
                      (lambda (param ...) (vector super-arg ...))  ;; superArgsFn
                      (lambda (param ...) body0 body ...)))        ;; initFn
       (define constructor-name type)
       (define predicate (record-predicate type))
       (define-class-field type field-tag-spec accessor . more) ...
       (define-class-method type parent (method-name method-params . method-body)) ...))

    ;; Form with parent + constructor WITHOUT (super ...) - pass all args to super
    ((define-class type parent
       constructor-name
       predicate
       (fields (field-tag-spec accessor . more) ...)
       (constructor (param ...) body ...)
       (methods (method-name method-params . method-body) ...))
     (begin
       (define type (make-class-with-init 'type parent '(field-tag-spec ...) '(param ...)
                      #f                                  ;; superArgsFn = null, use all args
                      (lambda (param ...) body ...)))     ;; initFn
       (define constructor-name type)
       (define predicate (record-predicate type))
       (define-class-field type field-tag-spec accessor . more) ...
       (define-class-method type parent (method-name method-params . method-body)) ...))

    ;; Form without parent + custom constructor clause
    ((define-class type
       constructor-name
       predicate
       (fields (field-tag-spec accessor . more) ...)
       (constructor (param ...) body ...)
       (methods (method-name method-params . method-body) ...))
     (begin
       (define type (make-class-with-init 'type #f '(field-tag-spec ...) '(param ...)
                      #f                                  ;; no super for no-parent
                      (lambda (param ...) body ...)))     ;; initFn
       (define constructor-name type)
       (define predicate (record-predicate type))
       (define-class-field type field-tag-spec accessor . more) ...
       (define-class-method type (method-name method-params . method-body)) ...))

    ;; Form with parent, no constructor clause (default behavior)
    ((define-class type parent
       constructor-name
       predicate
       (fields (field-tag-spec accessor . more) ...)
       (methods (method-name method-params . method-body) ...))
     (begin
       (define type (make-class 'type parent '(field-tag-spec ...) '(field-tag-spec ...)))
       (define constructor-name (record-constructor type))
       (define predicate (record-predicate type))
       (define-class-field type field-tag-spec accessor . more) ...
       (define-class-method type parent (method-name method-params . method-body)) ...))

    ;; Form without parent, no constructor clause (default behavior)
    ((define-class type
       constructor-name
       predicate
       (fields (field-tag-spec accessor . more) ...)
       (methods (method-name method-params . method-body) ...))
     (begin
       (define type (make-class 'type #f '(field-tag-spec ...) '(field-tag-spec ...)))
       (define constructor-name (record-constructor type))
       (define predicate (record-predicate type))
       (define-class-field type field-tag-spec accessor . more) ...
       (define-class-method type (method-name method-params . method-body)) ...))))

