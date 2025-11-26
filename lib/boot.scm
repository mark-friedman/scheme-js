; Bootstrapping code
; Loaded at startup

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...) val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...) body1 body2 ...)))
        tag)
      val ...))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var init) ...) body ...)
     (let ((var 'undefined) ...)
       (set! var init) ...
       (let () body ...)))))

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

(define-syntax define-record-field
  (syntax-rules ()
    ((define-record-field type field-tag accessor)
     (define accessor (record-accessor type 'field-tag)))
    ((define-record-field type field-tag accessor modifier)
     (begin
       (define accessor (record-accessor type 'field-tag))
       (define modifier (record-modifier type 'field-tag))))))

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

(define (equal? a b)
  (cond
    ((eqv? a b) #t)
    ((and (pair? a) (pair? b))
     (and (equal? (car a) (car b))
          (equal? (cdr a) (cdr b))))
    ((and (vector? a) (vector? b))
     (let ((len-a (vector-length a))
           (len-b (vector-length b)))
       (if (= len-a len-b)
           (let loop ((i 0))
             (if (= i len-a)
                 #t
                 (if (equal? (vector-ref a i) (vector-ref b i))
                     (loop (+ i 1))
                     #f)))
           #f)))
    ((and (string? a) (string? b))
     (eqv? a b)) ; Strings are primitives in JS, so eqv? (Object.is) works. 
                ; If we wrapped them, we'd need more. But our reader produces primitive strings.
                ; Wait, JS strings are value types, so === works.
    (else #f)))
