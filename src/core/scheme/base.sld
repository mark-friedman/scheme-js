;; R7RS (scheme base) library stub
;; 
;; This is a minimal version that re-exports primitives from the runtime.
;; Will be expanded to full R7RS base in Phase 8.

(define-library (scheme base)
  (import (scheme primitives))
  (import (scheme core))

  (export
    ;; Equivalence predicates
    eq? eqv? equal?
    
    ;; Numbers
    + - * / < > = modulo
    
    ;; Booleans
    
    ;; Pairs and lists
    cons car cdr pair? null? list?
    set-car! set-cdr!
    list append
    cadr cddr caddr cdddr cadddr
    
    ;; Symbols
    symbol->string string->symbol
    
    ;; Characters
    
    ;; Strings  
    string? string-append number->string
    
    ;; Vectors
    vector? make-vector vector vector-length
    vector-ref vector-set!
    vector->list list->vector
    
    ;; Control
    apply map
    call-with-current-continuation call/cc
    dynamic-wind
    values call-with-values
    
    ;; I/O (basic)
    display newline
    
    ;; Syntax (Macros & Special Forms)
    define set! lambda if begin quote quasiquote unquote unquote-splicing
    define-syntax let-syntax letrec-syntax ;; syntax-rules
    and or cond case do when unless
    let let* letrec
    define-record-type
  )
  
  ;; The actual bindings come from the runtime primitives.
  ;; This library just declares what (scheme base) exports.
  (begin
    ;; No definitions needed - primitives are injected by the runtime
  ))
