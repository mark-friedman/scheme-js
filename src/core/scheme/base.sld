;; R7RS (scheme base) library
;; 
;; This re-exports primitives from the runtime and core implementations.
;; Per R7RS Appendix A.

(define-library (scheme base)
  (import (scheme primitives))
  (import (scheme core))
  (import (scheme control))

  (export
    ;; Equivalence predicates
    eq? eqv? equal?
    
    ;; Numbers - basic arithmetic
    + - * /
    
    ;; Numbers - comparison (variadic, from core)
    = < > <= >=
    
    ;; Numbers - predicates (from primitives)
    number? integer? real? rational? exact-integer?
    finite? infinite? nan?
    
    ;; Numbers - predicates (from core)
    zero? positive? negative? odd? even?
    
    ;; Numbers - operations
    abs quotient remainder modulo
    floor ceiling truncate round
    max min gcd lcm
    expt sqrt
    
    ;; Booleans
    not boolean? boolean=?
    
    ;; Pairs and lists - basic
    cons car cdr pair? null? list?
    set-car! set-cdr!
    list append
    
    ;; Pairs and lists - extended
    length list-ref list-tail reverse list-copy
    memq memv member
    assq assv assoc
    
    ;; Compound accessors (cxr) - depth 2-4
    caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    
    ;; Symbols
    symbol? symbol->string string->symbol
    
    ;; Strings  
    string? string-append number->string
    
    ;; Vectors
    vector? make-vector vector vector-length
    vector-ref vector-set!
    vector->list list->vector
    
    ;; Control
    apply map for-each
    call-with-current-continuation call/cc
    dynamic-wind
    values call-with-values
    procedure?
    
    ;; Exceptions
    raise raise-continuable
    with-exception-handler
    error
    error-object? error-object-message error-object-irritants
    
    ;; I/O (basic)
    display newline
    
    ;; Syntax (Macros & Special Forms)
    define set! lambda if begin quote quasiquote unquote unquote-splicing
    define-syntax let-syntax letrec-syntax
    and or cond case do when unless guard
    let let* letrec
    define-record-type
  )
  
  ;; The actual bindings come from the runtime primitives.
  ;; This library just declares what (scheme base) exports.
  (begin
    ;; No definitions needed - primitives are injected by the runtime
  ))
