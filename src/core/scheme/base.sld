;; R7RS (scheme base) library stub
;; 
;; This is a minimal version that re-exports primitives from the runtime.
;; Will be expanded to full R7RS base in Phase 8.

(define-library (scheme base)
  (export
    ;; Equivalence predicates
    eq? eqv? equal?
    
    ;; Numbers
    + - * / < > <= >= = 
    number? integer? zero? positive? negative?
    abs modulo remainder quotient
    
    ;; Booleans
    not boolean?
    
    ;; Pairs and lists
    cons car cdr pair? null? list?
    set-car! set-cdr!
    list length append reverse
    list-ref list-tail
    memq memv member
    assq assv assoc
    
    ;; Symbols
    symbol? symbol->string string->symbol
    
    ;; Characters
    char? char=? char<? char>? char<=? char>=?
    
    ;; Strings  
    string? string-length string-ref string-set!
    string=? string<? string>?
    string-append substring
    string->list list->string
    string-copy string-fill!
    number->string string->number
    
    ;; Vectors
    vector? make-vector vector vector-length
    vector-ref vector-set!
    vector->list list->vector
    
    ;; Control
    procedure? apply
    map for-each
    call-with-current-continuation call/cc
    dynamic-wind
    
    ;; I/O (basic)
    display newline
    
    ;; Syntax (re-exported from boot)
    ;; These are macros defined in boot.scm:
    ;; and, or, let, let*, letrec, cond, case, when, unless, begin
  )
  
  ;; The actual bindings come from the runtime primitives.
  ;; This library just declares what (scheme base) exports.
  (begin
    ;; No definitions needed - primitives are injected by the runtime
  ))
