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
    number? complex? real? rational? integer? exact-integer?
    exact? inexact?
    finite? infinite? nan?
    
    ;; Numbers - predicates (from core)
    zero? positive? negative? odd? even?
    
    ;; Numbers - operations
    abs quotient remainder modulo
    floor ceiling truncate round
    max min gcd lcm
    expt sqrt square
    exact-integer-sqrt
    exact inexact
    floor/ floor-quotient floor-remainder
    truncate/ truncate-quotient truncate-remainder
    
    ;; Rational number procedures
    numerator denominator
    
    ;; Complex number procedures
    make-rectangular make-polar
    real-part imag-part magnitude angle
    
    ;; Bytevectors
    bytevector? make-bytevector bytevector bytevector-length
    bytevector-u8-ref bytevector-u8-set!
    bytevector-copy bytevector-copy! bytevector-append
    utf8->string string->utf8
    
    ;; Booleans
    not boolean? boolean=?
    
    ;; Pairs and lists - basic
    cons car cdr pair? null? list?
    set-car! set-cdr!
    list append
    
    ;; Pairs and lists - extended
    length list-ref list-tail reverse list-copy
    make-list list-set!
    memq memv member
    assq assv assoc
    
    ;; Compound accessors (cxr) - depth 2-4
    caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    
    ;; Symbols
    symbol? symbol=? symbol->string string->symbol
    
    ;; Characters
    char? char=? char<? char>? char<=? char>=?
    char->integer integer->char
    
    ;; Strings  
    string? make-string string string-length string-ref
    string=? string<? string>? string<=? string>=?
    substring string-append string-copy
    string->list list->string
    number->string string->number
    string-upcase string-downcase string-foldcase
    
    ;; Vectors
    vector? make-vector vector vector-length
    vector-ref vector-set! vector-fill!
    vector-copy vector-copy! vector-append
    vector->list list->vector
    vector->string string->vector
    
    ;; Control
    apply map for-each
    string-map string-for-each
    vector-map vector-for-each
    call-with-current-continuation call/cc
    dynamic-wind
    values call-with-values
    procedure?
    make-parameter parameterize param-dynamic-bind
    
    ;; Exceptions
    raise raise-continuable
    with-exception-handler
    error
    error-object? error-object-message error-object-irritants
    
    ;; I/O - Ports
    port? input-port? output-port?
    textual-port? binary-port?
    input-port-open? output-port-open?
    current-input-port current-output-port current-error-port
    close-port close-input-port close-output-port
    flush-output-port
    
    ;; I/O - String Ports
    open-input-string open-output-string get-output-string
    
    ;; I/O - Bytevector Ports (Binary I/O)
    open-input-bytevector open-output-bytevector get-output-bytevector
    
    ;; I/O - EOF
    eof-object eof-object?
    
    ;; I/O - Input
    read-char peek-char char-ready?
    read-line read-string
    read
    
    ;; I/O - Binary Input
    read-u8 peek-u8 u8-ready?
    read-bytevector
    
    ;; I/O - Output
    write-char write-string
    display newline write
    write-simple write-shared
    
    ;; I/O - Binary Output
    write-u8 write-bytevector
    
    ;; Syntax (Macros & Special Forms)
    define set! lambda if begin quote quasiquote unquote unquote-splicing
    define-syntax let-syntax letrec-syntax
    and or cond case do when unless guard
    let let* letrec letrec*
    let-values let*-values define-values
    define-record-type
  )
  
  ;; The actual bindings come from the runtime primitives.
  ;; This library just declares what (scheme base) exports.
  (begin
    ;; No definitions needed - primitives are injected by the runtime
  ))
