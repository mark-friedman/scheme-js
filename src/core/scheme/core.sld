(define-library (scheme core)
  (import (scheme primitives))
  
  ;; Include separate Scheme files in dependency order
  (include "macros.scm")     ; Core macros: and, let, letrec, cond
  (include "equality.scm")   ; equal?
  (include "cxr.scm")        ; caar, cadr, etc.
  (include "numbers.scm")    ; =, <, >, predicates, min/max
  (include "list.scm")       ; map, for-each, memq, assq, etc.
  (include "parameter.scm")  ; make-parameter, parameterize
  (include "ports.scm")      ; the current ports, reading and writing them, call-with-port, the file procedures
  
  (export
    ;; Macros
    and or let let* letrec cond syntax-error include include-ci
    define-record-type define-record-field
    define-class define-class-field define-class-method
    
    ;; Deep equality
    equal?
    
    ;; List operations
    map for-each
    string-map string-for-each
    vector-map vector-for-each
    call-with-port call-with-input-file call-with-output-file
    with-input-from-file with-output-to-file
    current-input-port current-output-port current-error-port
    read-char peek-char char-ready? read-line read-string
    read-u8 peek-u8 u8-ready? read-bytevector read-bytevector! read
    write-char write-string write-u8 write-bytevector
    newline display write write-simple write-shared flush-output-port
    memq memv member
    assq assv assoc
    length list-ref list-tail reverse list-copy
    make-list list-set!
    
    ;; Compound accessors (cxr)
    caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    
    ;; Comparison operators (variadic)
    = < > <= >=
    
    ;; Numeric predicates
    zero? positive? negative? odd? even?
    
    ;; Min/max
    max min
    
    ;; GCD/LCM
    gcd lcm rationalize
    
    ;; Rounding
    round inexact->exact
    
    ;; Parameter objects
    make-parameter parameterize param-dynamic-bind
    
    ;; Misc
    native-report-test-result
  )
)

