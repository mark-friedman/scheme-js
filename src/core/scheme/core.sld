(define-library (scheme core)
  (import (scheme primitives))
  
  ;; Include separate Scheme files in dependency order
  (include "macros.scm")     ; Core macros: and, let, letrec, cond
  (include "equality.scm")   ; equal?
  (include "cxr.scm")        ; caar, cadr, etc.
  (include "numbers.scm")    ; =, <, >, predicates, min/max
  (include "list.scm")       ; map, for-each, memq, assq, etc.
  (include "parameter.scm")  ; make-parameter, parameterize
  
  (export
    ;; Macros
    and or let let* letrec cond
    define-record-type define-record-field
    define-class define-class-field define-class-method
    
    ;; Deep equality
    equal?
    
    ;; List operations
    map for-each
    string-map string-for-each
    vector-map vector-for-each
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
    gcd lcm
    
    ;; Rounding
    round inexact->exact
    
    ;; Parameter objects
    make-parameter parameterize param-dynamic-bind
    
    ;; Misc
    native-report-test-result
  )
)

