;; (scheme repl) library - R7RS standard + Extensions
;;
;; Provides the interaction environment for REPL use.
;; This implementation makes (scheme repl) a "batteries included" library
;; for the REPL environment, including (scheme base) and (scheme-js ...) extras.

(define-library (scheme repl)
  (import (scheme base))
  (import (scheme-js interop))
  (import (scheme-js promise))

  (export
    interaction-environment

    ;; Re-export (scheme base)
    eq? eqv? equal?
    + - * / = < > <= >=
    number? complex? real? rational? integer? exact-integer? exact? inexact? finite? infinite? nan?
    zero? positive? negative? odd? even?
    abs quotient remainder modulo floor ceiling truncate round max min gcd lcm expt sqrt square exact-integer-sqrt
    exact inexact floor/ floor-quotient floor-remainder truncate/ truncate-quotient truncate-remainder
    numerator denominator
    make-rectangular make-polar real-part imag-part magnitude angle
    bytevector? make-bytevector bytevector bytevector-length bytevector-u8-ref bytevector-u8-set!
    bytevector-copy bytevector-copy! bytevector-append utf8->string string->utf8
    not boolean? boolean=?
    cons car cdr pair? null? list? set-car! set-cdr! list append
    length list-ref list-tail reverse list-copy make-list list-set! memq memv member assq assv assoc
    caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    symbol? symbol=? symbol->string string->symbol
    char? char=? char<? char>? char<=? char>=? char->integer integer->char
    string? make-string string string-length string-ref string=? string<? string>? string<=? string>=?
    substring string-append string-copy string->list list->string number->string string->number
    string-upcase string-downcase string-foldcase
    vector? make-vector vector vector-length vector-ref vector-set! vector-fill! vector-copy vector-copy! vector-append
    vector->list list->vector vector->string string->vector
    apply map for-each string-map string-for-each vector-map vector-for-each
    call-with-current-continuation call/cc dynamic-wind values call-with-values procedure?
    make-parameter parameterize param-dynamic-bind
    raise raise-continuable with-exception-handler error error-object? error-object-message error-object-irritants
    port? input-port? output-port? textual-port? binary-port? input-port-open? output-port-open?
    current-input-port current-output-port current-error-port close-port close-input-port close-output-port flush-output-port
    open-input-string open-output-string get-output-string open-input-bytevector open-output-bytevector get-output-bytevector
    eof-object eof-object?
    read-char peek-char char-ready? read-line read-string read read-u8 peek-u8 u8-ready? read-bytevector
    write-char write-string display newline write write-simple write-shared write-u8 write-bytevector
    define set! lambda if begin quote quasiquote unquote unquote-splicing
    define-syntax let-syntax letrec-syntax
    and or cond case do when unless guard
    let let* letrec letrec* let-values let*-values define-values define-record-type

    ;; Re-export (scheme-js interop)
    js-eval js-ref js-set!

    ;; Re-export (scheme-js promise)
    js-promise? make-js-promise js-promise-resolve js-promise-reject
    js-promise-then js-promise-catch js-promise-finally
    js-promise-all js-promise-race js-promise-all-settled
    js-promise-map js-promise-chain async-lambda
  )
  (begin
    ;; interaction-environment is already a primitive in our system
  ))
