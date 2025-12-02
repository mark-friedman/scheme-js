;;; ----------------------------------------------------------------------------
;;; R7RS-small Compliance Test Suite
;;; Chapter 6: Standard Procedures
;;; ----------------------------------------------------------------------------

(test-group "6 Standard procedures"

  (test-group "6.1 Equivalence predicates"
    (test-group "eqv?"
      (test "eqv? symbols" #t (eqv? 'a 'a))
      (test "eqv? symbol fail" #f (eqv? 'a 'b))
      (test "eqv? numbers" #t (eqv? 2 2))
      (test "eqv? number/obj" #f (eqv? 2 2.0)) ;; Exactness matters
      (test "eqv? empty list" #t (eqv? '() '()))
      (test "eqv? pairs same" #f (eqv? (cons 1 2) (cons 1 2)))
      (test "eqv? #f" #f (eqv? #f 'nil)))

    (test-group "eq?"
      (test "eq? symbols" #t (eq? 'a 'a))
      (test "eq? list distinct" #f (eq? (list 'a) (list 'a)))
      (test "eq? same list" #t (let ((x '(a))) (eq? x x)))
      (test "eq? procedures" #t (let ((p (lambda (x) x))) (eq? p p))))

    (test-group "equal?"
      (test "equal? symbols" #t (equal? 'a 'a))
      (test "equal? lists" #t (equal? '(a (b) c) '(a (b) c)))
      (test "equal? strings" #t (equal? "abc" "abc"))
      (test "equal? vectors" #t (equal? #(a 10) #(a 10))))
  )

  (test-group "6.2 Numbers"
    (test-group "6.2.6 Numerical operations"
      (test "number?" #t (number? 3))
      (test "complex?" #t (complex? 3+4i))
      (test "real?" #t (real? 3))
      (test "integer?" #t (integer? 3.0))
      
      (test "exact?" #t (exact? 3))
      (test "inexact?" #t (inexact? 3.0))
      
      (test "max" 4 (max 3 4))
      (test "max mixed" 4.0 (max 3.9 4))
      
      (test "+" 7 (+ 3 4))
      (test "*" 4 (* 4))
      (test "-" -1 (- 3 4))
      (test "/" 0.5 (/ 3 6.0))
      
      (test "abs" 7 (abs -7))
      
      (test-group "Division"
        (let-values (((q r) (floor/ 5 2)))
          (test "floor/ q" 2 q)
          (test "floor/ r" 1 r))
        (test "quotient" 2 (quotient 5 2))
        (test "remainder" 1 (remainder 5 2))
        (test "modulo" 1 (modulo 5 2)))

      (test "gcd" 4 (gcd 32 -36))
      (test "lcm" 288 (lcm 32 -36))
      
      (test "numerator" 3 (numerator (/ 6 4)))
      (test "denominator" 2 (denominator (/ 6 4)))
      
      (test "floor" 3.0 (floor 3.5))
      (test "ceiling" 4.0 (ceiling 3.5))
      (test "truncate" 3.0 (truncate 3.5))
      (test "round" 4.0 (round 3.5))
      
      (test "exact-integer-sqrt" '(2 0) (call-with-values (lambda () (exact-integer-sqrt 4)) list))
      (test "expt" 16 (expt 2 4))
    )
    
    (test-group "6.2.7 Numerical input and output"
      (test "number->string" "100" (number->string 100))
      (test "string->number" 100 (string->number "100"))
      (test "string->number radix" 256 (string->number "100" 16)))
  )

  (test-group "6.3 Booleans"
    (test "not #t" #f (not #t))
    (test "not 3" #f (not 3))
    (test "not #f" #t (not #f))
    (test "boolean? #f" #t (boolean? #f))
    (test "boolean? 0" #f (boolean? 0)))

  (test-group "6.4 Pairs and lists"
    (test "pair?" #t (pair? '(a . b)))
    (test "cons" '(a) (cons 'a '()))
    (test "car" 'a (car '(a b c)))
    (test "cdr" '(b c) (cdr '(a b c)))
    (test "set-car!" '(3 b c) (let ((x (list 'a 'b 'c))) (set-car! x 3) x))
    (test "null?" #t (null? '()))
    (test "list?" #t (list? '(a b c)))
    (test "make-list" '(3 3) (make-list 2 3))
    (test "length" 3 (length '(a b c)))
    (test "append" '(a b c d) (append '(a) '(b c d)))
    (test "reverse" '(c b a) (reverse '(a b c)))
    (test "list-ref" 'c (list-ref '(a b c d) 2))
    (test "memq" '(a b c) (memq 'a '(a b c)))
    (test "memq fail" #f (memq 'b '(a)))
    (test "assq" '(a 1) (assq 'a '((a 1) (b 2)))))

  (test-group "6.5 Symbols"
    (test "symbol?" #t (symbol? 'foo))
    (test "symbol->string" "flying-fish" (symbol->string 'flying-fish))
    (test "string->symbol" 'Malvina (string->symbol "Malvina")))

  (test-group "6.6 Characters"
    (test "char?" #t (char? #\a))
    (test "char=?" #t (char=? #\A #\A))
    (test "char<?" #t (char<? #\a #\b))
    (test "digit-value" 3 (digit-value #\3))
    (test "char->integer" 65 (char->integer #\A))
    (test "integer->char" #\A (integer->char 65)))

  (test-group "6.7 Strings"
    (test "string?" #t (string? "abc"))
    (test "make-string" "AAA" (make-string 3 #\A))
    (test "string-length" 5 (string-length "abcde"))
    (test "string-ref" #\a (string-ref "abc" 0))
    (test "string=?" #t (string=? "abc" "abc"))
    (test "substring" "bc" (substring "abcde" 1 3))
    (test "string-append" "abc" (string-append "a" "b" "c"))
    (test "string->list" '(#\a #\b) (string->list "ab"))
    (test "list->string" "ab" (list->string '(#\a #\b)))
    (test "string-copy" "abc" (string-copy "abc")))

  (test-group "6.8 Vectors"
    (test "vector?" #t (vector? #(0 1 2)))
    (test "make-vector" #(0 0) (make-vector 2 0))
    (test "vector" #(a b c) (vector 'a 'b 'c))
    (test "vector-length" 3 (vector-length #(1 2 3)))
    (test "vector-ref" 8 (vector-ref #(1 1 2 3 5 8) 5))
    (test "vector-set!" #(0 1 2) (let ((v (vector 0 "Sue" 2))) (vector-set! v 1 1) v))
    (test "vector->list" '(dah didah) (vector->list #(dah didah)))
    (test "list->vector" #(dididit dah) (list->vector '(dididit dah))))

  (test-group "6.9 Bytevectors"
    (test "bytevector?" #t (bytevector? #u8(0 1 2)))
    (test "make-bytevector" #u8(12 12) (make-bytevector 2 12))
    (test "bytevector-length" 3 (bytevector-length #u8(0 10 5)))
    (test "bytevector-u8-ref" 8 (bytevector-u8-ref #u8(1 1 2 3 5 8) 5))
    (test "bytevector-u8-set!" #u8(1 3 3) (let ((b (bytevector 1 2 3))) (bytevector-u8-set! b 1 3) b)))

  (test-group "6.10 Control features"
    (test "procedure?" #t (procedure? car))
    (test "apply" 7 (apply + '(3 4)))
    (test "map" '(b e h) (map cadr '((a b) (d e) (g h))))
    (test "string-map" "StUdLy" (string-map (lambda (c) (if (char-lower-case? c) (char-upcase c) (char-downcase c))) "sTuDlY"))
    (test "vector-map" #(b e h) (vector-map cadr '#((a b) (d e) (g h))))
    (test "for-each" #(0 1 4 9 16) 
          (let ((v (make-vector 5))) 
            (for-each (lambda (i) (vector-set! v i (* i i))) '(0 1 2 3 4)) 
            v))
    (test "call/cc" -3 
          (call-with-current-continuation
           (lambda (exit)
             (for-each (lambda (x) (if (negative? x) (exit x))) 
                       '(54 0 37 -3 245 19)))))
    (test "values" '(4 5) (call-with-values (lambda () (values 4 5)) list))
  )

  (test-group "6.11 Exceptions"
    ;; Basic error signaling test
    (test "with-exception-handler" 'caught
          (call/cc (lambda (k)
                     (with-exception-handler 
                      (lambda (x) (k 'caught))
                      (lambda () (raise 'an-error))))))
    (test "error object" #t 
          (guard (condition (else (error-object? condition)))
            (error "Message" 'irritant)))
  )

  (test-group "6.12 Environments and evaluation"
    ;; Eval is powerful and depends on environment availability. 
    ;; Testing (scheme base) availability.
    (test "eval" 21 (eval '(* 7 3) (environment '(scheme base)))))

  (test-group "6.13 Input and output"
    (test "input-port?" #t (input-port? (current-input-port)))
    (test "output-port?" #t (output-port? (current-output-port)))
    
    (test-group "String ports"
      (let ((p (open-output-string)))
        (display "piece" p)
        (display " by piece" p)
        (test "get-output-string" "piece by piece" (get-output-string p))))

    (test-group "Input reading"
      (let ((p (open-input-string "(a b c)")))
        (test "read" '(a b c) (read p))))
  )

  (test-group "6.14 System interface"
    ;; These are highly system dependent, checking for existence/return type
    (test "current-second number?" #t (number? (current-second)))
    (test "current-jiffy integer?" #t (integer? (current-jiffy)))
    (test "jiffies-per-second integer?" #t (integer? (jiffies-per-second)))
    (test "features list?" #t (list? (features)))
  )
)