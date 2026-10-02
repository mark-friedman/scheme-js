(test-group "Primitives"
  (test-group "Equality"
    (test "eq? numbers" #t (eq? 1 1))
    (test "eq? symbols" #t (eq? 'a 'a))
    (test "eq? different symbols" #f (eq? 'a 'b))
    
    (test "equal? numbers" #t (equal? 1 1))
    (test "equal? strings" #t (equal? "abc" "abc"))
    (test "equal? lists" #t (equal? '(1 2 3) '(1 2 3)))
    (test "equal? nested lists" #t (equal? '(1 (2)) '(1 (2))))
    (test "equal? vectors" #t (equal? '#(1 2) '#(1 2)))
    (test "equal? different lists" #f (equal? '(1 2) '(1 3)))
  )

  (test-group "Equivalence"
    (test "eqv? numbers" #t (eqv? 1 1))
    (test "eqv? symbols" #t (eqv? 'a 'a))
    (test "eqv? NaN" #t (eqv? +nan.0 +nan.0))
    (test "eqv? 0.0 -0.0" #f (eqv? 0.0 -0.0))
    (test "equal? NaN" #t (equal? +nan.0 +nan.0))
    (test "equal? 0.0 -0.0" #f (equal? 0.0 -0.0))
  )

  ;; Lists made from strings and vectors of any length: a list was once made
  ;; by passing every element as an argument, which JavaScript cannot do
  ;; past some hundred thousand.
  (test-group "long strings and vectors to lists"
    (test "string->list" 200000 (length (string->list (make-string 200000 #\a))))
    (test "vector->list" 200000 (length (vector->list (make-vector 200000 0))))
    (test "vector->list keeps order" '(1 2 3) (vector->list #(0 1 2 3 4) 1 4))
  )

  ;; R7RS leaves eq? on characters unspecified, and this implementation
  ;; makes it eqv?, as most do: code that finds characters with memq or
  ;; assq, or compares them with eq?, as chibi's string library does, is
  ;; common, and a character is one object per code point.
  (test-group "eq? on characters"
    (test "two literals" #t (eq? #\a #\a))
    (test "a literal and string-ref" #t (eq? #\a (string-ref "abc" 0)))
    (test "two string-refs" #t (eq? (string-ref "xyz" 1) (string-ref "y" 0)))
    (test "integer->char" #t (eq? (integer->char 955) (string-ref "\x3bb;" 0)))
    (test "char-upcase" #t (eq? (char-upcase #\a) #\A))
    (test "read-char" #t (eq? (read-char (open-input-string "q")) #\q))
    (test "string->list" #t (eq? (car (string->list "z")) #\z))
    (test "memq finds one" '(#\b #\c) (memq #\b (string->list "abc")))
    (test "assq finds one" '(#\b . 2) (assq #\b '((#\a . 1) (#\b . 2))))
    (test "different characters" #f (eq? #\a #\b))
  )

  (test-group "Strings"
    (test "string?" #t (string? "abc"))
    (test "string? number" #f (string? 123))
    (test "string-append" "abcdef" (string-append "abc" "def"))
    (test "number->string" "123" (number->string 123))
  )

  (test-group "Division Primitives"
    ;; exact-integer-sqrt
    (test "exact-integer-sqrt 0" '(0 0)
          (let-values (((s r) (exact-integer-sqrt 0)))
            (list s r)))
    (test "exact-integer-sqrt 16" '(4 0)
          (let-values (((s r) (exact-integer-sqrt 16)))
            (list s r)))
    (test "exact-integer-sqrt 17" '(4 1)
          (let-values (((s r) (exact-integer-sqrt 17)))
            (list s r)))
    
    ;; floor/
    (test "floor/ positive" '(2 1)
          (let-values (((q r) (floor/ 5 2)))
            (list q r)))
    (test "floor/ negative dividend" '(-3 1)
          (let-values (((q r) (floor/ -5 2)))
            (list q r)))
    
    ;; truncate/
    (test "truncate/ positive" '(2 1)
          (let-values (((q r) (truncate/ 5 2)))
            (list q r)))
    (test "truncate/ negative dividend" '(-2 -1)
          (let-values (((q r) (truncate/ -5 2)))
            (list q r)))
    
    ;; Single-value versions
    (test "floor-quotient" 2 (floor-quotient 5 2))
    (test "floor-remainder" 1 (floor-remainder 5 2))
    (test "truncate-quotient" 2 (truncate-quotient 5 2))
    (test "truncate-remainder" 1 (truncate-remainder 5 2))
  )

  (test-group "List Procedures"
    ;; make-list
    (test "make-list empty" '() (make-list 0))
    (test "make-list default fill" '(#f #f #f) (make-list 3))
    (test "make-list with fill" '(x x x x) (make-list 4 'x))
    (test "make-list with list fill" '((a b) (a b)) (make-list 2 '(a b)))
    
    ;; list-set!
    (test "list-set! first" '(99 2 3)
      (let ((lst (list 1 2 3)))
        (list-set! lst 0 99)
        lst))
    (test "list-set! middle" '(1 99 3)
      (let ((lst (list 1 2 3)))
        (list-set! lst 1 99)
        lst))
    (test "list-set! last" '(1 2 99)
      (let ((lst (list 1 2 3)))
        (list-set! lst 2 99)
        lst))
  )

  (test-group "Symbols"
    (test "symbol=? same" #t (symbol=? 'a 'a))
    (test "symbol=? different" #f (symbol=? 'a 'b))
    (test "symbol=? multiple same" #t (symbol=? 'foo 'foo 'foo))
    (test "symbol=? multiple different" #f (symbol=? 'foo 'foo 'bar))
  )
)
