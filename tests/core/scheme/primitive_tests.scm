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

  (test-group "Strings"
    (test "string?" #t (string? "abc"))
    (test "string? number" #f (string? 123))
    (test "string-append" "abcdef" (string-append "abc" "def"))
    (test "number->string" "123" (number->string 123))
  )
)
