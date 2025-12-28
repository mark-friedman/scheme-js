;; Number Tests
;;
;; Comprehensive tests for basic numeric operations (integers, floats)

(test-group "Number tests"
  
  ;; ===== Arithmetic Operations =====
  
  (test-group "addition"
    
    (test "add zero args"
      0
      (+))
    
    (test "add one arg"
      5
      (+ 5))
    
    (test "add two args"
      7
      (+ 3 4))
    
    (test "add multiple args"
      15
      (+ 1 2 3 4 5))
    
    (test "add negatives"
      -5
      (+ -2 -3))
    
    (test "add mixed signs"
      2
      (+ 5 -3))
    
    (test "add floats"
      5.5
      (+ 2.5 3.0))
  )
  
  (test-group "subtraction"
    
    (test "negate single arg"
      -5
      (- 5))
    
    (test "subtract two args"
      3
      (- 7 4))
    
    (test "subtract multiple"
      0
      (- 10 5 3 2))
    
    (test "subtract negative"
      8
      (- 5 -3))
    
    (test "subtract floats"
      1.5
      (- 4.0 2.5))
  )
  
  (test-group "multiplication"
    
    (test "multiply zero args"
      1
      (*))
    
    (test "multiply one arg"
      5
      (* 5))
    
    (test "multiply two args"
      12
      (* 3 4))
    
    (test "multiply multiple"
      120
      (* 1 2 3 4 5))
    
    (test "multiply by zero"
      0
      (* 5 0 3))
    
    (test "multiply negatives"
      6
      (* -2 -3))
    
    (test "multiply mixed signs"
      -15
      (* 5 -3))
  )
  
  (test-group "division"
    
    (test "reciprocal"
      0.5
      (/ 2))
    
    (test "divide two args"
      3
      (/ 12 4))
    
    (test "divide multiple"
      2
      (/ 24 3 4))
    
    (test "divide negative"
      -3
      (/ -12 4))
    
    (test "divide floats"
      2.5
      (/ 5.0 2))
  )
  
  ;; ===== Comparison =====
  
  (test-group "comparison"
    
    (test "= equal"
      #t
      (= 5 5))
    
    (test "= not equal"
      #f
      (= 5 6))
    
    (test "= multiple equal"
      #t
      (= 3 3 3 3))
    
    (test "= one different"
      #f
      (= 3 3 4 3))
    
    (test "< increasing"
      #t
      (< 1 2 3 4))
    
    (test "< not increasing"
      #f
      (< 1 3 2))
    
    (test "> decreasing"
      #t
      (> 4 3 2 1))
    
    (test "> not decreasing"
      #f
      (> 4 2 3))
    
    (test "<= with equal"
      #t
      (<= 1 2 2 3))
    
    (test ">= with equal"
      #t
      (>= 3 2 2 1))
  )
  
  ;; ===== Integer Division =====
  
  (test-group "integer division"
    
    (test "quotient positive"
      3
      (quotient 10 3))
    
    (test "quotient negative dividend"
      -3
      (quotient -10 3))
    
    (test "quotient negative divisor"
      -3
      (quotient 10 -3))
    
    (test "quotient both negative"
      3
      (quotient -10 -3))
    
    (test "remainder positive"
      1
      (remainder 10 3))
    
    (test "remainder negative dividend"
      -1
      (remainder -10 3))
    
    (test "modulo positive"
      1
      (modulo 10 3))
    
    (test "modulo different from remainder"
      2
      (modulo -10 3))
  )
  
  ;; ===== Type Predicates =====
  
  (test-group "type predicates"
    
    (test "number? on integer"
      #t
      (number? 5))
    
    (test "number? on float"
      #t
      (number? 3.14))
    
    (test "number? on string"
      #f
      (number? "5"))
    
    (test "integer? on integer"
      #t
      (integer? 42))
    
    (test "integer? on float"
      #f
      (integer? 3.14))
    
    (test "integer? on whole float"
      #t
      (integer? 5.0))
    
    (test "real? on float"
      #t
      (real? 3.14))
    
    (test "rational? on finite"
      #t
      (rational? 3.14))
    
    (test "exact-integer? on integer"
      #t
      (exact-integer? 42))
    
    (test "exact-integer? on float"
      #f
      (exact-integer? 42.5))
  )
  
  ;; ===== Math Functions =====
  
  (test-group "math functions"
    
    (test "abs positive"
      5
      (abs 5))
    
    (test "abs negative"
      5
      (abs -5))
    
    (test "abs zero"
      0
      (abs 0))
    
    (test "floor"
      3
      (floor 3.7))
    
    (test "floor negative"
      -4
      (floor -3.2))
    
    (test "ceiling"
      4
      (ceiling 3.2))
    
    (test "ceiling negative"
      -3
      (ceiling -3.7))
    
    (test "truncate positive"
      3
      (truncate 3.9))
    
    (test "truncate negative"
      -3
      (truncate -3.9))
    
    (test "expt square"
      25
      (expt 5 2))
    
    (test "expt cube"
      8
      (expt 2 3))
    
    (test "sqrt"
      5
      (sqrt 25))
    
    ;; square
    (test "square zero" 0 (square 0))
    (test "square positive" 4 (square 2))
    (test "square negative" 9 (square -3))
    (test "square large" 1764 (square 42))
    (test "square float" 6.25 (square 2.5))
    
    ;; exact (converts to exact, rounds floats)
    (test "exact integer" 5 (exact 5))
    (test "exact float" 5 (exact 5.0))
    
    ;; inexact tests - SKIPPED due to JavaScript numeric limitation
    ;; JavaScript has a single numeric type (IEEE 754 double).
    ;; There is no native way to distinguish exact/inexact integers.
    ;; See README.md "Exact/Inexact Numbers" for details.
    (test-skip "(inexact? (inexact 5))"
      "JavaScript cannot distinguish exact/inexact integers - see README.md")
  )
  
  ;; ===== Special Values =====
  
  (test-group "special values"
    
    (test "positive infinity"
      #t
      (infinite? +inf.0))
    
    (test "negative infinity"
      #t
      (infinite? -inf.0))
    
    (test "nan"
      #t
      (nan? +nan.0))
    
    (test "finite number"
      #t
      (finite? 5))
    
    (test "infinity not finite"
      #f
      (finite? +inf.0))
  )
  
  ;; ===== Predicates =====
  
  (test-group "numeric predicates"
    
    (test "zero? on zero"
      #t
      (zero? 0))
    
    (test "zero? on nonzero"
      #f
      (zero? 5))
    
    (test "positive? on positive"
      #t
      (positive? 5))
    
    (test "positive? on negative"
      #f
      (positive? -5))
    
    (test "negative? on negative"
      #t
      (negative? -5))
    
    (test "negative? on positive"
      #f
      (negative? 5))
    
    (test "odd? on odd"
      #t
      (odd? 7))
    
    (test "odd? on even"
      #f
      (odd? 8))
    
    (test "even? on even"
      #t
      (even? 8))
    
    (test "even? on odd"
      #f
      (even? 7))
  )
  
  ;; ===== Error Cases =====
  
  (test-group "error handling"
    
    (test "+ with non-number"
      'error
      (guard (e (#t 'error))
        (+ 1 "a")))
    
    (test "quotient with non-integer"
      'error
      (guard (e (#t 'error))
        (quotient 10.5 3)))
    
    (test "sqrt of negative"
      #t
      (nan? (sqrt -1)))
  )

  ;; ===== Rational Support =====
  
  (test-group "rational arithmetic"
    
    (test "round rational 7/2"
      4
      (round 7/2))
    
    (test "round rational 5/2"
      2
      (round 5/2))  ;; round to even
    
    (test "floor rational"
      3
      (floor 7/2))
    
    (test "ceiling rational"
      4
      (ceiling 7/2))
    
    (test "inexact rational"
      3.5
      (inexact 7/2))
  )

  ;; ===== String->Number =====
  
  (test-group "string->number"
    
    (test "scientific notation"
      100.0
      (string->number "1e2"))
    
    (test "scientific notation with decimal"
      1.5e10
      (string->number "1.5e10"))
    
    (test "basic integer"
      100
      (string->number "100"))
    
    (test "hex radix"
      256
      (string->number "100" 16))
    
    (test "invalid string returns false"
      #f
      (string->number "1 2"))
    
    (test "positive infinity"
      +inf.0
      (string->number "+inf.0"))
    
    (test "negative infinity"
      -inf.0
      (string->number "-inf.0"))
  )

) ;; end test-group
