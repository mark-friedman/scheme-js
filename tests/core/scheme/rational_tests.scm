;; Rational Number Tests
;;
;; Comprehensive tests for exact rational number support

(test-group "Rational Number tests"
  
  ;; ===== Parsing and Basic Construction =====
  
  (test-group "parsing"
    
    (test "parse simple fraction"
      #t
      (rational? 1/2))
    
    (test "parse larger fraction"
      #t
      (rational? 123/456))
    
    (test "parse negative numerator"
      #t
      (rational? -3/4))
  )
  
  ;; ===== Reduction =====
  
  (test-group "reduction"
    
    (test "reduces 2/4 to 1/2"
      1
      (numerator 2/4))
    
    (test "denominator of reduced 2/4"
      2
      (denominator 2/4))
    
    (test "reduces 10/5 to 2"
      2
      (numerator 10/5))
    
    (test "denominator of integer-like rational"
      1
      (denominator 10/5))
    
    (test "reduces large fraction"
      1
      (numerator 100/200))
  )
  
  ;; ===== numerator and denominator =====
  
  (test-group "numerator/denominator"
    
    (test "numerator of 3/7"
      3
      (numerator 3/7))
    
    (test "denominator of 3/7"
      7
      (denominator 3/7))
    
    (test "numerator of negative"
      -3
      (numerator -3/7))
    
    (test "denominator of negative (stays positive)"
      7
      (denominator -3/7))
    
    (test "numerator of integer"
      5
      (numerator 5))
    
    (test "denominator of integer"
      1
      (denominator 5))
    
    (test "numerator of zero"
      0
      (numerator 0))
    
    (test "denominator of zero"
      1
      (denominator 0))
  )
  
  ;; ===== Type Predicates =====
  
  (test-group "type predicates"
    
    (test "rational? on fraction"
      #t
      (rational? 1/2))
    
    (test "number? on rational"
      #t
      (number? 1/2))
    
    (test "complex? on rational"
      #t
      (complex? 1/2))
    
    (test "real? on rational"
      #t
      (real? 1/2))
    
    (test "integer? on non-integer rational"
      #f
      (integer? 1/2))
    
    (test "integer? on integer-valued rational"
      #t
      (integer? 4/2))
    
    (test "exact? on rational"
      #t
      (exact? 1/2))
    
    (test "inexact? on rational"
      #f
      (inexact? 1/2))
  )
  
  ;; ===== Integer as Rational =====
  
  (test-group "integers as rationals"
    
    (test "rational? on integer"
      #t
      (rational? 5))
    
    (test "numerator on integer"
      42
      (numerator 42))
    
    (test "denominator on integer"
      1
      (denominator 42))
    
    (test "negative integer numerator"
      -7
      (numerator -7))
    
    (test "negative integer denominator"
      1
      (denominator -7))
  )
  
  ;; ===== Error Cases =====
  
  (test-group "error handling"
    
    (test "numerator on non-rational"
      'error
      (guard (e (#t 'error))
        (numerator 3.14)))
    
    (test "denominator on non-rational"
      'error
      (guard (e (#t 'error))
        (denominator 3.14)))
  )
  
  ;; ===== Mixed Exactness =====

  ;; R7RS 6.2.2: an operation with an inexact argument returns an inexact
  ;; result, here a flonum, whose value an integral one like 1000.0 does not
  ;; make exact.
  (test-group "an inexact argument makes the result inexact"

    (test "* of an integral flonum and a fraction is inexact"
      #f
      (exact? (* 1000.0 1/3)))

    (test "* of an integral flonum and a fraction is a flonum"
      "333.3333333333333"
      (number->string (* 1000.0 1/3)))

    (test "* of a fraction and an integral flonum"
      "0.3333333333333333"
      (number->string (* 1/3 1.0)))

    (test "+ of a fraction and an inexact zero"
      "0.5"
      (number->string (+ 1/2 0.0)))

    (test "- of an integral flonum and a fraction"
      0.5
      (- 1.0 1/2))

    (test "- of a fraction and an integral flonum"
      -0.5
      (- 1/2 1.0))

    (test "rounding the product leaves it inexact"
      #t
      (inexact? (round (* 1000.0 71/75))))

    (test "a percentage computed as a test framework computes one"
      "94.7"
      (number->string (/ (round (* 1000.0 (/ 71 75))) 10))))

) ;; end test-group
