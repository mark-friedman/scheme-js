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

  ;; Negation keeps exactness, and a flonum's sign: it was computed as zero
  ;; minus the number, with an inexact zero.
  (test-group "negation"
    (test "an exact rational stays exact" '(-3/10 #t) (let ((n (- 3/10))) (list n (exact? n))))
    (test "an exact integer stays exact" '(-3 #t) (let ((n (- 3))) (list n (exact? n))))
    (test "an inexact number stays inexact" '(-1.5 #t) (let ((n (- 1.5))) (list n (inexact? n))))
    (test "zero's sign flips" "-0.0" (number->string (- 0.0))))

  ;; `square` of a fraction was computed with JavaScript's `*` on the
  ;; Rational objects, which is NaN.
  (test-group "square"
    (test "of a fraction" 1/4 (square 1/2))
    (test "of a negative fraction" 9/4 (square -3/2))
    (test "is exact" #t (exact? (square 1/2))))

  ;; An exact fraction to an exact integer power is exact (R7RS 6.2.2); it was
  ;; computed with Math.pow, so `(expt 1/2 2)` was 0.25.
  (test-group "expt of a fraction"
    (test "squared" 1/4 (expt 1/2 2))
    (test "is exact" #t (exact? (expt 1/2 2)))
    (test "cubed" 8/27 (expt 2/3 3))
    (test "a negative fraction cubed" -1/8 (expt -1/2 3))
    (test "to the zeroth power" 1 (expt 1/2 0))
    (test "the zeroth power is an exact integer" #t (exact-integer? (expt 1/2 0)))
    (test "to a negative power" 4 (expt 1/2 -2))
    (test "a negative power is an exact integer" #t (exact-integer? (expt 1/2 -2)))
    (test "to a negative power, a fraction" 27/8 (expt 2/3 -3))
    (test "a negative fraction to a negative power" -27/8 (expt -2/3 -3))
    (test "to an inexact power" 0.25 (expt 1/2 2.0))
    (test "an inexact base" 0.25 (expt 0.5 2)))

  ;; `exact` of a flonum that is not an integer is the rational it is: a
  ;; flonum is a fraction over a power of two (R7RS 6.2.6).
  (test-group "exact of a flonum"
    (test "a half" 1/2 (exact 0.5))
    (test "below zero" -5/2 (exact -2.5))
    (test "a tenth, as the flonum holds it" 3602879701896397/36028797018963968 (exact 0.1))
    (test "exact, and back again the same" #t (= 0.1 (inexact (exact 0.1))))
    (test "an infinity is an error" 'raised (guard (e (#t 'raised)) (exact +inf.0))))

) ;; end test-group
