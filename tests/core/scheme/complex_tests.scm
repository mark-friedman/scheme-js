;; Complex Number Tests
;;
;; Comprehensive tests for complex number support

(test-group "Complex Number tests"
  
  ;; ===== Parsing =====
  
  (test-group "parsing"
    
    (test "complex with + sign"
      #t
      (complex? 3+4i))
    
    (test "complex with - sign"
      #t
      (complex? 3-4i))
    
    (test "pure imaginary +i"
      #t
      (complex? +i))
    
    (test "pure imaginary -i"
      #t
      (complex? -i))
    
    (test "pure imaginary 5i"
      #t
      (complex? 5i))
    
    (test "pure imaginary -3i"
      #t
      (complex? -3i))
    
    (test "complex with decimal"
      #t
      (complex? 1.5+2.5i))
    
    (test "negative real part"
      #t
      (complex? -3+4i))
  )
  
  ;; ===== make-rectangular =====
  
  (test-group "make-rectangular"
    
    (test "creates complex"
      #t
      (complex? (make-rectangular 3 4)))
    
    (test "real part correct"
      3
      (real-part (make-rectangular 3 4)))
    
    (test "imag part correct"
      4
      (imag-part (make-rectangular 3 4)))
    
    (test "negative parts"
      -3
      (real-part (make-rectangular -3 -4)))
    
    (test "zero imaginary part"
      0
      (imag-part (make-rectangular 5 0)))
  )
  
  ;; ===== make-polar =====
  
  (test-group "make-polar"
    
    (test "creates complex"
      #t
      (complex? (make-polar 5 0)))
    
    (test "angle 0 gives real"
      5.0
      (real-part (make-polar 5 0)))
    
    (test "angle 0 gives zero imag"
      0.0
      (imag-part (make-polar 5 0)))
  )
  
  ;; ===== real-part and imag-part =====
  
  (test-group "real-part/imag-part"
    
    (test "real-part of 3+4i"
      3
      (real-part 3+4i))
    
    (test "imag-part of 3+4i"
      4
      (imag-part 3+4i))
    
    (test "real-part of 3-4i"
      3
      (real-part 3-4i))
    
    (test "imag-part of 3-4i"
      -4
      (imag-part 3-4i))
    
    (test "real-part of +i"
      0
      (real-part +i))
    
    (test "imag-part of +i"
      1
      (imag-part +i))
    
    (test "real-part of -i"
      0
      (real-part -i))
    
    (test "imag-part of -i"
      -1
      (imag-part -i))
    
    (test "real-part of 5i"
      0
      (real-part 5i))
    
    (test "imag-part of 5i"
      5
      (imag-part 5i))
    
    (test "real-part of real number"
      7
      (real-part 7))
    
    (test "imag-part of real number"
      0
      (imag-part 7))
  )
  
  ;; ===== magnitude =====
  
  (test-group "magnitude"
    
    (test "magnitude of 3+4i"
      5.0
      (magnitude 3+4i))
    
    (test "magnitude of 4+3i"
      5.0
      (magnitude 4+3i))
    
    (test "magnitude of -3+4i"
      5.0
      (magnitude -3+4i))
    
    (test "magnitude of +i"
      1.0
      (magnitude +i))
    
    (test "magnitude of -i"
      1.0
      (magnitude -i))
    
    (test "magnitude of 5i"
      5.0
      (magnitude 5i))
    
    (test "magnitude of positive real"
      5
      (magnitude 5))
    
    (test "magnitude of negative real"
      5
      (magnitude -5))
    
    (test "magnitude of 0"
      0
      (magnitude 0))
  )
  
  ;; ===== angle =====
  
  (test-group "angle"
    
    (test "angle of positive real"
      0
      (angle 5))
    
    (test "angle of +i is pi/2"
      #t
      (< (abs (- (angle +i) 1.5707963)) 0.0001))
    
    (test "angle of -i is -pi/2"
      #t
      (< (abs (- (angle -i) -1.5707963)) 0.0001))
  )
  
  ;; ===== Type Predicates =====
  
  (test-group "type predicates"
    
    (test "complex? on complex"
      #t
      (complex? 3+4i))
    
    (test "number? on complex"
      #t
      (number? 3+4i))
    
    (test "complex? on real"
      #t
      (complex? 5))
    
    (test "complex? on rational"
      #t
      (complex? 1/2))
    
    (test "real? on complex with nonzero imag"
      #f
      (real? 3+4i))
    
    (test "real? on complex with zero imag"
      #t
      (real? 3+0i))
    
    (test "rational? on complex with nonzero imag"
      #f
      (rational? 3+4i))
    
    (test "integer? on complex with nonzero imag"
      #f
      (integer? 3+4i))
    
    (test "integer? on complex with zero imag and int real"
      #t
      (integer? 3+0i))
  )

  ;; ===== An exact zero imaginary part, made or read =====
  ;; A complex number whose imaginary part is an exact zero is the real
  ;; number its real part is (R7RS 6.2.6: `(real? -2.5+0i)` is true), however
  ;; it is made. An inexact zero part keeps it complex: `(real? -2.5+0.0i)` is
  ;; false.

  (test-group "make-rectangular with an exact zero imaginary part"

    (test "gives the integer"
      5
      (make-rectangular 5 0))

    (test "an exact integer"
      #t
      (exact-integer? (make-rectangular 5 0)))

    (test "eqv? to the integer"
      #t
      (eqv? 5 (make-rectangular 5 0)))

    (test "gives the fraction"
      1/2
      (make-rectangular 1/2 0))

    (test "gives an inexact real part as it is"
      -2.5
      (make-rectangular -2.5 0))

    (test "an inexact zero part stays complex"
      "5.0+0.0i"
      (number->string (make-rectangular 5 0.0))))

  (test-group "reading an exact zero imaginary part"

    (test "5+0i is the integer"
      #t
      (eqv? 5 5+0i))

    (test "5-0i is the integer"
      #t
      (eqv? 5 5-0i))

    (test "+0i is zero"
      #t
      (eqv? 0 +0i))

    (test "a fraction plus 0i"
      #t
      (eqv? 1/2 1/2+0i))

    (test "-2.5+0i is real"
      #t
      (eqv? -2.5 -2.5+0i))

    (test "an inexact zero part stays complex"
      "5.0+0.0i"
      (number->string 5+0.0i))

    (test "with #e"
      #t
      (eqv? 5 #e5+0i))

    (test "with #x"
      #t
      (eqv? 5 #x5+0i))

    (test "#e makes inexact parts exact"
      #t
      (eqv? 5 #e5.0+0.0i))

    (test "#e makes a decimal part a fraction"
      (make-rectangular 3/2 2)
      #e1.5+2i)

    (test "#e1.5+2i is exact"
      #t
      (exact? #e1.5+2i))

    (test "#i keeps a zero part, inexact"
      "5.0+0.0i"
      (number->string #i5+0i))

    (test "#i makes both parts inexact"
      (make-rectangular 1.0 2.0)
      #i1+2i))

  ;; ===== Arithmetic and exactness =====
  ;; An operation on exact operands is exact (R7RS 6.2.2), so a complex
  ;; number with exact parts keeps them exact through + - * /. `test`
  ;; compares with `equal?`, which tells 0+2i from 0.0+2.0i.

  (test-group "exact operands give an exact result"

    (test "an exact complex times an exact integer"
      0+2i
      (* (make-rectangular 0 1) 2))

    (test "doubled three times"
      0+8i
      (let loop ((z (make-rectangular 0 1)) (n 3))
        (if (= n 0) z (loop (* z 2) (- n 1)))))

    (test "the product is exact"
      #t
      (exact? (* (make-rectangular 0 1) 2)))

    (test "an integer times a complex"
      0+6i
      (* 3 +2i))

    (test "plus an integer"
      1+i
      (+ +i 1))

    (test "an integer plus"
      1+i
      (+ 1 +i))

    (test "minus an integer"
      0+4i
      (- 3+4i 3))

    (test "an integer minus"
      3-4i
      (- 3 +4i))

    (test "divided by an integer"
      +i
      (/ +2i 2))

    (test "divided by an integer, giving fractions"
      (make-rectangular 1/2 3/2)
      (/ 1+3i 2))

    (test "two complex numbers multiplied"
      11+2i
      (* 1+2i 3-4i))

    (test "two complex numbers divided"
      +i
      (/ 1+i 1-i))

    (test "an integer divided by a complex"
      (make-rectangular 0 -1/2)
      (/ 1 +2i))

    (test "the square of a complex"
      -3+4i
      (square 1+2i))

    (test "negation"
      -3-4i
      (- 3+4i))

    (test "the reciprocal"
      -i
      (/ +i))

    (test "fractional parts plus a fraction"
      1+i
      (+ 1/2+i 1/2))

    (test "fractional parts times an integer"
      3+2i
      (* (make-rectangular 1/2 1/3) 6)))

  ;; An exact complex number whose imaginary part is exact zero is a real
  ;; number (R7RS 6.2.6: `(real? -2.5+0i)` is true), and arithmetic returns it
  ;; as one, so `integer?`, `exact-integer?` and `eqv?` see the integer it is.
  (test-group "an exact zero imaginary part leaves a real"

    (test "a complex minus itself"
      0
      (- +i +i))

    (test "the square of i"
      -1
      (square +i))

    (test "i times i"
      -1
      (* +i +i))

    (test "conjugates added"
      2
      (+ 1+i 1-i))

    (test "is an exact integer"
      #t
      (exact-integer? (* +i +i)))

    (test "is eqv? to the integer"
      #t
      (eqv? -1 (square +i))))

  (test-group "an inexact operand gives an inexact result"

    (test "an exact complex times a flonum"
      (make-rectangular 0.0 2.0)
      (* +i 2.0))

    (test "the product is inexact"
      #t
      (inexact? (* +i 2.0)))

    (test "a flonum plus an exact complex"
      (make-rectangular 1.5 1.0)
      (+ 1.5 +i))

    (test "fractional parts plus a flonum"
      (make-rectangular 1.0 1.0)
      (+ 1/2+i 0.5))

    (test "fractional parts times a flonum"
      (make-rectangular 1.25 2.5)
      (* 1/2+i 2.5))

    (test "an inexact complex times an exact integer"
      (make-rectangular 2.0 4.0)
      (* (make-rectangular 1.0 2.0) 2))

    (test "negation keeps the sign of a zero part"
      "-1.0-0.0i"
      (number->string (- (make-rectangular 1.0 0.0)))))

  (test-group "exact and inexact of a complex"

    (test "inexact"
      (make-rectangular 1.0 2.0)
      (inexact 1+2i))

    (test "inexact is inexact"
      #t
      (inexact? (inexact 1+2i)))

    (test "exact"
      1+2i
      (exact (make-rectangular 1.0 2.0)))

    (test "exact of fractional parts"
      (make-rectangular 1/2 1/4)
      (exact (make-rectangular 0.5 0.25))))

  (test-group "numeric equality"

    (test "an exact complex and the same parts made"
      #t
      (= 1/3+i (make-rectangular 1/3 1)))

    (test "a product equal to an integer"
      #t
      (= (* +i +i) -1))

    ;; The second is the double nearest 1/3, made exact.
    (test "a complex and a real are not rounded to compare"
      #f
      (= (make-rectangular 1/3 0) 6004799503160661/18014398509481984)))

) ;; end test-group
