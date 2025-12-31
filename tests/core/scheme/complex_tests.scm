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
  
) ;; end test-group
