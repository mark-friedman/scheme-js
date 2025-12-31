;; BigInt Exactness Tests
;;
;; Tests for R7RS-compliant exactness using BigInt for exact integers
;; and Number for inexact reals.

(test-group "BigInt Exactness Tests"
  
  ;; ===== Exactness Predicates =====
  
  (test-group "exact? predicate"
    
    (test "exact? on integer literal"
      #t
      (exact? 5))
    
    (test "exact? on large integer"
      #t
      (exact? 9007199254740993))  ;; Beyond JS safe integer range
    
    (test "exact? on negative integer"
      #t
      (exact? -42))
    
    (test "exact? on zero"
      #t
      (exact? 0))
    
    (test "exact? on rational"
      #t
      (exact? 1/3))
    
    (test "exact? on float"
      #f
      (exact? 5.0))
    
    (test "exact? on decimal"
      #f
      (exact? 3.14))
  )
  
  (test-group "inexact? predicate"
    
    (test "inexact? on integer literal"
      #f
      (inexact? 5))
    
    (test "inexact? on float"
      #t
      (inexact? 5.0))
    
    (test "inexact? on decimal"
      #t
      (inexact? 3.14))
    
    (test "inexact? on rational"
      #f
      (inexact? 1/3))
  )
  
  ;; ===== Conversion Procedures =====
  
  (test-group "inexact procedure"
    
    (test "inexact converts exact integer to inexact"
      #t
      (inexact? (inexact 5)))
    
    (test "inexact preserves value"
      5.0
      (inexact 5))
    
    (test "inexact on float is no-op"
      3.14
      (inexact 3.14))
    
    (test "inexact on rational"
      0.5
      (inexact 1/2))
  )
  
  (test-group "exact procedure"
    
    (test "exact converts inexact integer to exact"
      #t
      (exact? (exact 5.0)))
    
    (test "exact preserves integer value"
      5
      (exact 5.0))
    
    (test "exact on exact integer is no-op"
      5
      (exact 5))
  )
  
  ;; ===== Mixed Arithmetic =====
  
  (test-group "exactness propagation"
    
    (test "exact + exact = exact"
      #t
      (exact? (+ 2 3)))
    
    (test "exact + inexact = inexact"
      #t
      (inexact? (+ 2 3.0)))
    
    (test "inexact + inexact = inexact"
      #t
      (inexact? (+ 2.0 3.0)))
    
    (test "exact * exact = exact"
      #t
      (exact? (* 2 3)))
    
    (test "exact * inexact = inexact"
      #t
      (inexact? (* 2 3.0)))
  )
  
  ;; ===== Integer? with Exactness =====
  
  (test-group "integer? with exactness"
    
    (test "integer? on exact integer"
      #t
      (integer? 5))
    
    (test "integer? on inexact integer (5.0)"
      #t
      (integer? 5.0))
    
    (test "integer? on non-integer float"
      #f
      (integer? 3.14))
    
    (test "exact-integer? on exact integer"
      #t
      (exact-integer? 5))
    
    (test "exact-integer? on inexact integer (5.0)"
      #f
      (exact-integer? 5.0))
  )
  
  ;; ===== Equality with Mixed Exactness =====
  
  (test-group "numeric equality"
    
    (test "= compares values regardless of exactness"
      #t
      (= 5 5.0))
    
    (test "eqv? distinguishes exact vs inexact"
      #f
      (eqv? 5 5.0))
  )
  
  ;; ===== Reader Syntax =====
  
  (test-group "reader exactness syntax"
    
    (test "#e forces exact"
      #t
      (exact? #e5.0))
    
    (test "#i forces inexact"
      #t
      (inexact? #i5))
    
    (test "#e5.0 value"
      5
      #e5.0)
    
    (test "#i5 value equals 5"
      #t
      (= #i5 5))
  )

) ;; end test-group
