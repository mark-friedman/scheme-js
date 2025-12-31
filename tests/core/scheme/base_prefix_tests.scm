;; Base Prefix Tests
;;
;; Tests for R7RS numeric base prefixes (#x, #o, #b, #d, #e, #i)

(test-group "Base Prefix tests"
  
  ;; ===== Hexadecimal #x =====
  
  (test-group "hexadecimal"
    
    (test "#x10 = 16"
      16
      #x10)
    
    (test "#xff = 255"
      255
      #xff)
    
    (test "#xA = 10"
      10
      #xA)
    
    (test "#x-10 = -16"
      -16
      #x-10)
  )
  
  ;; ===== Octal #o =====
  
  (test-group "octal"
    
    (test "#o10 = 8"
      8
      #o10)
    
    (test "#o77 = 63"
      63
      #o77)
    
    (test "#o-10 = -8"
      -8
      #o-10)
  )
  
  ;; ===== Binary #b =====
  
  (test-group "binary"
    
    (test "#b101 = 5"
      5
      #b101)
    
    (test "#b11111111 = 255"
      255
      #b11111111)
    
    (test "#b-10 = -2"
      -2
      #b-10)
  )
  
  ;; ===== Decimal #d =====
  
  (test-group "decimal"
    
    (test "#d10 = 10"
      10
      #d10)
    
    (test "#d-5 = -5"
      -5
      #d-5)
  )
  
  ;; ===== Exactness prefixes =====
  
  (test-group "exactness"
    
    (test "#e10 = 10"
      10
      #e10)
    
    (test "#i10 = 10.0"
      10.0
      #i10)
  )
  
  ;; ===== Combined prefixes =====
  
  (test-group "combined prefixes"
    
    (test "#e#x10 = 16"
      16
      #e#x10)
    
    (test "#x#e10 = 16"
      16
      #x#e10)
    
    (test "#o#e10 = 8"
      8
      #o#e10)
  )
  
  ;; ===== Rationals with base =====
  
  (test-group "rationals with base"
    
    (test "#x10/2 numerator"
      8
      (numerator #x10/2))
    
    (test "#x10/2 denominator"
      1
      (denominator #x10/2))
  )
  
) ;; end test-group
