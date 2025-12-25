;; Features procedure tests
;;
;; Tests for (features) returning a proper list of R7RS feature symbols.

(test-group "(features) tests"
  
  ;; ===== Basic tests =====
  
  (test "features returns list"
    #t
    (list? (features)))
  
  (test "features returns non-empty list"
    #t
    (pair? (features)))
  
  (test "features contains r7rs"
    #t
    (if (memq 'r7rs (features)) #t #f))
  
  ;; ===== Type tests =====
  
  (test "all features are symbols"
    #t
    (let check-all ((lst (features)))
      (cond
        ((null? lst) #t)
        ((not (symbol? (car lst))) #f)
        (else (check-all (cdr lst))))))
  
  ;; ===== Standard features =====
  
  (test "features includes ieee-float"
    #t
    (if (memq 'ieee-float (features)) #t #f))
  
  (test "features includes full-unicode"
    #t
    (if (memq 'full-unicode (features)) #t #f))
  
  ;; ===== Implementation-specific =====
  
  (test "features includes scheme-js"
    #t
    (if (memq 'scheme-js (features)) #t #f))
  
  ) ;; end test-group
