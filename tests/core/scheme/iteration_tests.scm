;; Iteration Procedure Tests
;;
;; Tests for string-map, string-for-each, vector-map, vector-for-each

(test-group "Iteration Procedures tests"
  
  ;; ===== string-map tests =====
  
  (test-group "string-map"
    
    (test "string-map basic"
      "ABC"
      (string-map char-upcase "abc"))
    
    (test "string-map with case toggle"
      "aBcD"
      (string-map (lambda (c)
                    (if (char-lower-case? c)
                        (char-upcase c)
                        (char-downcase c)))
                  "AbCd"))
    
    (test "string-map empty string"
      ""
      (string-map char-upcase ""))
    
    (test "string-map returns string"
      #t
      (string? (string-map char-upcase "abc")))
    
    (test "string-map type error"
      'error
      (guard (e (#t 'error))
        (string-map 'not-a-proc "abc")))
    
    (test "string-map string type error"
      'error
      (guard (e (#t 'error))
        (string-map char-upcase 123)))
  )
  
  ;; ===== string-for-each tests =====
  
  (test-group "string-for-each"
    
    (test "string-for-each collects"
      '(#\c #\b #\a)
      (let ((result '()))
        (string-for-each 
         (lambda (c) (set! result (cons c result)))
         "abc")
        result))
    
    (test "string-for-each empty string"
      '()
      (let ((result '()))
        (string-for-each 
         (lambda (c) (set! result (cons c result)))
         "")
        result))
    
    (test "string-for-each type error"
      'error
      (guard (e (#t 'error))
        (string-for-each 'not-a-proc "abc")))
  )
  
  ;; ===== vector-map tests =====
  
  (test-group "vector-map"
    
    (test "vector-map basic"
      #(2 4 6)
      (vector-map (lambda (x) (* x 2)) #(1 2 3)))
    
    (test "vector-map cadr"
      #(b e h)
      (vector-map cadr '#((a b) (d e) (g h))))
    
    (test "vector-map empty vector"
      #()
      (vector-map (lambda (x) x) #()))
    
    (test "vector-map returns vector"
      #t
      (vector? (vector-map (lambda (x) x) #(1 2 3))))
    
    (test "vector-map type error"
      'error
      (guard (e (#t 'error))
        (vector-map 'not-a-proc #(1 2 3))))
    
    (test "vector-map vector type error"
      'error
      (guard (e (#t 'error))
        (vector-map (lambda (x) x) "not-a-vector")))
  )
  
  ;; ===== vector-for-each tests =====
  
  (test-group "vector-for-each"
    
    (test "vector-for-each collects"
      #(0 1 4 9 16)
      (let ((v (make-vector 5)))
        (vector-for-each 
         (lambda (i) (vector-set! v i (* i i)))
         #(0 1 2 3 4))
        v))
    
    (test "vector-for-each empty vector"
      '()
      (let ((result '()))
        (vector-for-each 
         (lambda (x) (set! result (cons x result)))
         #())
        result))
    
    (test "vector-for-each type error"
      'error
      (guard (e (#t 'error))
        (vector-for-each 'not-a-proc #(1 2 3))))
  )
  
) ;; end test-group
