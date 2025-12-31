(test-group "reader"
  (test-group "exponent-suffixes"
    (test 100.0 (read (open-input-string "1s2")))
    (test 100.0 (read (open-input-string "1S2")))
    (test 100.0 (read (open-input-string "1f2")))
    (test 100.0 (read (open-input-string "1F2")))
    (test 100.0 (read (open-input-string "1d2")))
    (test 100.0 (read (open-input-string "1D2")))
    (test 100.0 (read (open-input-string "1l2")))
    (test 100.0 (read (open-input-string "1L2")))
    
    (test 150.0 (read (open-input-string "1.5s2")))
    (test 150.0 (read (open-input-string "1.5S2")))
    
    (test 1e10 (read (open-input-string "1s10")))
    (test 1e-10 (read (open-input-string "1s-10")))
    (test 1.5e10 (read (open-input-string "1.5L10")))
    
    ;; Check with complex numbers
    (test #i1+2i (read (open-input-string "1s0+2i")))
    (test #i1+2i (read (open-input-string "1+2s0i")))
  )
  
  (test-group "datum-labels"
    ;; Basic circular list
    (let ((x (read (open-input-string "#0=(a . #0#)"))))
      (test #t (pair? x))
      (test 'a (car x))
      (test #t (eq? x (cdr x))))
      
    ;; Shared structure (not circular)
    (let ((x (read (open-input-string "#1=(a) #1#"))))
      (test #t (pair? x))
      (test 'a (car x))
      (test '() (cdr x)))
      
    ;; Circular vector
    (let ((v (read (open-input-string "#0=#(1 #0# 2)"))))
      (test #t (vector? v))
      (test 3 (vector-length v))
      (test 1 (vector-ref v 0))
      (test #t (eq? v (vector-ref v 1)))
      (test 2 (vector-ref v 2)))
      
    ;; Fused tokens
    (test 100 (read (open-input-string "#1=100")))
    (test 100 (read (open-input-string "#2= 100")))
    (let ((x (read (open-input-string "(#3=10 #3#)"))))
      (test 10 (car x))
      (test 10 (cadr x)))
      
    ;; Nested
    (let ((x (read (open-input-string "#0=(#1=(a) #0# #1#)"))))
      (test #t (eq? x (cadr x)))        ; second element is x itself
      (test #t (eq? (car x) (caddr x))) ; first element same as third
      (test 'a (caar x)))
  )
  
  (test-group "angle-bracket-identifiers"
     (test '<pare> (read (open-input-string "<pare>")))
     (test '< (read (open-input-string "<")))
     (test '> (read (open-input-string ">")))
     (test '<> (read (open-input-string "<>")))
     (test '<a-b-c> (read (open-input-string "<a-b-c>")))
  )
)
