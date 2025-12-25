;; case-lambda tests
;;
;; Tests for the (scheme case-lambda) library

(test-group "case-lambda tests"
  
  ;; Basic single clause (should work like regular lambda)
  (test "case-lambda single clause"
    (let ((f (case-lambda
               ((x) (* x 2)))))
      (f 5))
    10)
  
  ;; Two clauses with different arities (avoiding zero-arity for now)
  (test "case-lambda two clauses"
    (let ((f (case-lambda
               ((x) x)
               ((x y) (+ x y)))))
      (list (f 42) (f 3 4)))
    '(42 7))
  
  ;; Three clauses
  (test "case-lambda three clauses"
    (let ((add (case-lambda
                 ((x) x)
                 ((x y) (+ x y))
                 ((x y z) (+ x y z)))))
      (list (add 5) (add 3 4) (add 1 2 3)))
    '(5 7 6))
  
  ) ;; end test-group
