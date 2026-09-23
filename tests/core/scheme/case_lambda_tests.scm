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

  ;; Clauses with more fixed parameters than the dispatcher spells out one by
  ;; one. A five-parameter clause used to be taken for `(a b c . rest)`.
  (test "case-lambda five and six fixed parameters"
    '(15 21 3)
    (let ((f (case-lambda
               ((a b c d e) (+ a b c d e))
               ((a b c d e g) (+ a b c d e g))
               ((a b) (+ a b)))))
      (list (f 1 2 3 4 5) (f 1 2 3 4 5 6) (f 1 2))))

  (test "case-lambda four fixed parameters and a rest"
    '(10 (5 6))
    (let ((f (case-lambda
               ((a b c d . more) (list (+ a b c d) more)))))
      (f 1 2 3 4 5 6)))

  (test "case-lambda rest clause after a five-parameter clause"
    '(five (1 2))
    (let ((f (case-lambda
               ((a b c d e) 'five)
               (args args))))
      (list (f 1 2 3 4 5) (f 1 2))))

  (test "case-lambda with no matching clause is an error"
    'error
    (guard (e (#t 'error))
      ((case-lambda ((a) a) ((a b c d e) e)) 1 2)))
  
  ) ;; end test-group
