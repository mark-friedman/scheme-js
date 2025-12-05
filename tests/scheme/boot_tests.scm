;; Tests for lib/boot.scm
(test-group "Boot Library Tests"
  ;; Test 'and' macro
  (test-group "and"
    (assert-equal "and empty" (and) #t)
    (assert-equal "and single true" (and 1) 1)
    (assert-equal "and single false" (and #f) #f)
    (assert-equal "and multiple true" (and 1 2 3) 3)
    (assert-equal "and short-circuit" (and #f (undefined-var)) #f)
    (assert-equal "and mixed" (and 1 #f 3) #f))

  ;; Test 'let' macro
  (test-group "let"
    (assert-equal "let basic" (let ((x 1) (y 2)) (+ x y)) 3)
    (assert-equal "let shadowing" (let ((x 1)) (let ((x 2)) x)) 2)
    (assert-equal "named let" 
      (let loop ((n 5) (acc 0))
        (if (= n 0) acc (loop (- n 1) (+ acc n))))
      15))

  ;; Test 'letrec' macro
  (test-group "letrec"
    (assert-equal "letrec factorial"
      (letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))))
        (fact 5))
      120)
    (assert-equal "letrec mutual recursion"
      (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
               (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
        (even? 4))
      #t))

  ;; Test 'cond' macro
  (test-group "cond"
    (assert-equal "cond first true" (cond (#t 1) (else 2)) 1)
    (assert-equal "cond else" (cond (#f 1) (else 2)) 2)
    (assert-equal "cond no else" (cond (#f 1)) (if #f #t)) ;; Expect undefined/void
    (assert-equal "cond =>" (cond (5 => (lambda (x) (+ x 1)))) 6)
    (assert-equal "cond implicit begin" (cond (#t 1 2)) 2))

  ;; Test 'equal?' procedure
  (test-group "equal?"
    (assert-equal "equal? numbers" (equal? 1 1) #t)
    (assert-equal "equal? symbols" (equal? 'a 'a) #t)
    (assert-equal "equal? strings" (equal? "abc" "abc") #t)
    (assert-equal "equal? lists" (equal? '(1 2) '(1 2)) #t)
    (assert-equal "equal? nested lists" (equal? '(1 (2 3)) '(1 (2 3))) #t)
    (assert-equal "equal? vectors" (equal? '#(1 2) '#(1 2)) #t)
    (assert-equal "equal? mixed fail" (equal? '(1 2) '#(1 2)) #f))
)
