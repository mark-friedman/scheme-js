;; (scheme eval) library tests
;;
;; Tests for eval, environment, interaction-environment

(test-group "(scheme eval) tests"
  
  ;; ===== eval basic tests =====
  
  (test "eval simple arithmetic"
    (eval '(+ 1 2) (interaction-environment))
    3)
  
  (test "eval quote"
    (eval '(quote hello) (interaction-environment))
    'hello)
  
  (test "eval list construction"
    (eval '(list 1 2 3) (interaction-environment))
    '(1 2 3))
  
  (test "eval lambda application"
    (eval '((lambda (x) (* x x)) 5) (interaction-environment))
    25)
  
  (test "eval if expression true"
    (eval '(if #t 'yes 'no) (interaction-environment))
    'yes)
  
  (test "eval if expression false"
    (eval '(if #f 'yes 'no) (interaction-environment))
    'no)
  
  ;; ===== eval with definitions =====
  
  (test "eval define then use"
    (begin
      (eval '(define eval-test-var 42) (interaction-environment))
      (eval 'eval-test-var (interaction-environment)))
    42)
  
  (test "eval define procedure"
    (begin
      (eval '(define (eval-test-add x y) (+ x y)) (interaction-environment))
      (eval '(eval-test-add 3 4) (interaction-environment)))
    7)
  
  ;; ===== eval with let =====
  
  (test "eval let expression"
    (eval '(let ((x 10) (y 20)) (+ x y)) (interaction-environment))
    30)
  
  (test "eval nested let"
    (eval '(let ((x 5))
             (let ((y 10))
               (* x y)))
          (interaction-environment))
    50)
  
  ;; ===== environment tests =====
  
  (test "interaction-environment returns env"
    (not (eq? (interaction-environment) #f))
    #t)
  
  (test "environment returns interaction-environment"
    ;; Our implementation returns interaction-environment
    (eq? (environment) (interaction-environment))
    #t)
  
  ;; ===== eval with quasiquote =====
  
  (test "eval quasiquote"
    (eval '`(a b c) (interaction-environment))
    '(a b c))
  
  (test "eval quasiquote with unquote"
    (eval (list 'quasiquote (list 'a (list 'unquote '(+ 1 2)) 'c))
          (interaction-environment))
    '(a 3 c))
  
  ;; ===== eval error handling =====
  
  (test "eval with malformed expression"
    (guard (e (#t 'error-caught))
      (eval '(1 2 3) (interaction-environment)))  ; 1 is not a procedure
    'error-caught)
  
  ) ;; end test-group
