;; Define-Values Tests
;;
;; Tests for define-values macro functionality

(test-group "Define-values tests"
  
  ;; ===== Internal define-values (inside let) =====
  
  (test-group "internal define-values"
    
    (test "define-values single in let"
      1
      (let ()
        (define-values (x) (values 1))
        x))
    
    (test "define-values two vars in let"
      3
      (let ()
        (define-values (a b) (values 1 2))
        (+ a b)))
    
    (test "define-values three vars in let"
      6
      (let ()
        (define-values (a b c) (values 1 2 3))
        (+ a b c)))
    
    (test "define-values rest in let"
      '(3 4)
      (let ()
        (define-values (a b . rest) (values 1 2 3 4))
        rest))
    
    (test "define-values all as list"
      '(1 2 3)
      (let ()
        (define-values x (values 1 2 3))
        x))
  )
  
  ;; ===== Pattern variable hygiene =====
  ;; These tests check if pattern variables are consistently renamed
  
  (test-group "pattern variable hygiene"
    
    ;; Test that pattern variable appears consistently in macro output
    (test "macro with multiple occurrences of pattern var"
      1
      (let ()
        ;; Simple macro where pattern var appears twice
        (define-syntax dup-define
          (syntax-rules ()
            ((dup-define var val)
             (begin
               (define var val)
               var))))
        (dup-define x 1)))
    
    ;; Test begin-define-define pattern like define-values uses
    (test "begin with two defines using same var"
      3
      (let ()
        (define-syntax two-defs
          (syntax-rules ()
            ((two-defs var1 var2 val1 val2)
             (begin
               (define var1 val1)
               (define var2 val2)))))
        (two-defs a b 1 2)
        (+ a b)))
    
    ;; Test that introduced variable can be shared across defines
    (test "define with set! in body"
      2
      (let ()
        (define-syntax def-then-set
          (syntax-rules ()
            ((def-then-set var)
             (begin
               (define var 1)
               (set! var 2)))))
        (def-then-set x)
        x))
    
    ;; Test variable reference across defines in same begin
    (test "second define references first"
      3
      (let ()
        (define-syntax def-pair
          (syntax-rules ()
            ((def-pair a b)
             (begin
               (define temp-list '(1 2 3))
               (define a (car temp-list))
               (define b (cadr temp-list))))))
        (def-pair x y)
        (+ x y)))
  )

) ;; end test-group
