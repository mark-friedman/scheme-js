;;; ----------------------------------------------------------------------------
;;; R7RS-small Compliance Test Suite
;;; Chapter 4: Expressions
;;; ----------------------------------------------------------------------------

(test-group "4 Expressions"

  ;; --------------------------------------------------------------------------
  ;; 4.1 Primitive expression types
  ;; --------------------------------------------------------------------------
  (test-group "4.1 Primitive expression types"

    (test-group "4.1.1 Variable references"
      (define x 28)
      (test "Variable reference resolves to bound value" 28 x))

    (test-group "4.1.2 Literal expressions"
      (test "quote symbol" 'a (quote a))
      (test "quote vector" '#(a b c) (quote #(a b c)))
      (test "quote list" '(+ 1 2) (quote (+ 1 2)))
      (test "Abbrev quote symbol" 'a 'a)
      (test "Abbrev quote vector" '#(a b c) '#(a b c))
      (test "Self-evaluating integer" 145932 145932)
      (test "Self-evaluating string" "abc" "abc")
      (test "Self-evaluating boolean" #t #t)
      (test "Self-evaluating vector" '#(a 10) #(a 10))
    )

    (test-group "4.1.3 Procedure calls"
      (test "Simple call" 7 (+ 3 4))
      (test "Computed operator" 12 ((if #f + *) 3 4)))

    (test-group "4.1.4 Procedures"
      (test "Lambda application" 8 ((lambda (x) (+ x x)) 4))
      
      (define reverse-subtract (lambda (x y) (- y x)))
      (test "Named lambda" 3 (reverse-subtract 7 10))
      
      (define add4 (let ((x 4)) (lambda (y) (+ x y))))
      (test "Closure" 10 (add4 6))
      
      (test "Rest args" '(3 4 5 6) ((lambda x x) 3 4 5 6))
      (test "Fixed + rest args" '(5 6) ((lambda (x y . z) z) 3 4 5 6))
    )

    (test-group "4.1.5 Conditionals"
      (test "if true" 'yes (if (> 3 2) 'yes 'no))
      (test "if false" 'no (if (> 2 3) 'yes 'no))
      (test "if arithmetic" 1 (if (> 3 2) (- 3 2) (+ 3 2)))
      ;; One-armed if side-effect test
      (let ((result #f))
        (if #t (set! result 'visited))
        (test "One-armed if" 'visited result))
    )

    (test-group "4.1.6 Assignments"
      (let ((x 2))
        (test "Before set!" 3 (+ x 1))
        (set! x 4)
        (test "After set!" 5 (+ x 1))))

    (test-group "4.1.7 Inclusion"
      ;; Cannot meaningfully test file inclusion in this environment
      (test "Include tests require external files" #t #f))
  )

  ;; --------------------------------------------------------------------------
  ;; 4.2 Derived expression types
  ;; --------------------------------------------------------------------------
  (test-group "4.2 Derived expression types"

    (test-group "4.2.1 Conditionals"
      (test-group "cond"
        (test "cond first clause" 'greater (cond ((> 3 2) 'greater) ((< 3 2) 'less)))
        (test "cond second clause" 'equal (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)))
        (test "cond => syntax" 2 (cond ((assv 'b '((a 1) (b 2))) => cadr) (else #f))))

      (test-group "case"
        (test "case match composite" 'composite
              (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite)))
        (test "case match symbol" 'c
              (case (car '(c d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else => (lambda (x) x)))))

      (test-group "and"
        (test "and true" #t (and (= 2 2) (> 2 1)))
        (test "and false" #f (and (= 2 2) (< 2 1)))
        (test "and value" '(f g) (and 1 2 'c '(f g)))
        (test "and empty" #t (and)))

      (test-group "or"
        (test "or true" #t (or (= 2 2) (> 2 1)))
        (test "or false" #f (or #f #f #f))
        (test "or value" '(b c) (or (memq 'b '(a b c)) (/ 3 0))))

      (test-group "when/unless"
        (let ((x 0))
          (when (= 1 1) (set! x 1) (set! x (+ x 1)))
          (test "when side effects" 2 x))
        (let ((y 0))
          (unless (= 1 0) (set! y 1) (set! y (+ y 1)))
          (test "unless side effects" 2 y)))

      (test-group "cond-expand"
        ;; Basic check that else branch works if feature not present, 
        ;; or r7rs is present.
        (test "cond-expand r7rs" #t (cond-expand (r7rs #t) (else #f))))
    )

    (test-group "4.2.2 Binding constructs"
      (test-group "let"
        (test "let basic" 6 (let ((x 2) (y 3)) (* x y)))
        (test "let shadowing" 35 (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))))

      (test-group "let*"
        (test "let* sequencing" 70 (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))))

      (test-group "letrec"
        (test "letrec mutual recursion" #t
              (letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
                       (odd? (lambda (n) (if (zero? n) #f (even? (- n 1))))))
                (even? 88))))

      (test-group "letrec*"
        ;; R7RS-small example: letrec* enforces left-to-right evaluation order
        (test "letrec* sequencing" 5
              (letrec* ((p (lambda (x)
                             (+ 1 (q (- x 1)))))
                        (q (lambda (y)
                             (if (zero? y)
                                 0
                                 (+ 1 (p (- y 1))))))
                        (x (p 5))
                        (y x))
                y)))

      (test-group "let-values"
        (test "let-values exact-integer-sqrt" 35
              (let-values (((root rem) (exact-integer-sqrt 32))) (* root rem))))

      (test-group "let*-values"
        ;; let*-values binds sequentially, so second binding sees new values of a, b
        (test "let*-values sequencing" '((x y) (x y))
              (let ((a 'a) (b 'b) (x 'x) (y 'y))
                (let*-values (((a b) (values x y))    ; a='x, b='y
                              ((x y) (values a b)))   ; x='x, y='y (uses new a, b)
                  (list (list a b) (list x y)))))))
    )

    (test-group "4.2.3 Sequencing"
      (test "begin sequencing" 6
            (begin (define x 0) (set! x 5) (+ x 1))))

    (test-group "4.2.4 Iteration"
      (test "do" #(0 1 2 3 4)
            (do ((vec (make-vector 5))
                 (i 0 (+ i 1)))
                ((= i 5) vec)
              (vector-set! vec i i)))
      
      (test "named let" '((6 1 3) (-5 -2))
            (let loop ((numbers '(3 -2 1 6 -5))
                       (nonneg '())
                       (neg '()))
              (cond ((null? numbers) (list nonneg neg))
                    ((>= (car numbers) 0)
                     (loop (cdr numbers) (cons (car numbers) nonneg) neg))
                    ((< (car numbers) 0)
                     (loop (cdr numbers) nonneg (cons (car numbers) neg)))))))

    (test-group "4.2.5 Delayed evaluation"
      (test "lazy evaluation" 3
            (force (delay (+ 1 2))))
      (test "memoization" '(3 3)
            (let ((p (delay (+ 1 2))))
              (list (force p) (force p))))
      (test "promise?" #t
            (let ((p (delay 1))) (promise? p))))

    (test-group "4.2.6 Dynamic bindings"
      (define radix (make-parameter 10))
      (test "default parameter" 10 (radix))
      (test "parameterize" 2
            (parameterize ((radix 2)) (radix)))
      (test "parameterize restore" 10
            (begin (parameterize ((radix 2)) (radix)) (radix))))

    (test-group "4.2.7 Exception handling"
      (test "guard catches raise" 42
            (guard (condition
                     ((assq 'a condition) => cdr)
                     ((assq 'b condition)))
              (raise (list (cons 'a 42))))))

    (test-group "4.2.8 Quasiquotation"
      (test "quasiquote basic" '(list 3 4) `(list ,(+ 1 2) 4))
      (test "quasiquote splicing multiple" '(a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs `(4 -5 6)) b))
      (test "quasiquote splicing cons" '((foo 7) . cons) `(( foo ,(- 10 3)) ,@(cdr `(c)) . ,(car `(cons))))
      (test "quasiquote vector" #(10 5 2 4 3 8) `#(10 5 ,(sqrt 4) ,@(map sqrt `(16 9)) 8))
      (test "quasiquote in let" '(list foo bar baz) 
            (let ((foo `(foo bar)) (@baz `baz))
              `(list ,@foo , @baz)))
      ;; Nested quasiquote - inner unquotes stay unevaluated, outer unquote of inner ,(foo...) evaluates the ,(+ 1 3)
      (test "quasiquote nested" '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
            `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
      ;; ,,name1 at level 1: outer unquote evaluates name1 -> 'x, leaves ,x in template
      ;; ,`,name2 at level 1: inner ` increases nesting, , decreases it, evaluates name2 -> 'y 
      (test "quasiquote nested in let" '(a `(b ,x ,'y d) e)
            (let ((name1 'x)
                  (name2 'y))
              `(a `(b ,,name1 ,',name2 d) e)))
      (test "explicit quasiquote unquote" '(list 3 4)
            (quasiquote (list (unquote (+ 1 2)) 4)))
      (test "quasiquote unquote-splicing" '(quasiquote (list (unquote (+ 1 2)) 4))
            `(quasiquote (list (unquote (+ 1 2)) 4))))

    (test-group "4.2.9 Case-lambda"
      (define range
        (case-lambda
         ((e) (range 0 e))
         ((b e) (do ((r '() (cons e r))
                     (e (- e 1) (- e 1)))
                    ((< e b) r)))))
      (test "case-lambda 1 arg" '(0 1 2) (range 3))
      (test "case-lambda 2 args" '(3 4) (range 3 5)))
  )

  ;; --------------------------------------------------------------------------
  ;; 4.3 Macros
  ;; --------------------------------------------------------------------------
  (test-group "4.3 Macros"
    
    (test-group "4.3.1 Binding constructs for syntactic keywords"
      (test "let-syntax" 'now
            (let-syntax ((given-that (syntax-rules ()
                                       ((given-that test stmt1 stmt2 ...)
                                        (if test (begin stmt1 stmt2 ...))))))
              (let ((if #t))
                (given-that if (set! if 'now))
                if)))
      
      (test "letrec-syntax" 7
            (letrec-syntax
                ((my-or (syntax-rules ()
                          ((my-or) #f)
                          ((my-or e) e)
                          ((my-or e1 e2 ...)
                           (let ((temp e1))
                             (if temp temp (my-or e2 ...)))))))
              (let ((x #f) (y 7) (temp 8))
                (my-or x (let ((temp 9)) (if y y)) y)))))
  )