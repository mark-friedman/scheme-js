;; The emitter's text and the lifting plan (src/compiler/emit.scm, lift.scm)
;;
;; Runs in the compiler's own environment. What the emitter generates as a
;; whole is checked by running it: every compiled procedure in the test suite
;; and the benchmark programs. These check the pieces whose mistakes would not
;; always show up that way -- a string escaped wrongly, a literal repeated that
;; should not be -- and the lifting plan's decisions directly.

(test-group "emit - JavaScript text"
  (test "a plain name is prefixed" "s_x_$1" (js-name 'x_$1))
  (test "a character JavaScript does not allow becomes its code" "s_null_3f" (js-name 'null?))
  (test "each disallowed character is replaced" "s_a_2d_3eb" (js-name 'a->b))
  (test "a string is quoted" "\"abc\"" (js-string "abc"))
  (test "a quote and a backslash are escaped" "\"a\\\"b\\\\c\"" (js-string "a\"b\\c"))
  (test "a newline and a tab are escaped" "\"a\\nb\\tc\"" (js-string "a\nb\tc"))
  (test "another control character is escaped by code" "\"\\u0001\"" (js-string (string (integer->char 1))))
  (test "an integral flonum is written as JavaScript writes it" "1" (js-number 1.))
  (test "a fraction is written in full" "0.5" (js-number .5))
  (test "negative zero keeps its sign" "-0" (js-number -0.))
  (test "infinity has no literal" "Number(\"Infinity\")" (js-number (/ 1. 0)))
  (test "nor has NaN" "Number(\"NaN\")" (js-number (/ 0. 0))))

(test-group "emit - expressions"
  (test "an expression renders its parts" "f(s_a, 1)" (expr->string (js "f(" 's_a ", " "1" ")")))
  (test "a nested expression is spliced in" '("(" s_a ")") (js "(" (js 's_a) ")"))
  (test "an expression's locals are its symbols" '(s_a $t1) (expr-locals (js "f(" 's_a ", " '$t1 ")")))
  (test "a lone local can be written twice" #t (repeatable? (js 's_a)))
  (test "an exact integer can be written twice" #t (repeatable? (js "-12n")))
  (test "a pooled constant can be written twice" #t (repeatable? (js "K[3]")))
  (test "a boxed read cannot" #f (repeatable? (js 's_a "[0]")))
  (test "a flonum literal is evaluated once, into a temporary" #f (repeatable? (js "0.5")))
  (test "a string literal is evaluated once, into a temporary" #f (repeatable? (js "\"a\"")))
  (test "a temporary is settled" #t (settled? (js '$t4)))
  (test "undefined is settled" #t (settled? (js "undefined")))
  (test "a parameter is not settled" #f (settled? (js 's_a))))

(test-group "emit - statements"
  (test "a goto" "$pc = 3; continue;" (render-statement #f '(goto 3)))
  (test "a branch" "if (s_c !== false) { $pc = 1; continue; } $pc = 2; continue;"
        (render-statement #f (list 'branch (js 's_c) 1 2)))
  (test "an assignment" "$t1 = s_a[0];" (render-statement #f (list 'assign (js '$t1) (js 's_a "[0]"))))
  (test "a raw statement has no semicolon added" "while (x) { }"
        (render-statement #f (list 'raw (js "while (x) { }")))))

;; /**
;;  * Lowers a lambda written as analyzed-AST data, for the lifting tests.
;;  * @param {list} ast - An analyzed lambda node.
;;  * @returns {list} Its IR.
;;  */
(define (lowered ast) (cadr (lower-lambda ast)))

(test-group "lift - the plan"
  ;; (lambda (a) (lambda (x) (f a x)))
  (let* ((ir (lowered '(lambda (a) #f #f (lambda (x) #f #f (app (var f) ((var a) (var x)))))))
         (plan (plan-lifting ir))
         (inner (car (outermost-lambdas (lambda-body ir)))))
    (test "a nested lambda is passed its free variables" '(a) (plan-free-of plan inner))
    (test "nothing is boxed without an assignment" '() (plan-boxed plan)))
  ;; (lambda (a) (set! a 1) (lambda () a))
  (let ((plan (plan-lifting
                (lowered '(lambda (a) #f #f
                            (seq ((set a (lit 1)) (lambda () #f #f (var a)))))))))
    (test "an assigned local is boxed" '(a) (plan-boxed plan)))
  ;; (lambda () (letrec ((e (lambda () (o))) (o (lambda () (e)))) (e)))
  (let ((plan (plan-lifting
                (lowered '(lambda () #f #f
                            (letrec (e o)
                                    ((lambda () #f #f (app (var o) ()))
                                     (lambda () #f #f (app (var e) ())))
                                    (app (var e) ())))))))
    (test "letrec names a sibling refers to are boxed" #t
          (and (boxed? plan 'e) (boxed? plan 'o) #t)))
  ;; (lambda () (letrec ((loop (lambda (n) (loop n)))) (loop 1)))
  (let* ((ir (lowered '(lambda () #f #f
                         (letrec (loop)
                                 ((lambda (n) #f #f (app (var loop) ((var n)))))
                                 (app (var loop) ((lit 1)))))))
         (plan (plan-lifting ir)))
    (test "a name only its own lambda refers to is not boxed" #f (boxed? plan 'loop))
    (test "and that lambda binds it inside its own factory" '(loop)
          (plan-self-of plan (car (outermost-lambdas (lambda-body ir)))))))
