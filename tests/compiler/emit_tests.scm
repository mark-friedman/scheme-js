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

;; `eqv?` is `===` when either operand is a constant whose identity is its
;; value -- a symbol, a boolean, the empty list -- and needs the primitive
;; otherwise: numbers compare by value and exactness, characters by code point.
;; That is the shape `case` produces, one test per datum.
(test-group "inline - eqv? against a constant"
  (define x '(local x #f #f))
  (define y '(local y #f #f))
  (define (expands? name . args) (if (inline-expansion name args) #t #f))
  (test "against a symbol it expands" #t (expands? 'eqv? x '(const a #f)))
  (test "with the constant first as well" #t (expands? 'eqv? '(const a #f) x))
  (test "against a boolean" #t (expands? 'eqv? x '(const #f #f)))
  (test "against the empty list" #t (expands? 'eqv? x '(const () #f)))
  (test "not against an exact integer" #f (expands? 'eqv? x '(const 1 #f)))
  (test "not against an inexact number" #f (expands? 'eqv? x '(const 1.5 #f)))
  (test "not against a character" #f (expands? 'eqv? x (list 'const #\a #f)))
  (test "not between two variables" #f (expands? 'eqv? x y))
  (test "and to identity when it does" "s_x === K[0]"
        (let ((entry (inline-expansion 'eqv? (list x '(const a #f)))))
          (expr->string ((cadddr entry) (list (js 's_x) (js "K[0]"))))))
  (test "other expansions still apply by arity alone" #t (expands? 'car x))
  (test "and not at another arity" #f (expands? 'car x y)))

;; An arithmetic expansion's fast path is taken when both operands are exact
;; integers or both are inexact reals, which are JavaScript `bigint` and
;; `number`: for either pair the JavaScript operator computes what the numeric
;; tower would. Any other pair -- mixed exactness, a rational, a complex, a
;; wrong type -- takes the primitive.
(test-group "inline - arithmetic on two exact integers or two flonums"
  (define (test-of name)
    (let ((entry (inline-expansion name '((local a #f #f) (local b #f #f)))))
      (expr->string ((caddr entry) (list (js 's_a) (js 's_b))))))
  (define (value-of name)
    (let ((entry (inline-expansion name '((local a #f #f) (local b #f #f)))))
      (expr->string ((cadddr entry) (list (js 's_a) (js 's_b))))))
  (test "the test admits two bigints or two numbers"
        "(typeof s_a === 'bigint' && typeof s_b === 'bigint') || (typeof s_a === 'number' && typeof s_b === 'number')"
        (test-of '+))
  (test "every operator has the same test" #t
        (every (lambda (name) (string=? (test-of name) (test-of '+))) '(- * < > <= >= =)))
  (test "and the fast path is the operator, for both" "s_a - s_b" (value-of '-))
  (test "numeric equality is ===, which agrees on -0.0 and NaN" "s_a === s_b" (value-of '=)))

;; Vectors are JavaScript arrays. An access calls a runtime helper, which reads or
;; writes the array when the vector is an array and the index an exact integer in
;; range, and passes any other operand to the primitive -- so every error is still
;; the primitive's. The helper is the whole fast path, so there is no run-time
;; test beside the binding guard.
(test-group "inline - vector access"
  (define (parts name args)
    (let ((entry (inline-expansion name (map (lambda (a) (list 'local a #f #f)) args))))
      (let ((test ((caddr entry) (map js args))))
        (list (and test (expr->string test))
              (expr->string ((cadddr entry) (map js args)))))))
  (test "vector-ref is the helper" '(#f "$vectorRef(v, i)") (parts 'vector-ref '(v i)))
  (test "and so is vector-set!" '(#f "$vectorSet(v, i, x)") (parts 'vector-set! '(v i x)))
  (test "vector-length needs an array and is inline" '("Array.isArray(v)" "BigInt(v.length)")
        (parts 'vector-length '(v)))
  (test "a procedure using the helper declares it"
        #t
        (let ((source (car (generate-unit (cadr (lower-lambda '(lambda (v) #f #f (app (var vector-ref) ((var v) (lit 0))))))
                                          '(vector-ref) "f" '(vector-ref)))))
          (and (string-contains source "const $vectorRef = R.vectorRef") #t))))
