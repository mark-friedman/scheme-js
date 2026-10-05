;; The expander's Scheme (src/core/scheme/expander.scm and syntax_rules.scm):
;; forms into the core forms the evaluator runs -- special forms, variables
;; and applications, bodies and their definitions, quasiquote, and macros of
;; every kind the expander defines.

(import (scheme-js expander)
        (scheme-js reader))

;; /**
;;  * A core form with each name the expander made by renaming, `x_$N`,
;;  * written `x_1`, `x_2` ... in the order the names first appear, so that an
;;  * expansion can be compared with one written out.
;;  */
(define (normalized core)
  (let ((renamings '()))
    (define (renamed? s)
      (let loop ((i (- (string-length s) 1)))
        (cond ((< i 1) #f)
              ((char-numeric? (string-ref s i)) (loop (- i 1)))
              (else (and (< i (- (string-length s) 1))
                         (char=? (string-ref s i) #\$)
                         (char=? (string-ref s (- i 1)) #\_)
                         (- i 1))))))
    (define (rename symbol)
      (let* ((s (symbol->string symbol))
             (end (renamed? s)))
        (cond ((not end) symbol)
              ((assq symbol renamings) => cdr)
              (else
               (let ((new (string->symbol
                           (string-append (substring s 0 end) "_"
                                          (number->string (+ 1 (length renamings)))))))
                 (set! renamings (cons (cons symbol new) renamings))
                 new)))))
    (let walk ((x core))
      (cond ((symbol? x) (rename x))
            ((pair? x) (let ((head (walk (car x)))) (cons head (walk (cdr x)))))
            (else x)))))

;; /**
;;  * A form's expansion, normalized.
;;  */
(define (expanded form)
  (normalized (expand form)))

;; /**
;;  * The message of the error expanding a form raises, or #f.
;;  */
(define (expand-failure form)
  (guard (e ((error-object? e) (error-object-message e)))
    (expand form)
    #f))

(test-group "expander - atoms"
  (test "a number" '(lit 1) (expand 1))
  (test "a string" '(lit "s") (expand "s"))
  (test "a boolean" '(lit #t) (expand #t))
  (test "a character" '(lit #\a) (expand #\a))
  (test "a vector" '(lit #(1 2)) (expand '#(1 2)))
  (test "a variable" '(var x) (expand 'x))
  (test "a quoted list" '(lit (a b)) (expand ''(a b)))
  (test "the empty list is not an expression" "analyze: cannot analyze null (empty list)"
        (expand-failure '())))

(test-group "expander - if, set!, define and begin"
  (test "if" '(if (lit 1) (lit 2) (lit 3)) (expand '(if 1 2 3)))
  (test "if without an alternative" `(if (lit 1) (lit 2) (lit ,js-undefined)) (expand '(if 1 2)))
  (test "set!" '(set x (lit 1)) (expand '(set! x 1)))
  (test "set! of a property" '(app (var js-set!) ((var o) (lit "p") (lit 1))) (expand '(set! o.p 1)))
  (test "define" '(define x (lit 1)) (expand '(define x 1)))
  (test "define of a procedure, named for it"
        '(define f (lambda (a_1) #f "f" (var a_1) (a) #f))
        (expanded '(define (f a) a)))
  (test "define of a lambda, named for it"
        '(define g (lambda () #f "g" (lit 1) () #f))
        (expanded '(define g (lambda () 1))))
  (test "begin of nothing" '(seq ()) (expand '(begin)))
  (test "begin of one form is the form" '(lit 1) (expand '(begin 1)))
  (test "begin of two" '(seq ((lit 1) (lit 2))) (expand '(begin 1 2))))

(test-group "expander - lambda and its parameters"
  (test "fixed parameters, renamed"
        '(lambda (a_1 b_2) #f "anonymous" (app (var a_1) ((var b_2))) (a b) #f)
        (expanded '(lambda (a b) (a b))))
  (test "a rest parameter"
        '(lambda (a_1) r_2 "anonymous" (var r_2) (a) r)
        (expanded '(lambda (a . r) r)))
  (test "only a rest parameter"
        '(lambda () args_1 "anonymous" (var args_1) () args)
        (expanded '(lambda args args)))
  (test "a parameter shadows a keyword"
        '(lambda (if_1) #f "anonymous" (app (var if_1) ((lit 1) (lit 2))) (if) #f)
        (expanded '(lambda (if) (if 1 2))))
  (test "a body's definitions are made where it runs"
        '(lambda () #f "anonymous" (seq ((define x (lit 1)) (var x))) () #f)
        (expanded '(lambda () (define x 1) x)))
  (test "a parameter that is not an identifier" "lambda: parameter must be a symbol"
        (expand-failure '(lambda (a 1) a)))
  (test "no body" "lambda: expected at least 2 operands, got 1" (expand-failure '(lambda (a)))))

(test-group "expander - let and letrec"
  (test "let, as a lambda applied"
        '(app (lambda (x_1 y_2) #f "let" (app (var +) ((var x_1) (var y_2))) (x y) #f)
              ((lit 1) (lit 2)))
        (expanded '(let ((x 1) (y 2)) (+ x y))))
  (test "named let, its initializers outside the loop"
        '(app (letrec (loop_1) ((lambda (i_2) #f "anonymous" (app (var loop_1) ((var i_2))) (i) #f))
                      (var loop_1) (loop))
              ((lit 0)))
        (expanded '(let loop ((i 0)) (loop i))))
  (test "letrec of lambdas"
        '(letrec (f_1) ((lambda () #f "anonymous" (app (var f_1) ()) () #f)) (var f_1) (f))
        (expanded '(letrec ((f (lambda () (f)))) f)))
  (test "letrec of anything else, every initializer run before any is assigned"
        `(app (lambda (a_1) #f "letrec"
                (app (lambda (a-init_2) #f "letrec-init"
                       (seq ((set a_1 (var a-init_2)) (var a_1))) (a-init_2) #f)
                     ((lit 1)))
                (a) #f)
              ((lit ,js-undefined)))
        (expanded '(letrec ((a 1)) a)))
  (test "a binding not (variable expression)" "let: a binding is (variable expression)"
        (expand-failure '(let ((x)) x))))

(test-group "expander - quasiquote"
  (test "unquote and unquote-splicing"
        '(app (var cons) ((lit a) (app (var cons) ((var b) (app (var append) ((var c) (lit ())))))))
        (expand '`(a ,b ,@c)))
  (test "a vector" '(app (var vector) ((lit 1) (var x))) (expand '`#(1 ,x)))
  (test "nested, unquoted at the inner level"
        '(app (var cons)
              ((lit a)
               (app (var cons)
                    ((app (var list)
                          ((lit quasiquote)
                           (app (var cons)
                                ((lit b)
                                 (app (var cons)
                                      ((app (var list) ((lit unquote) (var c)))
                                       (lit ())))))))
                     (lit ())))))
        (expand '`(a `(b ,,c)))))

(test-group "expander - applications and dot notation"
  (test "an application" '(app (var f) ((lit 1) (var x))) (expand '(f 1 x)))
  (test "a method call" '(app (var js-invoke) ((var o) (lit "m") (lit 1))) (expand '(o.m 1)))
  (test "a call of the superclass's method"
        '(app (var class-super-call) ((var this) (lit m) (lit 1))) (expand '(super.m 1))))

(test-group "expander - libraries and features"
  (test "import" '(import ((scheme base))) (expand '(import (scheme base))))
  (let ((form '(define-library (expander test) (export x))))
    (test "define-library" #t (let ((core (expand form))) (and (eq? (car core) 'define-library) (eq? (cadr core) form)))))
  (test "cond-expand, a feature met" '(lit 1) (expand '(cond-expand (r7rs 1) (else 2))))
  (test "cond-expand, else" '(lit 2) (expand '(cond-expand ((not r7rs) 1) (else 2)))))

(test-group "expander - a core form's operands"
  (test "too few" "quote: expected 1 operand, got 0" (expand-failure '(quote)))
  (test "a range" "if: expected 2 to 3 operands, got 1" (expand-failure '(if 1)))
  (test "not a proper list" "set!: its operands are not a proper list" (expand-failure '(set! x . 1)))
  (test "an application's operands not a proper list" "analyze: forms are not a proper list"
        (expand-failure '(f 1 . 2))))

(test-group "expander - syntax-rules"
  (test "defining a macro is no expression" '(lit ())
        (expand '(define-syntax expander-test-swap!
                   (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))))
  (test "a use, its introduced binding renamed"
        '(app (lambda (tmp_1) #f "let" (seq ((set p (var q)) (set q (var tmp_1)))) (tmp) #f) ((var p)))
        (expanded '(expander-test-swap! p q)))
  (test "kept apart from the user's binding of the same name"
        '(app (lambda (tmp_1) #f "let"
                (app (lambda (tmp_2) #f "let" (seq ((set tmp_1 (var q)) (set q (var tmp_2)))) (tmp) #f)
                     ((var tmp_1)))
                (tmp) #f)
              ((lit 5)))
        (expanded '(let ((tmp 5)) (expander-test-swap! tmp q))))
  (test "no clause matching" "expander-test-swap!: No matching clause for macro 'expander-test-swap!'"
        (expand-failure '(expander-test-swap! 1)))
  (expand '(define-syntax expander-test-ellipsis (syntax-rules ::: () ((_ x :::) (list x :::)))))
  (test "an ellipsis of its own" '(app (var list) ((lit 1) (lit 2))) (expand '(expander-test-ellipsis 1 2)))
  (expand '(define-syntax expander-test-vector (syntax-rules () ((_ #(a ...)) (list a ...)))))
  (test "a vector pattern" '(app (var list) ((lit 1) (lit 2))) (expand '(expander-test-vector #(1 2))))
  (expand '(define-syntax expander-test-nested (syntax-rules () ((_ (a b ...) ...) '((b ... a) ...)))))
  (test "nested ellipses" '(lit ((2 3 1) (4))) (expand '(expander-test-nested (1 2 3) (4))))
  (expand '(define-syntax expander-test-escape (syntax-rules () ((_ x) '(x (... ...))))))
  (test "an escaped ellipsis" '(lit (1 ...)) (expand '(expander-test-escape 1)))
  (test "a literal, matched where it is not bound"
        '(app (lambda (temp_1) #f "let" (if (var temp_1) (app (var f) ((var temp_1))) (lit 1)) (temp) #f)
              ((var x)))
        (expanded '(cond (x => f) (else 1))))
  (test "a definition with something else than syntax-rules defines nothing" '(lit ())
        (expand '(define-syntax expander-test-other (er-macro-transformer 1))))
  (expand '(define-syntax expander-test-shadowed (syntax-rules () ((_) 'macro))))
  (expand '(define expander-test-shadowed 1))
  (test "a top-level definition over a macro's name makes the name a variable's"
        '(app (var expander-test-shadowed) ()) (expand '(expander-test-shadowed))))

(test-group "expander - let-syntax and letrec-syntax"
  (test "let-syntax, its body a let's"
        '(app (lambda () #f "let" (app (var list) ((lit 1) (lit 1))) () #f) ())
        (expanded '(let-syntax ((m (syntax-rules () ((_ x) (list x x))))) (m 1))))
  (test "letrec-syntax, its macros seeing each other"
        '(app (var +) ((lit 1) (app (var +) ((lit 2) (lit 1)))))
        (expanded '(letrec-syntax ((m (syntax-rules () ((_) 1) ((_ x . r) (+ x (m . r))))))
                     (m 1 2)))))

(test-group "expander - define-macro"
  (test "defining one is no expression" '(lit ())
        (expand '(define-macro (expander-test-twice x) (list 'begin x x))))
  (test "a use, its operands as written" '(seq ((app (var f) ()) (app (var f) ())))
        (expand '(expander-test-twice (f))))
  (test "a transformer failing"
        "expander-test-broken: Error expanding macro 'expander-test-broken': car: expected pair at argument 1, got null"
        (begin (expand '(define-macro (expander-test-broken) (car '())))
               (expand-failure '(expander-test-broken))))
  (test "a transformer that cannot be made"
        "define-macro: Error evaluating macro transformer for 'expander-test-unmade': car: expected pair at argument 1, got null"
        (expand-failure '(define-macro expander-test-unmade (car '())))))

;; /**
;;  * A text's first datum, read with spans.
;;  */
(define (read-first text)
  (car (read-source text "expander.scm" #f #t)))

(test-group "expander - spans"
  (let ((form (read-first "(f x)")))
    (test "an application's, its form's" #t (eq? (js-ref (expand form) "source") (js-ref form "source"))))
  (let ((form (read-first "(if a b)")))
    (test "a special form's, its form's" #t (eq? (js-ref (expand form) "source") (js-ref form "source"))))
  (let* ((form (read-first "(define (f x)\n  x)"))
         (core (expand form)))
    (test "a procedure's definition gives its lambda its span" #t
          (eq? (js-ref (caddr core) "source") (js-ref form "source"))))
  (let* ((form (read-first "(when x\n  (f y))"))
         (core (expand form)))
    (test "what a macro's use was given keeps its span through the expansion" #t
          (eq? (js-ref (caddr core) "source") (js-ref (caddr form) "source")))))
