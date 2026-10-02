;; syntax-rules vector tests
;;
;; R7RS 4.3.2: a pattern may be a vector, #(P ...), matching a vector whose
;; elements match its elements, an ellipsis among them matching any number;
;; and a template may be a vector, built element by element as a list
;; template is. Pattern-matching macros written in syntax-rules, such as
;; chibi's (chibi match), match vectors this way. The macros' names are
;; unusual, since test files share an environment.

(define-syntax srv-swap
  (syntax-rules ()
    ((_ #(a b)) '#(b a))))

(define-syntax srv-elements
  (syntax-rules ()
    ((_ #(x ...)) '(x ...))))

(define-syntax srv-ends
  (syntax-rules ()
    ((_ #(first middle ... last)) '(first last (middle ...)))))

(define-syntax srv-kind
  (syntax-rules ()
    ((_ #(a)) 'one-element-vector)
    ((_ #(a b)) 'two-element-vector)
    ((_ (a)) 'list)
    ((_ anything-else) 'other)))

(define-syntax srv-unzip
  (syntax-rules ()
    ((_ #(#(a b) ...)) '((a ...) (b ...)))))

(define-syntax srv-vector-of
  (syntax-rules ()
    ((_ x ...) #(x ...))))

(define-syntax srv-escaped
  (syntax-rules ()
    ((_ x) '#(x (... ...)))))

(define-syntax srv-literal
  (syntax-rules (=>)
    ((_ #(a => b)) '(b a))
    ((_ anything-else) 'no-arrow)))

(test-group "syntax-rules vector patterns"

  (test "binds each element"
    #(2 1)
    (srv-swap #(1 2)))

  (test "an ellipsis matches every element"
    '(1 2 3)
    (srv-elements #(1 2 3)))

  (test "an ellipsis matches no elements"
    '()
    (srv-elements #()))

  (test "elements before and after an ellipsis"
    '(1 4 (2 3))
    (srv-ends #(1 2 3 4)))

  (test "a vector pattern does not match a list"
    'list
    (srv-kind (1)))

  (test "a vector pattern does not match a vector of another length"
    'two-element-vector
    (srv-kind #(1 2)))

  (test "a vector pattern does not match a symbol"
    'other
    (srv-kind x))

  (test "vector patterns nest, under an ellipsis"
    '((1 3) (2 4))
    (srv-unzip #(#(1 2) #(3 4))))

  (test "a literal inside a vector pattern"
    '(2 1)
    (srv-literal #(1 => 2)))

  (test "a literal inside a vector pattern must match"
    'no-arrow
    (srv-literal #(1 2 3))))

(test-group "syntax-rules vector templates"

  (test "an ellipsis in a vector template"
    #(1 2 3)
    (srv-vector-of 1 2 3))

  (test "a vector template's symbols are symbols"
    '(#t #t)
    (let ((v (srv-vector-of a b)))
      (list (eq? (vector-ref v 0) 'a) (symbol? (vector-ref v 1)))))

  (test "an escaped ellipsis in a vector template"
    '(1 ...)
    (vector->list (srv-escaped 1))))
