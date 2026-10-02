;; Datum label literal tests
;;
;; R7RS 2.4: datum labels write shared and circular structure, and a program
;; may contain circular references in literals. Every test here passes its
;; expression through the `test` macro, so the literal goes through macro
;; expansion as well as `quote`: both must terminate on a cycle and keep
;; what the labels share. The macro's name is unusual, since test files share
;; an environment.

(define-syntax dll-quote
  (syntax-rules ()
    ((_ datum) 'datum)))

(test-group "datum labels in literals"

  (test "a circular list literal"
    #t
    (let ((x '#0=(a . #0#)))
      (eq? x (cdr x))))

  (test "a circular list literal's elements"
    '(a a a)
    (let ((x '#0=(a . #0#)))
      (list (car x) (cadr x) (caddr x))))

  (test "a literal sharing a list"
    #t
    (let ((x '(#1=(a) #1#)))
      (eq? (car x) (cadr x))))

  (test "a circular vector literal"
    #t
    (let ((v '#2=#(a #2#)))
      (eq? v (vector-ref v 1))))

  (test "a circular literal a macro quotes"
    #t
    (let ((x (dll-quote #3=(b . #3#))))
      (eq? x (cdr x))))

  (test "a circular literal in a dotted tail"
    'baz
    (let ((x '(bar . #4=(baz . #4#))))
      (car (cddr x)))))

;; R7RS 6.1: equal? must always terminate, even if its arguments are
;; circular, and is #t when their unfoldings into (possibly infinite) trees
;; are equal.
(define (dll-circular-list . elements)
  (let ((head (list-copy elements)))
    (set-cdr! (list-tail head (- (length head) 1)) head)
    head))

(test-group "equal? on circular and shared structure"

  (test "two circular lists of the same elements"
    #t
    (equal? (dll-circular-list 1 2) (dll-circular-list 1 2)))

  (test "two circular lists of different elements"
    #f
    (equal? (dll-circular-list 1 2) (dll-circular-list 1 3)))

  (test "a circular list and a list"
    #f
    (equal? (dll-circular-list 1 2) (list 1 2 1 2)))

  (test "circular lists with the same unfolding and different periods"
    #t
    (equal? (dll-circular-list 1) (dll-circular-list 1 1)))

  (test "pairs circular through their cars"
    #t
    (let ((a (list 1)) (b (list 1)))
      (set-car! a a)
      (set-car! b b)
      (equal? a b)))

  (test "two vectors containing themselves"
    #t
    (let ((a (vector 1 #f)) (b (vector 1 #f)))
      (vector-set! a 1 a)
      (vector-set! b 1 b)
      (equal? a b)))

  (test "circular literals"
    #t
    (equal? '#0=(a b . #0#) '#1=(a b a b . #1#)))

  (test "shared and unshared structure with the same unfolding"
    #t
    (let ((x (list 'a)))
      (equal? (list x x) '((a) (a)))))

  (test "long lists, equal"
    #t
    (equal? (make-list 20000 'x) (make-list 20000 'x)))

  (test "long lists, differing at the end"
    #f
    (equal? (append (make-list 20000 'x) '(y)) (append (make-list 20000 'x) '(z)))))
