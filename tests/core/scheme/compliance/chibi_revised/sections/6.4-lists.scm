;; R7RS Compliance Tests - Section 6.4: Lists (Pairs and Lists)
;; Adapted from Chibi Scheme's R7RS test suite by Alex Shinn

(test-group "6.4 Lists"

  (let* ((x (list 'a 'b 'c))
         (y x))
    (test '(a b c) (values y))
    (test #t (list? y))
    (set-cdr! x 4)
    (test '(a . 4) (values x))
    (test #t (eqv? x y))
    (test #f (list? y))
    (set-cdr! x x)
    (test #f (list? x)))

  (test #t (pair? '(a . b)))
  (test #t (pair? '(a b c)))
  (test #f (pair? '()))
  (test #f (pair? '#(a b)))

  (test '(a) (cons 'a '()))
  (test '((a) b c d) (cons '(a) '(b c d)))
  (test '("a" b c) (cons "a" '(b c)))
  (test '(a . 3) (cons 'a 3))
  (test '((a b) . c) (cons '(a b) 'c))

  (test 'a (car '(a b c)))
  (test '(a) (car '((a) b c d)))
  (test 1 (car '(1 . 2)))

  (test '(b c d) (cdr '((a) b c d)))
  (test 2 (cdr '(1 . 2)))
  (define (g) '(constant-list))

  (test #t (list? '(a b c)))
  (test #t (list? '()))
  (test #f (list? '(a . b)))
  (test #f (let ((x (list 'a))) (set-cdr! x x) (list? x)))

  (test '(3 3) (make-list 2 3))

  (test '(a 7 c) (list 'a (+ 3 4) 'c))
  (test '() (list))

  (test 3 (length '(a b c)))
  (test 3 (length '(a (b) (c d e))))
  (test 0 (length '()))

  (test '(x y) (append '(x) '(y)))
  (test '(a b c d) (append '(a) '(b c d)))
  (test '(a (b) (c)) (append '(a (b)) '((c))))

  (test '(a b c . d) (append '(a b) '(c . d)))
  (test 'a (append '() 'a))

  (test '(c b a) (reverse '(a b c)))
  (test '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

  (test '(d e) (list-tail '(a b c d e) 3))

  (test 'c (list-ref '(a b c d) 2))
  (test 'c (list-ref '(a b c d)
            (exact (round 1.8))))

  (test '(0 ("Sue" "Sue") "Anna")
      (let ((lst (list 0 '(2 2 2 2) "Anna")))
        (list-set! lst 1 '("Sue" "Sue"))
        lst))

  (test '(a b c) (memq 'a '(a b c)))
  (test '(b c) (memq 'b '(a b c)))
  (test #f (memq 'a '(b c d)))
  (test #f (memq (list 'a) '(b (a) c)))
  (test '((a) c) (member (list 'a) '(b (a) c)))
  (test '("b" "c") (member "B" '("a" "b" "c") string-ci=?))
  (test '(101 102) (memv 101 '(100 101 102)))

  (let ()
    (define e '((a 1) (b 2) (c 3)))
    (test '(a 1) (assq 'a e))
    (test '(b 2) (assq 'b e))
    (test #f (assq 'd e)))

  (test #f (assq (list 'a) '(((a)) ((b)) ((c)))))
  (test '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))
  (test '(2 4) (assoc 2.0 '((1 1) (2 4) (3 9)) =))
  (test '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

  (test '(1 2 3) (list-copy '(1 2 3)))
  (test "foo" (list-copy "foo"))
  (test '() (list-copy '()))
  (test '(3 . 4) (list-copy '(3 . 4)))
  (test '(6 7 8 . 9) (list-copy '(6 7 8 . 9)))
  (let* ((l1 '((a b) (c d) e))
         (l2 (list-copy l1)))
    (test l2 '((a b) (c d) e))
    (test #t (eq? (car l1) (car l2)))
    (test #t (eq? (cadr l1) (cadr l2)))
    (test #f (eq? (cdr l1) (cdr l2)))
    (test #f (eq? (cddr l1) (cddr l2))))
)
