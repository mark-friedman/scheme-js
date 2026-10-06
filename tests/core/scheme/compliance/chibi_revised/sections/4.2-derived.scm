;; R7RS Compliance Tests - Section 4.2 Derived expression types
;; Adapted from Chibi Scheme's R7RS test suite by Alex Shinn: the section as
;; chibi_original/r7rs-tests.scm has it, every test as Chibi wrote it, run with
;; Chibi's test forms and compared as Chibi compares (../test-equal.scm).

(test-group "4.2 Derived expression types"

(test 'greater
    (cond ((> 3 2) 'greater)
          ((< 3 2) 'less)))

(test 'equal
    (cond ((> 3 3) 'greater)
          ((< 3 3) 'less)
          (else 'equal)))

(test 2
    (cond ((assv 'b '((a 1) (b 2))) => cadr)
          (else #f)))

(test 'composite
    (case (* 2 3)
      ((2 3 5 7) 'prime)
      ((1 4 6 8 9) 'composite)))

(test 'c
    (case (car '(c d))
      ((a e i o u) 'vowel)
      ((w y) 'semivowel)
      (else => (lambda (x) x))))

(test '((other . z) (semivowel . y) (other . x)
        (semivowel . w) (vowel . u))
    (map (lambda (x)
           (case x
             ((a e i o u) => (lambda (w) (cons 'vowel w)))
             ((w y) (cons 'semivowel x))
             (else => (lambda (w) (cons 'other w)))))
         '(z y x w u)))

(test #t (and (= 2 2) (> 2 1)))
(test #f (and (= 2 2) (< 2 1)))
(test '(f g) (and 1 2 'c '(f g)))
(test #t (and))

(test #t (or (= 2 2) (> 2 1)))
(test #t (or (= 2 2) (< 2 1)))
(test #f (or #f #f #f))
(test '(b c) (or (memq 'b '(a b c))
    (/ 3 0)))

(test 6 (let ((x 2) (y 3))
  (* x y)))

(test 35 (let ((x 2) (y 3))
  (let ((x 7)
        (z (+ x y)))
    (* z x))))

(test 70 (let ((x 2) (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x))))

(test #t
    (letrec ((even?
              (lambda (n)
                (if (zero? n)
                    #t
                    (odd? (- n 1)))))
             (odd?
              (lambda (n)
                (if (zero? n)
                    #f
                    (even? (- n 1))))))
      (even? 88)))

(test 5
    (letrec* ((p
               (lambda (x)
                 (+ 1 (q (- x 1)))))
              (q
               (lambda (y)
                 (if (zero? y)
                     0
                     (+ 1 (p (- y 1))))))
              (x (p 5))
              (y x))
             y))

;; By Jussi Piitulainen <jpiitula@ling.helsinki.fi>
;; and John Cowan <cowan@mercury.ccil.org>:
;; http://lists.scheme-reports.org/pipermail/scheme-reports/2013-December/003876.html
(define (means ton)
  (letrec*
     ((mean
        (lambda (f g)
          (f (/ (sum g ton) n))))
      (sum
        (lambda (g ton)
          (if (null? ton)
            (+)
            (if (number? ton)
                (g ton)
                (+ (sum g (car ton))
                   (sum g (cdr ton)))))))
      (n (sum (lambda (x) 1) ton)))
    (values (mean values values)
            (mean exp log)
            (mean / /))))
(let*-values (((a b c) (means '(8 5 99 1 22))))
  (test 27 a)
  (test 9.728 b)
  (test 1800/497 c))

(let*-values (((root rem) (exact-integer-sqrt 32)))
  (test 35 (* root rem)))

(test '(1073741824 0)
    (let*-values (((root rem) (exact-integer-sqrt (expt 2 60))))
      (list root rem)))

(test '(1518500249 3000631951)
    (let*-values (((root rem) (exact-integer-sqrt (expt 2 61))))
      (list root rem)))

(test '(815238614083298888 443242361398135744)
    (let*-values (((root rem) (exact-integer-sqrt (expt 2 119))))
      (list root rem)))

(test '(1152921504606846976 0)
    (let*-values (((root rem) (exact-integer-sqrt (expt 2 120))))
      (list root rem)))

(test '(1630477228166597776 1772969445592542976)
    (let*-values (((root rem) (exact-integer-sqrt (expt 2 121))))
      (list root rem)))

(test '(31622776601683793319 62545769258890964239)
    (let*-values (((root rem) (exact-integer-sqrt (expt 10 39))))
      (list root rem)))

(let*-values (((root rem) (exact-integer-sqrt (expt 2 140))))
  (test 0 rem)
  (test (expt 2 140) (square root)))

(test '(x y x y) (let ((a 'a) (b 'b) (x 'x) (y 'y))
  (let*-values (((a b) (values x y))
                ((x y) (values a b)))
    (list a b x y))))

(test 'ok (let-values () 'ok))

(test 1 (let ((x 1))
	  (let*-values ()
	    (define x 2)
	    #f)
	  x))

(let ()
  (define x 0)
  (set! x 5)
  (test 6 (+ x 1)))

(test #(0 1 2 3 4) (do ((vec (make-vector 5))
     (i 0 (+ i 1)))
    ((= i 5) vec)
  (vector-set! vec i i)))

(test 25 (let ((x '(1 3 5 7 9)))
  (do ((x x (cdr x))
       (sum 0 (+ sum (car x))))
      ((null? x) sum))))

(test '((6 1 3) (-5 -2))
    (let loop ((numbers '(3 -2 1 6 -5))
               (nonneg '())
               (neg '()))
      (cond ((null? numbers) (list nonneg neg))
            ((>= (car numbers) 0)
             (loop (cdr numbers)
                   (cons (car numbers) nonneg)
                   neg))
            ((< (car numbers) 0)
             (loop (cdr numbers)
                   nonneg
                   (cons (car numbers) neg))))))

(test 3 (force (delay (+ 1 2))))

(test '(3 3)  
    (let ((p (delay (+ 1 2))))
      (list (force p) (force p))))

(define integers
  (letrec ((next
            (lambda (n)
              (delay (cons n (next (+ n 1)))))))
    (next 0)))
(define head
  (lambda (stream) (car (force stream))))
(define tail
  (lambda (stream) (cdr (force stream))))

(test 2 (head (tail (tail integers))))

(define (stream-filter p? s)
  (delay-force
   (if (null? (force s)) 
       (delay '())
       (let ((h (car (force s)))
             (t (cdr (force s))))
         (if (p? h)
             (delay (cons h (stream-filter p? t)))
             (stream-filter p? t))))))

(test 5 (head (tail (tail (stream-filter odd? integers)))))

(let ()
  (define x 5)
  (define count 0)
  (define p
    (delay (begin (set! count (+ count 1))
                  (if (> count x)
                      count
                      (force p)))))
  (test 6 (force p))
  (test 6 (begin (set! x 10) (force p))))

(test #t (promise? (delay (+ 2 2))))
(test #t (promise? (make-promise (+ 2 2))))
(test #t
    (let ((x (delay (+ 2 2))))
      (force x)
      (promise? x)))
(test #t
    (let ((x (make-promise (+ 2 2))))
      (force x)
      (promise? x)))
(test 4 (force (make-promise (+ 2 2))))
(test 4 (force (make-promise (make-promise (+ 2 2)))))

(define radix
  (make-parameter
   10
   (lambda (x)
     (if (and (integer? x) (<= 2 x 16))
         x
         (error "invalid radix")))))
(define (f n) (number->string n (radix)))
(test "12" (f 12))
(test "1100" (parameterize ((radix 2))
  (f 12)))
(test "12" (f 12))

(test '(list 3 4) `(list ,(+ 1 2) 4))
(let ((name 'a)) (test '(list a (quote a)) `(list ,name ',name)))
(test '(a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
(test #(10 5 4 16 9 8)
    `#(10 5 ,(square 2) ,@(map square '(4 3)) 8))
(test '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
    `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) )
(let ((name1 'x)
      (name2 'y))
   (test '(a `(b ,x ,'y d) e) `(a `(b ,,name1 ,',name2 d) e)))
(test '(list 3 4) (quasiquote (list (unquote (+ 1 2)) 4)) )
(test `(list ,(+ 1 2) 4) (quasiquote (list (unquote (+ 1 2)) 4)))

(define any-arity
  (case-lambda 
    (() 'zero)
    ((x) x)
    ((x y) (cons x y))
    ((x y z) (list x y z))
    (args (cons 'many args))))

(test 'zero (any-arity))
(test 1 (any-arity 1))
(test '(1 . 2) (any-arity 1 2))
(test '(1 2 3) (any-arity 1 2 3))
(test '(many 1 2 3 4) (any-arity 1 2 3 4))

(define rest-arity
  (case-lambda 
    (() '(zero))
    ((x) (list 'one x))
    ((x y) (list 'two x y))
    ((x y . z) (list 'more x y z))))

(test '(zero) (rest-arity))
(test '(one 1) (rest-arity 1))
(test '(two 1 2) (rest-arity 1 2))
(test '(more 1 2 (3)) (rest-arity 1 2 3))

(define dead-clause
  (case-lambda
    ((x . y) 'many)
    (() 'none)
    (foo 'unreachable)))

(test 'none (dead-clause))
(test 'many (dead-clause 1))
(test 'many (dead-clause 1 2))
(test 'many (dead-clause 1 2 3))

)
