;; R7RS Compliance Tests - Section 5: Program Structure
;; Adapted from Chibi Scheme's R7RS test suite by Alex Shinn

(test-group "5 Program structure"

  (define add3
    (lambda (x) (+ x 3)))
  (test 6 (add3 3))
  (define first car)
  (test 1 (first '(1 2)))

  (test 45 (let ((x 5))
    (define foo (lambda (y) (bar x y)))
    (define bar (lambda (a b) (+ (* a b) a)))
    (foo (+ x 3))))

  (test 'ok
      (let ()
        (define-values () (values))
        'ok))
  (test 1
      (let ()
        (define-values (x) (values 1))
        x))
  (test 3
      (let ()
        (define-values x (values 1 2))
        (apply + x)))
  (test 3
      (let ()
        (define-values (x y) (values 1 2))
        (+ x y)))
  (test 6
      (let ()
        (define-values (x y z) (values 1 2 3))
        (+ x y z)))
  (test 10
      (let ()
        (define-values (x y . z) (values 1 2 3 4))
        (+ x y (car z) (cadr z))))

  (test '(2 1) (let ((x 1) (y 2))
    (define-syntax swap!
      (syntax-rules ()
        ((swap! a b)
         (let ((tmp a))
           (set! a b)
           (set! b tmp)))))
    (swap! x y)
    (list x y)))

  ;; Records - wrapped to isolate any crashes
  (test #t
    (let ()
      (define-record-type <pare>
        (kons x y)
        pare?
        (x kar set-kar!)
        (y kdr))
      (pare? (kons 1 2))))
      
  (test #f
    (let ()
      (define-record-type <pare>
        (kons x y)
        pare?
        (x kar set-kar!)
        (y kdr))
      (pare? (cons 1 2))))
      
  (test 1
    (let ()
      (define-record-type <pare>
        (kons x y)
        pare?
        (x kar set-kar!)
        (y kdr))
      (kar (kons 1 2))))
      
  (test 2
    (let ()
      (define-record-type <pare>
        (kons x y)
        pare?
        (x kar set-kar!)
        (y kdr))
      (kdr (kons 1 2))))
      
  (test 3
    (let ()
      (define-record-type <pare>
        (kons x y)
        pare?
        (x kar set-kar!)
        (y kdr))
      (let ((k (kons 1 2)))
        (set-kar! k 3)
        (kar k))))
)
