;; Tests for Standard Control Macros
;; (when, unless, or, let*, do, case)



(test-group "Control Macros"

  ;; when
  (test-group "when"
    (assert-equal "when true"
      (let ((x 0))
        (when #t (set! x 1) (set! x (+ x 1)))
        x)
      2)
    (assert-equal "when false"
      (let ((x 0))
        (when #f (set! x 1))
        x)
      0))

  ;; unless
  (test-group "unless"
    (assert-equal "unless false"
      (let ((x 0))
        (unless #f (set! x 1) (set! x (+ x 1)))
        x)
      2)
    (assert-equal "unless true"
      (let ((x 0))
        (unless #t (set! x 1))
        x)
      0))

  ;; or
  (test-group "or"
    (assert-equal "or empty" (or) #f)
    (assert-equal "or single true" (or 1) 1)
    (assert-equal "or single false" (or #f) #f)
    (assert-equal "or first true" (or 1 2 3) 1)
    (assert-equal "or last true" (or #f #f 3) 3)
    (assert-equal "or all false" (or #f #f #f) #f)
    (assert-equal "or short-circuit" (or #t (undefined-var)) #t))

  ;; let*
  (test-group "let*"
    (assert-equal "let* simple"
      (let* ((x 1) (y 2)) (+ x y))
      3)
    (assert-equal "let* sequential"
      (let* ((x 1) (y (+ x 1))) (+ x y))
      3)
    (assert-equal "let* shadowing"
      (let* ((x 1) (x 2)) x)
      2))

  ;; do
  (test-group "do"
    (assert-equal "do basic loop"
      (do ((i 0 (+ i 1))
           (sum 0 (+ sum i)))
          ((= i 5) sum))
      10) ;; 0+1+2+3+4 = 10
    (assert-equal "do empty body"
      (do ((i 0 (+ i 1)))
          ((= i 3) i))
      3))

  ;; case
  (test-group "case"
    (assert-equal "case match first"
      (case 'a ((a) 1) ((b) 2) (else 3))
      1)
    (assert-equal "case match second"
      (case 'b ((a) 1) ((b) 2) (else 3))
      2)
    (assert-equal "case match list"
      (case 'b ((a b c) 1) ((d) 2) (else 3))
      1)
    (assert-equal "case match else"
      (case 'z ((a) 1) ((b) 2) (else 3))
      3)
    (assert-equal "case no else match"
      (case 'z ((a) 1))
      (if #f #t)) ;; unspecified/void
    (assert-equal "case key evaluated once"
      (let ((count 0))
        (case (begin (set! count (+ count 1)) 'a)
          ((a) count)
          (else 99)))
      1))
)
