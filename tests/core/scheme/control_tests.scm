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
      1)
    ;; case with => syntax
    (test "case else =>" 'x
          (case 'x
            ((a b) 'ab)
            (else => (lambda (v) v))))
    (test "case datum =>" 'matched
          (case 'b
            ((a) 'a-matched)
            ((b c) => (lambda (v) 'matched))
            (else 'no-match))))

  ;; letrec*
  (test-group "letrec*"
    (test "letrec* basic" 6
          (letrec* ((x 1)
                    (y (+ x 2))
                    (z (+ y 3)))
            z))
    (test "letrec* mutual recursion" #t
          (letrec* ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                    (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
            (even? 10))))

  ;; let-values
  (test-group "let-values"
    (test "let-values basic" 5
          (let-values (((a b) (values 2 3)))
            (+ a b)))
    (test "let-values with exact-integer-sqrt" 35
          (let-values (((root rem) (exact-integer-sqrt 32)))
            (* root rem)))
    (test "let-values empty" 42
          (let-values () 42))
    (test "let-values single" 7
          (let-values (((x) (values 7)))
            x))
    (test "let-values with floor/" '(2 1)
          (let-values (((q r) (floor/ 5 2)))
            (list q r))))

  ;; let*-values
  (test-group "let*-values"
    (test "let*-values sequential" 10
          (let*-values (((a b) (values 2 3))
                        ((c) (values (+ a b))))
            (* c 2))))
)
