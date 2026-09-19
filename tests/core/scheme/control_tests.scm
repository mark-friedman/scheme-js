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

  ;; letrec (R7RS 4.2.2) -- deliberately distinct from letrec* above.
  ;;
  ;; In letrec, ALL inits are evaluated before ANY variable is assigned, so a
  ;; later init cannot see an earlier variable's value; in letrec* it can. That
  ;; difference is the only externally observable one, and it is the reason the
  ;; two forms are not implemented as each other. Pinned here so that a change
  ;; to how letrec is expanded or analyzed cannot quietly collapse it into
  ;; letrec*. R7RS says reading an unassigned letrec variable is an error, so
  ;; these tests assert that the value is *not* the letrec* answer rather than
  ;; asserting any particular marker.
  (test-group "letrec"
    (test "letrec basic" 3
          (letrec ((a 1) (b 2)) (+ a b)))
    (test "letrec mutual recursion" #t
          (letrec ((ev? (lambda (n) (if (= n 0) #t (od? (- n 1)))))
                   (od? (lambda (n) (if (= n 0) #f (ev? (- n 1))))))
            (ev? 10)))
    (test "letrec self recursion" 120
          (letrec ((fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1)))))))
            (fact 5)))
    (test "letrec later init does not see an earlier variable" #t
          (not (equal? 1 (letrec ((a 1) (b a)) b))))
    (test "letrec* later init DOES see an earlier variable" 1
          (letrec* ((a 1) (b a)) b))
    (test "letrec evaluates inits left to right" '(2 1)
          (let ((log '()))
            (letrec ((a (begin (set! log (cons 1 log)) 1))
                     (b (begin (set! log (cons 2 log)) 2)))
              log)))
    (test "call/cc captured in a letrec init" 3
          (letrec ((a (call/cc (lambda (c) 1)))
                   (b 2))
            (+ a b))))

  ;; Named let and do both expand through letrec, so they are pinned here too.
  (test-group "named let and do"
    (test "named let accumulates" 10
          (let loop ((i 0) (a 0)) (if (> i 4) a (loop (+ i 1) (+ a i)))))
    (test "named let returns early" 'found
          (let scan ((xs '(1 2 3)))
            (cond ((null? xs) 'missing)
                  ((= (car xs) 2) 'found)
                  (else (scan (cdr xs))))))
    (test "named let shadows an outer binding of the same name" 7
          (let ((loop 99)) (let loop ((i 7)) i)))
    (test "internal defines are mutually recursive" #t
          (letrec ((probe (lambda (n)
                            (define (a k) (if (= k 0) #t (b (- k 1))))
                            (define (b k) (if (= k 0) #f (a (- k 1))))
                            (a n))))
            (probe 10)))
    (test "do accumulates" 10
          (do ((i 0 (+ i 1)) (a 0 (+ a i))) ((> i 4) a)))
    (test "do with empty body and a result" 5
          (do ((i 0 (+ i 1))) ((= i 5) i))))

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
