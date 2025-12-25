;; (scheme lazy) library tests
;;
;; Tests for delay, force, delay-force, make-promise, promise?

(test-group "(scheme lazy) tests"
  
  ;; ===== promise? tests =====
  
  (test "promise? on make-promise"
    (promise? (make-promise 42))
    #t)
  
  (test "promise? on delay"
    (promise? (delay 42))
    #t)
  
  (test "promise? on number"
    (promise? 42)
    #f)
  
  (test "promise? on list"
    (promise? '(1 2 3))
    #f)
  
  (test "promise? on procedure"
    (promise? (lambda () 42))
    #f)
  
  ;; ===== make-promise tests =====
  
  (test "make-promise wraps value"
    (force (make-promise 42))
    42)
  
  (test "make-promise wraps list"
    (force (make-promise '(a b c)))
    '(a b c))
  
  (test "make-promise is idempotent"
    (let ((p (make-promise 100)))
      (eq? (make-promise p) p))
    #t)
  
  ;; ===== delay tests =====
  
  (test "delay creates promise"
    (promise? (delay (+ 1 2)))
    #t)
  
  (test "delay does not evaluate immediately"
    (let ((evaluated #f))
      (let ((p (delay (begin (set! evaluated #t) 42))))
        evaluated))
    #f)
  
  (test "delay evaluates on force"
    (force (delay (+ 1 2)))
    3)
  
  ;; ===== force tests =====
  
  (test "force on delay"
    (force (delay (* 6 7)))
    42)
  
  (test "force memoizes result"
    (let ((count 0))
      (let ((p (delay (begin (set! count (+ count 1)) count))))
        (force p)
        (force p)
        (force p)
        count))
    1)
  
  (test "force on make-promise"
    (force (make-promise 'hello))
    'hello)
  
  ;; ===== delay-force tests =====
  
  (test "delay-force creates promise"
    (promise? (delay-force (delay 42)))
    #t)
  
  (test "delay-force evaluates nested delay"
    (force (delay-force (delay 100)))
    100)
  
  ;; Iterative lazy stream example
  (test "delay-force for lazy iteration"
    (letrec ((countdown
              (lambda (n)
                (if (= n 0)
                    (delay 'done)
                    (delay-force (countdown (- n 1)))))))
      (force (countdown 10)))
    'done)
  
  ;; ===== Error cases =====
  
  (test "force on non-promise errors"
    (guard (e (#t 'error-caught))
      (force 42))
    'error-caught)
  
  ) ;; end test-group
