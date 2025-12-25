;; (scheme time) library tests
;;
;; Tests for current-second, current-jiffy, jiffies-per-second

(test-group "(scheme time) tests"
  
  ;; ===== current-second tests =====
  
  (test "current-second returns number"
    (number? (current-second))
    #t)
  
  (test "current-second returns real"
    (real? (current-second))
    #t)
  
  (test "current-second is positive"
    (> (current-second) 0)
    #t)
  
  (test "current-second after Unix epoch"
    (> (current-second) 1700000000)  ; After Nov 2023
    #t)
  
  (test "current-second progresses"
    (let ((t1 (current-second)))
      ;; Time should not go backward
      (>= (current-second) t1))
    #t)
  
  ;; ===== current-jiffy tests =====
  
  (test "current-jiffy returns integer"
    (integer? (current-jiffy))
    #t)
  
  (test "current-jiffy returns exact"
    (exact? (current-jiffy))
    #t)
  
  (test "current-jiffy is non-negative"
    (>= (current-jiffy) 0)
    #t)
  
  (test "current-jiffy progresses"
    (let ((j1 (current-jiffy)))
      ;; Should not go backward
      (>= (current-jiffy) j1))
    #t)
  
  ;; ===== jiffies-per-second tests =====
  
  (test "jiffies-per-second is 1000"
    (jiffies-per-second)
    1000)
  
  (test "jiffies-per-second returns integer"
    (integer? (jiffies-per-second))
    #t)
  
  (test "jiffies-per-second is positive"
    (> (jiffies-per-second) 0)
    #t)
  
  ;; ===== Arity tests =====
  
  (test "current-second no args required"
    (guard (e (#t 'error))
      (current-second 'extra-arg))
    'error)
  
  (test "current-jiffy no args required"
    (guard (e (#t 'error))
      (current-jiffy 'extra-arg))
    'error)
  
  (test "jiffies-per-second no args required"
    (guard (e (#t 'error))
      (jiffies-per-second 'extra-arg))
    'error)
  
  ) ;; end test-group
