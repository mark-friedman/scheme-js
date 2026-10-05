;; BigInt Exactness Tests
;;
;; Tests for R7RS-compliant exactness using BigInt for exact integers
;; and Number for inexact reals.

(test-group "BigInt Exactness Tests"
  
  ;; ===== Exactness Predicates =====
  
  (test-group "exact? predicate"
    
    (test "exact? on integer literal"
      #t
      (exact? 5))
    
    (test "exact? on large integer"
      #t
      (exact? 9007199254740993))  ;; Beyond JS safe integer range
    
    (test "exact? on negative integer"
      #t
      (exact? -42))
    
    (test "exact? on zero"
      #t
      (exact? 0))
    
    (test "exact? on rational"
      #t
      (exact? 1/3))
    
    (test "exact? on float"
      #f
      (exact? 5.0))
    
    (test "exact? on decimal"
      #f
      (exact? 3.14))
  )
  
  (test-group "inexact? predicate"
    
    (test "inexact? on integer literal"
      #f
      (inexact? 5))
    
    (test "inexact? on float"
      #t
      (inexact? 5.0))
    
    (test "inexact? on decimal"
      #t
      (inexact? 3.14))
    
    (test "inexact? on rational"
      #f
      (inexact? 1/3))
  )
  
  ;; ===== Conversion Procedures =====
  
  (test-group "inexact procedure"
    
    (test "inexact converts exact integer to inexact"
      #t
      (inexact? (inexact 5)))
    
    (test "inexact preserves value"
      5.0
      (inexact 5))
    
    (test "inexact on float is no-op"
      3.14
      (inexact 3.14))
    
    (test "inexact on rational"
      0.5
      (inexact 1/2))
  )
  
  (test-group "exact procedure"
    
    (test "exact converts inexact integer to exact"
      #t
      (exact? (exact 5.0)))
    
    (test "exact preserves integer value"
      5
      (exact 5.0))
    
    (test "exact on exact integer is no-op"
      5
      (exact 5))
  )
  
  ;; ===== Mixed Arithmetic =====
  
  (test-group "exactness propagation"
    
    (test "exact + exact = exact"
      #t
      (exact? (+ 2 3)))
    
    (test "exact + inexact = inexact"
      #t
      (inexact? (+ 2 3.0)))
    
    (test "inexact + inexact = inexact"
      #t
      (inexact? (+ 2.0 3.0)))
    
    (test "exact * exact = exact"
      #t
      (exact? (* 2 3)))
    
    (test "exact * inexact = inexact"
      #t
      (inexact? (* 2 3.0)))
  )
  
  ;; ===== Integer? with Exactness =====
  
  (test-group "integer? with exactness"
    
    (test "integer? on exact integer"
      #t
      (integer? 5))
    
    (test "integer? on inexact integer (5.0)"
      #t
      (integer? 5.0))
    
    (test "integer? on non-integer float"
      #f
      (integer? 3.14))
    
    (test "exact-integer? on exact integer"
      #t
      (exact-integer? 5))
    
    (test "exact-integer? on inexact integer (5.0)"
      #f
      (exact-integer? 5.0))
  )
  
  ;; ===== Equality with Mixed Exactness =====
  
  (test-group "numeric equality"
    
    (test "= compares values regardless of exactness"
      #t
      (= 5 5.0))
    
    (test "eqv? distinguishes exact vs inexact"
      #f
      (eqv? 5 5.0))
  )
  
  ;; ===== Reader Syntax =====
  
  (test-group "reader exactness syntax"
    
    (test "#e forces exact"
      #t
      (exact? #e5.0))
    
    (test "#i forces inexact"
      #t
      (inexact? #i5))
    
    (test "#e5.0 value"
      5
      #e5.0)
    
    (test "#i5 value equals 5"
      #t
      (= #i5 5))
  )

) ;; end test-group

;; exact-integer-sqrt (R7RS 6.2.6) on exact integers of every size: the root
;; s and remainder r of k have s*s + r = k, 0 <= r, and k < (s+1)^2. The
;; large cases are also what `pi` and `chudnovsky` spend their time in, so a
;; root found slowly shows here as a test that takes seconds.

;; /**
;;  * Whether exact-integer-sqrt's root and remainder of k are right.
;;  * @param {integer} k - A non-negative exact integer.
;;  * @returns {boolean}
;;  */
(define (isqrt-right? k)
  (call-with-values (lambda () (exact-integer-sqrt k))
    (lambda (s r)
      (and (exact? s) (exact? r)
           (= k (+ (* s s) r))
           (<= 0 r)
           (< k (* (+ s 1) (+ s 1)))))))

;; /**
;;  * The integers among which exact-integer-sqrt is wrong, of those given.
;;  * @param {list} ks - Non-negative exact integers.
;;  * @returns {list}
;;  */
(define (isqrt-wrong ks)
  (cond ((null? ks) '())
        ((isqrt-right? (car ks)) (isqrt-wrong (cdr ks)))
        (else (cons (car ks) (isqrt-wrong (cdr ks))))))

;; /**
;;  * A square, one less than it, and one more: the integers a root is most
;;  * easily wrong about.
;;  * @param {integer} s - The root.
;;  * @returns {list}
;;  */
(define (around-square s)
  (list (- (* s s) 1) (* s s) (+ (* s s) 1)))

(test-group "exact-integer-sqrt, of every size"
  (test "small integers" '() (isqrt-wrong '(0 1 2 3 4 5 8 9 10 15 16 17 24 25 26 99 100 101)))
  (test "around the largest integer a double holds exactly"
        '()
        (isqrt-wrong (append (around-square 94906265) (around-square 94906266)
                             (list (expt 2 52) (- (expt 2 53) 1) (expt 2 53) (+ (expt 2 53) 1)))))
  (test "around squares of hundreds of digits"
        '()
        (isqrt-wrong (append (around-square (expt 10 150)) (around-square (+ (expt 3 400) 7)))))
  (test "a power of ten with an odd number of digits, whose root begins as the root of 10 does"
        3162
        (call-with-values (lambda () (exact-integer-sqrt (expt 10 2001))) (lambda (s r) (quotient s (expt 10 997)))))
  (test "around squares of thousands of digits"
        '()
        (isqrt-wrong (append (around-square (- (expt 7 5000) 1)) (list (* 2 (expt 10 6000))))))
  (test "the root of a square of ten thousand digits"
        (expt 13 4500)
        (call-with-values (lambda () (exact-integer-sqrt (expt 13 9000))) (lambda (s r) s))))
