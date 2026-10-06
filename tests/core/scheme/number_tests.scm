;; Number Tests
;;
;; Comprehensive tests for basic numeric operations (integers, floats)

(test-group "Number tests"
  
  ;; ===== Arithmetic Operations =====
  
  (test-group "addition"
    
    (test "add zero args"
      0
      (+))
    
    (test "add one arg"
      5
      (+ 5))
    
    (test "add two args"
      7
      (+ 3 4))
    
    (test "add multiple args"
      15
      (+ 1 2 3 4 5))
    
    (test "add negatives"
      -5
      (+ -2 -3))
    
    (test "add mixed signs"
      2
      (+ 5 -3))
    
    (test "add floats"
      5.5
      (+ 2.5 3.0))
  )
  
  (test-group "subtraction"
    
    (test "negate single arg"
      -5
      (- 5))
    
    (test "subtract two args"
      3
      (- 7 4))
    
    (test "subtract multiple"
      0
      (- 10 5 3 2))
    
    (test "subtract negative"
      8
      (- 5 -3))
    
    (test "subtract floats"
      1.5
      (- 4.0 2.5))
  )
  
  (test-group "multiplication"
    
    (test "multiply zero args"
      1
      (*))
    
    (test "multiply one arg"
      5
      (* 5))
    
    (test "multiply two args"
      12
      (* 3 4))
    
    (test "multiply multiple"
      120
      (* 1 2 3 4 5))
    
    (test "multiply by zero"
      0
      (* 5 0 3))
    
    (test "multiply negatives"
      6
      (* -2 -3))
    
    (test "multiply mixed signs"
      -15
      (* 5 -3))
  )
  
  (test-group "division"
    
    (test "reciprocal"
      1/2
      (/ 2))
    
    (test "divide two args"
      3
      (/ 12 4))
    
    (test "divide multiple"
      2
      (/ 24 3 4))
    
    (test "divide negative"
      -3
      (/ -12 4))
    
    (test "divide floats"
      2.75
      (/ 5.5 2))
  )
  
  ;; ===== Comparison =====
  
  (test-group "comparison"
    
    (test "= equal"
      #t
      (= 5 5))
    
    (test "= not equal"
      #f
      (= 5 6))
    
    (test "= multiple equal"
      #t
      (= 3 3 3 3))
    
    (test "= one different"
      #f
      (= 3 3 4 3))
    
    (test "< increasing"
      #t
      (< 1 2 3 4))
    
    (test "< not increasing"
      #f
      (< 1 3 2))
    
    (test "> decreasing"
      #t
      (> 4 3 2 1))
    
    (test "> not decreasing"
      #f
      (> 4 2 3))
    
    (test "<= with equal"
      #t
      (<= 1 2 2 3))
    
    (test ">= with equal"
      #t
      (>= 3 2 2 1))
  )
  
  ;; ===== Integer Division =====
  
  (test-group "integer division"
    
    (test "quotient positive"
      3
      (quotient 10 3))
    
    (test "quotient negative dividend"
      -3
      (quotient -10 3))
    
    (test "quotient negative divisor"
      -3
      (quotient 10 -3))
    
    (test "quotient both negative"
      3
      (quotient -10 -3))
    
    (test "remainder positive"
      1
      (remainder 10 3))
    
    (test "remainder negative dividend"
      -1
      (remainder -10 3))
    
    (test "modulo positive"
      1
      (modulo 10 3))
    
    (test "modulo different from remainder"
      2
      (modulo -10 3))
  )
  
  ;; ===== Type Predicates =====
  
  (test-group "type predicates"
    
    (test "number? on integer"
      #t
      (number? 5))
    
    (test "number? on float"
      #t
      (number? 3.14))
    
    (test "number? on string"
      #f
      (number? "5"))
    
    (test "integer? on integer"
      #t
      (integer? 42))
    
    (test "integer? on float"
      #f
      (integer? 3.14))
    
    (test "integer? on whole float"
      #t
      (integer? 5.0))
    
    (test "real? on float"
      #t
      (real? 3.14))
    
    (test "rational? on finite"
      #t
      (rational? 3.14))
    
    (test "exact-integer? on integer"
      #t
      (exact-integer? 42))
    
    (test "exact-integer? on float"
      #f
      (exact-integer? 42.5))
  )
  
  ;; ===== Math Functions =====
  
  (test-group "math functions"
    
    (test "abs positive"
      5
      (abs 5))
    
    (test "abs negative"
      5
      (abs -5))
    
    (test "abs zero"
      0
      (abs 0))
    
    (test "floor"
      3.0
      (floor 3.7))
    
    (test "floor negative"
      -4.0
      (floor -3.2))
    
    (test "ceiling"
      4.0
      (ceiling 3.2))
    
    (test "ceiling negative"
      -3.0
      (ceiling -3.7))
    
    (test "truncate positive"
      3.0
      (truncate 3.9))
    
    (test "truncate negative"
      -3.0
      (truncate -3.9))
    
    (test "expt square"
      25
      (expt 5 2))
    
    (test "expt cube"
      8
      (expt 2 3))
    
    ;; R7RS 6.2.6's own example: the root of an exact square is exact.
    (test "sqrt"
      5
      (sqrt 25))
    
    ;; square
    (test "square zero" 0 (square 0))
    (test "square positive" 4 (square 2))
    (test "square negative" 9 (square -3))
    (test "square large" 1764 (square 42))
    (test "square float" 6.25 (square 2.5))
    
    ;; exact (converts to exact, rounds floats)
    (test "exact integer" 5 (exact 5))
    (test "exact float" 5 (exact 5.0))
    
    ;; inexact tests - now work with BigInt/Number distinction
    ;; (inexact 5) converts BigInt 5 to Number 5.0, which is inexact
    (test "inexact integer becomes inexact" #t (inexact? (inexact 5)))
  )
  
  ;; ===== Special Values =====
  
  (test-group "special values"
    
    (test "positive infinity"
      #t
      (infinite? +inf.0))
    
    (test "negative infinity"
      #t
      (infinite? -inf.0))
    
    (test "nan"
      #t
      (nan? +nan.0))
    
    (test "finite number"
      #t
      (finite? 5))
    
    (test "infinity not finite"
      #f
      (finite? +inf.0))
  )
  
  ;; ===== Predicates =====
  
  (test-group "numeric predicates"
    
    (test "zero? on zero"
      #t
      (zero? 0))
    
    (test "zero? on nonzero"
      #f
      (zero? 5))
    
    (test "positive? on positive"
      #t
      (positive? 5))
    
    (test "positive? on negative"
      #f
      (positive? -5))
    
    (test "negative? on negative"
      #t
      (negative? -5))
    
    (test "negative? on positive"
      #f
      (negative? 5))
    
    (test "odd? on odd"
      #t
      (odd? 7))
    
    (test "odd? on even"
      #f
      (odd? 8))
    
    (test "even? on even"
      #t
      (even? 8))
    
    (test "even? on odd"
      #f
      (even? 7))
  )
  
  ;; ===== Error Cases =====
  
  (test-group "error handling"
    
    (test "+ with non-number"
      'error
      (guard (e (#t 'error))
        (+ 1 "a")))
    
    (test "quotient with non-integer"
      'error
      (guard (e (#t 'error))
        (quotient 10.5 3)))
    
    ;; A NaN stands for a non-real value only where there are no complex
    ;; numbers (R7RS 6.2.4); here (sqrt -1) is +i, as R7RS 6.2.6 has it.
    (test "sqrt of negative"
      +i
      (sqrt -1))
  )

  ;; ===== Rational Support =====
  
  (test-group "rational arithmetic"
    
    (test "round rational 7/2"
      4
      (round 7/2))
    
    (test "round rational 5/2"
      2
      (round 5/2))  ;; round to even
    
    (test "floor rational"
      3
      (floor 7/2))
    
    (test "ceiling rational"
      4
      (ceiling 7/2))
    
    (test "inexact rational"
      3.5
      (inexact 7/2))
  )

  ;; ===== String->Number =====
  
  (test-group "string->number"
    
    (test "scientific notation"
      100.0
      (string->number "1e2"))
    
    (test "scientific notation with decimal"
      1.5e10
      (string->number "1.5e10"))
    
    (test "basic integer"
      100
      (string->number "100"))
    
    (test "hex radix"
      256
      (string->number "100" 16))

    (test "a complex number that begins with its sign and no digit"
      '(#t 0 1)
      (let ((i (string->number "+i")))
        (list (number? i) (real-part i) (imag-part i))))
    
    (test "invalid string returns false"
      #f
      (string->number "1 2"))
    
    (test "positive infinity"
      +inf.0
      (string->number "+inf.0"))
    
    (test "negative infinity"
      -inf.0
      (string->number "-inf.0"))
  )

) ;; end test-group

;; ===== Numeric Comparison Across the Tower =====
;;
;; Comparison must work across exact integers, rationals and inexact reals.
;; These were previously compared with JavaScript's `<` and `===` applied
;; directly to the representation objects, which fell back to string or
;; identity comparison for rationals and produced wrong answers.

(test-group "comparison across the numeric tower"

  (test-group "exact integers"

    (test "less than"
      #t
      (< 1 2))

    (test "not less than"
      #f
      (< 2 1))

    (test "chained increasing"
      #t
      (< 1 2 3))

    (test "chained not increasing"
      #f
      (< 1 3 2))

    (test "greater than chained"
      #t
      (> 3 2 1))

    (test "less or equal on equal values"
      #t
      (<= 1 1))

    (test "greater or equal on equal values"
      #t
      (>= 2 2)))

  (test-group "rationals"

    (test "rational equal to itself"
      #t
      (= 1/2 1/2))

    (test "unreduced rational equals reduced"
      #t
      (= 1/2 2/4))

    (test "distinct rationals not equal"
      #f
      (= 1/2 1/3))

    (test "rational ordering"
      #t
      (< 1/3 1/2))

    (test "rational ordering reversed"
      #f
      (< 1/2 1/3))

    ;; Lexicographic comparison of the printed form would get this wrong,
    ;; since "1/2" sorts before "10/3" only by accident of digit order.
    (test "rational ordering with multi-digit numerator"
      #t
      (< 1/2 10/3))

    (test "rational ordering with multi-digit numerator reversed"
      #f
      (< 10/3 1/2))

    (test "rational less than integer"
      #t
      (< 1/2 1))

    (test "integer less than rational"
      #t
      (< 1 3/2))

    (test "rational greater than integer"
      #t
      (> 3/2 1))

    (test "rational equal to integer"
      #t
      (= 4/2 2)))

  (test-group "mixed exactness"

    (test "exact integer equals inexact"
      #t
      (= 1 1.0))

    (test "exact integer less than inexact"
      #t
      (< 1 1.5))

    (test "inexact less than exact"
      #t
      (< 0.5 1))

    (test "rational equals inexact"
      #t
      (= 1/2 0.5))

    (test "rational less than inexact"
      #t
      (< 1/2 0.75))

    (test "inexact less than rational"
      #t
      (< 0.25 1/2))

    (test "chained mixed exactness"
      #t
      (< 1/4 0.5 1)))

  (test-group "large exact values"

    ;; Comparing via double precision would lose these distinctions, so exact
    ;; operands must be compared exactly rather than converted to floats.
    (test "large integers differing beyond double precision"
      #t
      (< 10000000000000000000000000001 10000000000000000000000000002))

    (test "large integers not less than"
      #f
      (< 10000000000000000000000000002 10000000000000000000000000001))

    (test "large integers equal"
      #t
      (= 10000000000000000000000000001 10000000000000000000000000001))

    (test "large rationals compare exactly"
      #t
      (< 1/10000000000000000000000000002 1/10000000000000000000000000001))))

;; ===== Continuation Capture During Argument Evaluation =====
;;
;; Argument evaluation builds up a partially-filled application frame. If those
;; frames were mutated in place rather than rebuilt, a continuation captured
;; mid-evaluation would observe later mutations, and re-invoking it would see
;; the wrong arguments. These tests pin that behaviour so any future change to
;; the frame representation has to preserve it.

(test-group "continuations captured during argument evaluation"

  ;; A continuation captured while evaluating the second argument of a
  ;; three-argument call. Re-invoking it must re-run the call with the already
  ;; evaluated arguments intact.
  (test "escape from the middle of an argument list"
    111
    (call/cc
      (lambda (k)
        (+ 1 (k 111) 1000))))

  ;; Re-entrant capture: the continuation is invoked after it has already
  ;; returned normally, which requires the captured frame to be independent of
  ;; the one the original computation went on to use.
  ;; Runs three times: the initial call returns 1, then the continuation is
  ;; re-invoked with 2 and then 3. Verified against Gambit, which also gives 13.
  (test "re-entrant capture accumulates correctly"
    13
    (let ((saved #f)
          (count 0))
      (let ((result (+ 10 (call/cc (lambda (k) (set! saved k) 1)))))
        (set! count (+ count 1))
        (if (< count 3)
            (saved (+ count 1))
            result))))

  ;; Capture in the operator position rather than an argument position.
  (test "capture in operator position"
    7
    (+ 1 (call/cc (lambda (k) (k 6)))))

  ;; Nested captures within a single argument list.
  (test "two captures in one argument list"
    30
    (+ (call/cc (lambda (k) (k 10)))
       (call/cc (lambda (k) (k 20)))))

  ;; Multi-shot: invoking the same continuation twice must produce consistent
  ;; results, not results contaminated by the first invocation.
  (test "same continuation invoked twice"
    '(5 5)
    (let ((k #f)
          (results '()))
      (let ((v (call/cc (lambda (c) (set! k c) 5))))
        (set! results (cons v results))
        (if (= (length results) 1)
            (k 5)
            results)))))

;; ===== The Transcendental Functions Across the Numeric Tower =====
;;
;; The functions of R7RS 6.2.6 that JavaScript's Math computes, given every
;; kind of real number. An exact argument is converted to the double nearest
;; it -- an exact integer is a BigInt, which Math refuses, so (exp 0) used to
;; raise "Cannot convert a BigInt value to a number" -- and the result is the
;; one the inexact argument gives. One far outside a double's range is not
;; converted: its logarithm, square root and powers are computed from its
;; exact value.

;; /**
;;  * Whether an inexact real is within a relative error of 1e-12 of the value
;;  * expected: for results computed from exact numbers too large or too small
;;  * for a double, which may differ from the value written in the last bits.
;;  * @param {real} expected - The value expected, not zero.
;;  * @param {real} actual - The value computed.
;;  * @returns {boolean}
;;  */
(define (close? expected actual)
  (and (inexact? actual)
       (< (abs (- actual expected)) (* 1e-12 (abs expected)))))

;; /**
;;  * Whether a complex number's parts are the two given.
;;  * @param {real} real - The real part expected.
;;  * @param {real} imag - The imaginary part expected.
;;  * @param {number} z - The number computed.
;;  * @returns {boolean}
;;  */
(define (parts? real imag z)
  (and (equal? real (real-part z)) (equal? imag (imag-part z))))

(define pi (acos -1.0))

(test-group "the transcendental functions across the numeric tower"

  (test-group "exp"
    (test "of exact 0" 1.0 (exp 0))
    (test "of exact 1" 2.718281828459045 (exp 1))
    (test "of an exact negative integer" (exp -1.0) (exp -1))
    (test "of an exact rational" (exp (inexact 1/3)) (exp 1/3))
    (test "of an exact integer too large for a double" +inf.0 (exp (expt 10 400)))
    (test "of an exact integer too small for a double" 0.0 (exp (- (expt 10 400)))))

  (test-group "log"
    (test "of exact 1" 0.0 (log 1))
    (test "of an exact integer" (log 100.0) (log 100))
    (test "of an exact rational" (log 0.5) (log 1/2))
    (test "of exact 0" -inf.0 (log 0))
    (test "of an exact integer too large for a double"
      #t (close? 921.0340371976183 (log (expt 10 400))))
    (test "of an exact rational too small for a double"
      #t (close? -921.0340371976183 (log (/ 1 (expt 10 400)))))
    (test "of an exact rational with both parts too large for a double"
      #t (close? (log 1.5) (log (/ (+ (* 3 (expt 10 400)) 1) (* 2 (expt 10 400))))))
    (test "of an exact rational too large for a double, with a large denominator"
      #t (close? (- 921.0340371976183 (log 3.0)) (log (/ (expt 10 800) (* 3 (expt 10 400))))))
    (test "of an exact integer just past the largest double"
      #t (close? (* 1024 (log 2.0)) (log (expt 2 1024))))
    ;; Not (log 3e-320): that double is subnormal, with a dozen bits left.
    (test "of an exact rational in a double's subnormal range"
      #t (close? (- (log 3.0) (* 320 (log 10.0))) (log (/ 3 (expt 10 320)))))
    (test "with a base: an exact power of two" (/ (log 8.0) (log 2.0)) (log 8 2))
    (test "with a base: exact 100 in base 10" 2.0 (log 100 10))
    (test "with a base: an exact rational" (/ (log 0.125) (log 2.0)) (log 1/8 2))
    (test "with a base: inexact arguments" (/ (log 1000.0) (log 10.0)) (log 1000.0 10.0))
    (test "with a base: an exact integer too large for a double"
      #t (close? 400.0 (log (expt 10 400) 10)))
    (test "with three arguments" 'error (guard (e (#t 'error)) (log 8 2 3))))

  (test-group "sin, cos and tan"
    (test "sin of exact 0" 0.0 (sin 0))
    (test "sin of an exact integer" (sin 1.0) (sin 1))
    (test "sin of an exact rational" (sin 0.5) (sin 1/2))
    (test "cos of exact 0" 1.0 (cos 0))
    (test "cos of an exact integer" (cos 1.0) (cos 1))
    (test "tan of exact 0" 0.0 (tan 0))
    (test "tan of an exact integer" (tan 1.0) (tan 1))
    (test "tan of an exact rational" (tan 0.5) (tan 1/2)))

  (test-group "asin, acos and atan"
    (test "asin of exact 0" 0.0 (asin 0))
    (test "asin of exact 1" (asin 1.0) (asin 1))
    (test "asin of an exact rational" (asin 0.5) (asin 1/2))
    (test "acos of exact 1" 0.0 (acos 1))
    (test "acos of an exact rational" (acos 0.5) (acos 1/2))
    (test "atan of an exact integer" (atan 1.0) (atan 1))
    (test "atan of an exact integer too large for a double" (atan +inf.0) (atan (expt 10 400)))
    (test "atan of two exact integers" (atan 2.0 3.0) (atan 2 3))
    (test "atan of two exact rationals" (atan 0.5 (inexact 1/3)) (atan 1/2 1/3))
    (test "atan of exact 0 and a negative exact integer" pi (atan 0 -1))
    (test "atan with three arguments" 'error (guard (e (#t 'error)) (atan 1 2 3))))

  (test-group "sqrt"
    (test "of exact 0" 0 (sqrt 0))
    (test "of an exact square" 3 (sqrt 9))
    (test "of an exact rational square" 2/3 (sqrt 4/9))
    (test "of an exact square too large for a double" (expt 10 200) (sqrt (expt 10 400)))
    (test "of an exact integer that is not a square" 1.4142135623730951 (sqrt 2))
    (test "of an exact rational that is not a square" (sqrt (inexact 2/3)) (sqrt 2/3))
    (test "of an inexact square stays inexact" 2.0 (sqrt 4.0))
    (test "of an exact integer just past a square, too large for a double"
      1e200 (sqrt (+ (expt 10 400) 1)))
    (test "of an exact integer too large for a double, an odd power of ten"
      #t (close? 3.1622776601683794e200 (sqrt (expt 10 401))))
    (test "of an exact rational too small for a double"
      #t (close? 3.1622776601683794e-201 (sqrt (/ 1 (expt 10 401)))))
    (test "of the square of an exact integer of hundreds of digits"
      (+ (expt 3 400) 7) (sqrt (square (+ (expt 3 400) 7))))))

;; ===== Real Arguments Whose Values Are Not Real =====
;;
;; R7RS 6.2.4 lets an implementation without complex numbers use a NaN for a
;; value like (sqrt -1.0) or (asin 2.0). This one has complex numbers, so the
;; values are the principal ones R7RS 6.2.6 defines: the square root with a
;; non-negative imaginary part, log z = log|z| + i angle(z), asin z = -i
;; log(iz + sqrt(1 - z^2)), acos z = pi/2 - asin z, and z1^z2 = e^(z2 log z1).

(test-group "real arguments whose values are not real"

  (test-group "sqrt of a negative number"
    (test "an exact square is exact" +2i (sqrt -4))
    (test "R7RS's example" +i (sqrt -1))
    (test "an exact rational square is exact" (make-rectangular 0 1/2) (sqrt -1/4))
    (test "is exact" #t (exact? (sqrt -4)))
    (test "an exact integer that is not a square" (make-rectangular 0.0 (sqrt 2.0)) (sqrt -2))
    (test "an inexact square stays inexact" (make-rectangular 0.0 2.0) (sqrt -4.0))
    (test "an inexact one is inexact" #f (exact? (sqrt -4.0)))
    (test "negative infinity" (make-rectangular 0.0 +inf.0) (sqrt -inf.0))
    (test "negative zero is its own root" -inf.0 (/ 1 (sqrt -0.0)))
    (test "an exact rational square too small for a double is exact"
      (make-rectangular 0 (/ 1 (expt 10 200))) (sqrt (- (/ 1 (expt 10 400)))))
    (test "an exact rational too small for a double"
      #t (let ((z (sqrt (- (/ 1 (expt 10 401))))))
           (and (= 0 (real-part z)) (close? 3.1622776601683794e-201 (imag-part z))))))

  (test-group "log of a negative number"
    (test "an exact integer" (make-rectangular 0.0 pi) (log -1))
    (test "an inexact one" (make-rectangular (log 2.0) pi) (log -2.0))
    (test "an exact rational" (make-rectangular (log 0.5) pi) (log -1/2))
    (test "negative zero, as R7RS 6.2.6 has it" (make-rectangular -inf.0 pi) (log -0.0))
    (test "an exact integer too large for a double"
      #t (let ((z (log (- (expt 10 400)))))
           (and (close? 921.0340371976183 (real-part z)) (= pi (imag-part z)))))
    (test "an exact rational too small for a double"
      #t (let ((z (log (- (/ 1 (expt 10 400))))))
           (and (close? -921.0340371976183 (real-part z)) (= pi (imag-part z)))))
    (test "with a base" (/ (make-rectangular (log 8.0) pi) (log 2.0)) (log -8 2)))

  (test-group "asin and acos beyond [-1, 1]"
    (test "asin of 2" #t (parts? (/ pi 2) -1.3169578969248166 (asin 2)))
    (test "asin of -2" #t (parts? (/ pi -2) 1.3169578969248166 (asin -2)))
    (test "asin of an inexact one" #t (parts? (/ pi 2) -1.3169578969248166 (asin 2.0)))
    (test "acos of 2" #t (parts? 0.0 1.3169578969248166 (acos 2)))
    (test "acos of -2" #t (parts? pi -1.3169578969248166 (acos -2)))
    (test "asin of 1 stays real" (/ pi 2) (asin 1))
    (test "acos of -1 stays real" pi (acos -1)))

  (test-group "expt of a negative base to a power that is not an integer"
    (test "the principal cube root of -8, as e^(log(-8)/3)"
      #t (parts? 1.0000000000000002 1.7320508075688772 (expt -8 1/3)))
    (test "an inexact base" #t (parts? 1.0000000000000002 1.7320508075688772 (expt -8.0 1/3)))
    (test "the square root of -1, as e^(log(-1)/2)"
      #t (parts? 6.123233995736766e-17 1.0 (expt -1 0.5)))
    (test "an integral inexact power stays real" -8.0 (expt -2 3.0))
    (test "a negative exact base too small for a double, to an odd inexact power, is -0.0"
      -inf.0 (/ 1 (expt (- (/ 1 (expt 10 400))) 3.0)))))

;; ===== expt Across the Numeric Tower =====

(test-group "expt across the numeric tower"
  (test "an exact rational to an exact integer power is exact" 1/4 (expt 1/2 2))
  (test "an exact rational to a negative exact integer power is exact" 9/4 (expt 2/3 -2))
  (test "a negative exact rational to an odd power" -8/27 (expt -2/3 3))
  (test "a negative exact rational to a negative odd power" -27/8 (expt -2/3 -3))
  (test "an exact rational to the power -1 is an exact integer" 2 (expt 1/2 -1))
  (test "an exact rational to the power 0" 1 (expt 1/2 0))
  (test "an exact integer to a negative exact integer power" 1/4 (expt 2 -2))
  (test "an exact integer to an inexact power" (sqrt 2.0) (expt 2 0.5))
  (test "an exact integer to an exact rational power" (expt 2.0 0.5) (expt 2 1/2))
  (test "an exact rational to an inexact power" (expt 0.5 0.5) (expt 1/2 0.5))
  (test "an inexact base to an exact integer power" 6.25 (expt 2.5 2))
  (test "an exact integer too large for a double, to an inexact power"
    #t (close? 1e200 (expt (expt 10 400) 0.5)))
  (test "an exact rational too small for a double, to a negative inexact power"
    #t (close? 1e200 (expt (/ 1 (expt 10 400)) -0.5))))

;; ===== Exact Numbers Converted to Inexact =====
;;
;; An exact rational's nearest double, which the functions above are given.
;; Converting its numerator and denominator first and dividing rounds twice,
;; and for parts past 2^1024 divides infinity by infinity.

(test-group "exact rationals converted to inexact"
  (test "both parts too large for a double" 10.0 (inexact (/ (+ (expt 10 400) 1) (expt 10 399))))
  (test "both parts too large, a value near 1/3"
    (inexact 1/3) (inexact (/ (expt 10 400) (+ (* 3 (expt 10 400)) 1))))
  (test "too large for a double" +inf.0 (inexact (/ (expt 10 400) 3)))
  (test "too large for a double, negative" -inf.0 (inexact (/ (- (expt 10 400)) 3)))
  (test "too small for a double" 0.0 (inexact (/ 1 (expt 10 400))))
  (test "the smallest subnormal" 5e-324 (inexact (/ 1 (expt 2 1074))))
  (test "half way between two subnormals, rounded to even" 1e-323 (inexact (/ 3 (expt 2 1075))))
  (test "a numerator past 2^53, rounded once"
    3002399751580331.5 (inexact (/ (+ (expt 2 53) 3) 3)))
  (test "a denominator past 2^53, rounded once"
    (/ 1.0 3002399751580331.5) (inexact (/ 3 (+ (expt 2 53) 3)))))

;; ===== Integer Division With Inexact Arguments =====
;;
;; R7RS 6.2.6: (truncate/ -5.0 -2) => 2.0 -1.0. An inexact argument makes the
;; results inexact; quotient, remainder and modulo are truncate-quotient,
;; truncate-remainder and floor-remainder.

(test-group "integer division with inexact arguments"
  (test "quotient of an inexact dividend" 3.0 (quotient 17. 5))
  (test "quotient of an inexact divisor" 3.0 (quotient 17 5.))
  (test "quotient of exact integers stays exact" 3 (quotient 17 5))
  (test "remainder of an inexact divisor, Chibi's test" -1.0 (remainder -13 -4.0))
  (test "remainder of an inexact dividend" 2.0 (remainder 17. 5))
  (test "modulo of an inexact dividend" 1.0 (modulo -7. 2))
  (test "truncate/, R7RS's example" '(2.0 -1.0) (call-with-values (lambda () (truncate/ -5.0 -2)) list))
  (test "floor/ of an inexact dividend" '(-3.0 1.0) (call-with-values (lambda () (floor/ -5.0 2)) list))
  (test "floor-quotient of an inexact divisor" -3.0 (floor-quotient 5 -2.0))
  (test "floor-remainder of an inexact divisor" -1.0 (floor-remainder 5 -2.0))
  (test "truncate-quotient of an inexact dividend" -2.0 (truncate-quotient -5.0 2))
  (test "truncate-remainder of an inexact dividend" -1.0 (truncate-remainder -5.0 2))
  (test "an inexact integer past 2^53" 1e300 (* 7 (quotient 7e300 49)))
  (test "lcm of an inexact argument, R7RS's example" 288.0 (lcm 32.0 -36))
  (test "quotient by exact zero" 'error (guard (e (#t 'error)) (quotient 1 0)))
  (test "remainder by inexact zero" 'error (guard (e (#t 'error)) (remainder 1.0 0.0)))
  (test "modulo by exact zero, the message names modulo"
    "modulo: division by zero"
    (guard (e ((error-object? e) (error-object-message e))) (modulo 1 0))))

;; ===== round =====

(test-group "round to even"
  (test "R7RS's example: 3.5 rounds up" 4.0 (round 3.5))
  (test "2.5 rounds down, to even" 2.0 (round 2.5))
  (test "0.5 rounds to zero" 0.0 (round 0.5))
  (test "1.5 rounds up, to even" 2.0 (round 1.5))
  (test "-2.5 rounds up, to even" -2.0 (round -2.5))
  (test "-3.5 rounds down, to even" -4.0 (round -3.5))
  (test "-0.5 rounds to negative zero" -inf.0 (/ 1 (round -0.5)))
  (test "a value that is not half way" 3.0 (round 2.6))
  (test "an integral value" 7.0 (round 7.0)))

;; ===== square =====

(test-group "square across the numeric tower"
  (test "of an exact rational is exact" 1/4 (square 1/2))
  (test "of an exact complex number" -7+24i (square 3+4i))
  (test "of an inexact complex number" (* 1.5+2.0i 1.5+2.0i) (square 1.5+2.0i))
  (test "of an exact integer" 1764 (square 42))
  (test "of an inexact real" 4.0 (square 2.0)))

;; ===== Complex Arguments to the Functions of Real Numbers =====
;;
;; Not supported yet, beyond a complex number whose imaginary part is zero,
;; which is the real number it equals.

(test-group "complex arguments to the transcendental functions"
  (test "a zero imaginary part: exp" 1.0 (exp (make-rectangular 0 0)))
  (test "a zero imaginary part: sqrt of a negative" +2i (sqrt (make-rectangular -4 0)))
  (test "a zero imaginary part: expt" 1/4 (expt (make-rectangular 1/2 0) 2))
  (test "a non-real argument is an error that says so"
    "exp: complex not fully supported"
    (guard (e ((error-object? e) (error-object-message e))) (exp +i)))
  (test "asin of a non-real argument is an error, not a NaN"
    'error (guard (e (#t 'error)) (asin +i)))
  (test "a zero imaginary part: atan of two arguments"
    (atan 1.0 1.0) (atan (make-rectangular 1 0) 1))
  (test "atan of two arguments requires reals"
    'error (guard (e (#t 'error)) (atan +i 1))))

;; ===== exact and inexact of Complex Numbers =====
;;
;; R7RS 6.2.6: inexact gives the inexact number nearest its argument, and exact
;; the exact one, a complex number's parts each converted. inexact gave an exact
;; complex number back unchanged, and exact refused every complex number.

(test-group "exact and inexact of complex numbers"
  (test "inexact of an exact complex number is inexact" #f (exact? (inexact +2i)))
  (test "inexact of an exact complex number" (make-rectangular 1.0 2.0) (inexact 1+2i))
  (test "inexact of exact rational parts"
    (make-rectangular 0.5 -0.25) (inexact (make-rectangular 1/2 -1/4)))
  (test "inexact of an inexact complex number" (make-rectangular 1.5 2.5) (inexact 1.5+2.5i))
  (test "exact of an inexact complex number is exact" #t (exact? (exact 1.0+2.0i)))
  (test "exact of an inexact complex number" 1+2i (exact 1.0+2.0i))
  (test "exact of fractional parts" (make-rectangular 1/2 -1/4) (exact 0.5-0.25i))
  (test "exact of an imaginary number" +i (exact (make-rectangular 0.0 1.0)))
  (test "exact of a zero imaginary part is the real number" 3 (exact (make-rectangular 3.0 0.0)))
  (test "inexact->exact of an inexact complex number" 1+2i (inexact->exact 1.0+2.0i))
  (test "exact, then inexact, gives the number back" 0.1+0.2i (inexact (exact 0.1+0.2i)))
  (test "exact of an infinite part is an error"
    'error (guard (e (#t 'error)) (exact (make-rectangular +inf.0 1.0))))
  (test "exact of a NaN part is an error"
    'error (guard (e (#t 'error)) (exact (make-rectangular 1.0 +nan.0)))))

;; ===== exact-integer-sqrt's Argument =====
;;
;; R7RS 6.2.6 takes an exact non-negative integer: an inexact one was accepted,
;; and its root returned exact.

;; /**
;;  * The message of the error a thunk raises, or #f if it raises none.
;;  * @param {procedure} thunk - The thunk.
;;  * @returns {string|boolean}
;;  */
(define (error-message thunk)
  (guard (e ((error-object? e) (error-object-message e)))
    (thunk)
    #f))

(test-group "exact-integer-sqrt's argument"
  (test "an exact integer" '(4 1) (call-with-values (lambda () (exact-integer-sqrt 17)) list))
  (test "an inexact integer is an error"
    "exact-integer-sqrt: expected non-negative exact integer at argument 1, got number"
    (error-message (lambda () (exact-integer-sqrt 4.0))))
  (test "an exact rational is an error"
    'error (guard (e (#t 'error)) (exact-integer-sqrt 1/4)))
  (test "a negative exact integer is an error naming the procedure"
    "exact-integer-sqrt: expected non-negative exact integer"
    (let ((message (error-message (lambda () (exact-integer-sqrt -4)))))
      (and message (>= (string-length message) 55) (substring message 0 55))))
  (test "a negative exact integer is the error's irritant"
    '(-4)
    (guard (e ((error-object? e) (error-object-irritants e))) (exact-integer-sqrt -4))))

;; An exact integer is a JavaScript number or a BigInt, and an inexact real a
;; number or, where its value is an integer, a box
;; (src/core/interpreter/number_representation.js). The commonest primitives
;; take each directly ("Numbers and boxes taken directly" in
;; src/core/primitives/math.js); every answer here, and its exactness, is the
;; numeric tower's.
(test-group "the primitives on every way a real is held"
  (define big (expt 2 70))
  (define (both x) (list x (exact? x)))
  (define predicates (list number? real? rational? integer? exact? inexact? nan? finite? infinite?))
  (test "inexact" '((3. #f) (2.5 #f) (3. #f)) (map (lambda (x) (both (inexact x))) '(3 2.5 3.)))
  (test "exact" '((3 #t) (0 #t) (3 #t)) (map (lambda (x) (both (exact x))) '(3. -0. 3)))
  (test "exact of a fraction is a rational" 5/2 (exact 2.5))
  (test "abs" '((3 #t) (2.5 #f) (3. #f) (0. #f)) (map (lambda (x) (both (abs x))) '(-3 -2.5 -3. -0.)))
  (test "abs of a BigInt" big (abs (- big)))
  (test "magnitude" '((3 #t) (3. #f) (5. #f))
        (map (lambda (x) (both (magnitude x))) (list -3 -3. (make-rectangular 3. 4.))))
  (test "real-part and imag-part of an exact integer" '((3 #t) (0 #t))
        (list (both (real-part 3)) (both (imag-part 3))))
  (test "of an inexact integer" '((3. #f) (0. #f)) (list (both (real-part 3.)) (both (imag-part 3.))))
  (test "of a complex number" '((1. #f) (2. #f))
        (let ((z (make-rectangular 1. 2.))) (list (both (real-part z)) (both (imag-part z)))))
  (test "floor, ceiling, truncate and round of a fraction are inexact integers"
        '((-3. #f) (-2. #f) (-2. #f) (-2. #f))
        (map (lambda (f) (both (f -2.5))) (list floor ceiling truncate round)))
  (test "of an exact integer, itself" '(7 7 7 7) (map (lambda (f) (f 7)) (list floor ceiling truncate round)))
  (test "ceiling of a negative fraction is -0.0" "-0.0" (number->string (ceiling -.5)))
  (test "round takes a half to the even integer" '(4. 2.) (list (round 3.5) (round 2.5)))
  (test "square" '((9 #t) (6.25 #f) (9. #f)) (map (lambda (x) (both (square x))) '(3 2.5 3.)))
  (test "square past 2^53 is exact" 1152921504606846976 (square 1073741824))
  (test "sqrt of an inexact real" '((2. #f) (1.5 #f)) (map (lambda (x) (both (sqrt x))) '(4. 2.25)))
  (test "sqrt of -0.0" "-0.0" (number->string (sqrt -0.)))
  (test "sqrt of an exact square stays exact" '(3 #t) (both (sqrt 9)))
  (test "sqrt of a negative inexact real is not real" #f (real? (sqrt -4.)))
  (test "exp, sin, cos and atan of an exact integer are inexact" '(#f #f #f #f)
        (map (lambda (f) (exact? (f 0))) (list exp sin cos atan)))
  (test "exp of 0" 1. (exp 0))
  (test "atan of two exact integers" (atan 1. 1.) (atan 1 1))
  (test "log of an inexact real" 0. (log 1.))
  (test "asin beyond 1 is not real" #f (real? (asin 2.)))
  (test "division with an inexact real" '((1.5 #f) (2. #f) (+inf.0 #f))
        (list (both (/ 3. 2)) (both (/ 3 1.5)) (both (/ 1.5 0))))
  (test "division of exact integers stays exact" 3/2 (/ 3 2))
  (test "the predicates of an exact integer" '(#t #t #t #t #t #f #f #t #f)
        (map (lambda (p) (p 3)) predicates))
  (test "of a BigInt" '(#t #t #t #t #t #f #f #t #f) (map (lambda (p) (p big)) predicates))
  (test "of an inexact integer" '(#t #t #t #t #f #t #f #t #f) (map (lambda (p) (p 3.)) predicates))
  (test "of a fraction" '(#t #t #t #f #f #t #f #t #f) (map (lambda (p) (p 2.5)) predicates))
  (test "of +inf.0" '(#t #t #f #f #f #t #f #f #t) (map (lambda (p) (p +inf.0)) predicates))
  (test "of +nan.0" '(#t #t #f #f #f #t #t #f #f) (map (lambda (p) (p +nan.0)) predicates))
  (test "exact-integer?" '(#t #t #f #f) (map exact-integer? (list 3 big 3. 2.5)))
  (test "number? of something that is not a number" #f (number? 'a))
  (test-error "abs of something that is not a number is the primitive's error" "number" (abs 'a)))

;; ===== Argument Counts =====
;;
;; R7RS gives each numeric procedure its number of arguments -- one, or two,
;; or for atan and log one or two, for - and / at least one, and for the
;; comparisons at least two -- and a call with any other number is an error.
;; A call with too many returned normally, its extra argument ignored, and one
;; with too few raised a type error about the missing argument or, from the
;; predicates, returned #f.
;;
;; The calls with too many give a first argument that a primitive's direct path
;; takes ("Numbers and boxes taken directly" in src/core/primitives/math.js) --
;; an exact integer, or for sqrt and log an inexact real -- so that the direct
;; path's own test of the count is what is tested.

;; /**
;;  * Whether an error's message is the arity error of the procedure a call
;;  * names: the procedure's name, then "wrong number of arguments".
;;  * @param {list} call - The call, as a datum.
;;  * @param {string|boolean} message - The message, or #f for no error.
;;  * @returns {boolean}
;;  */
(define (arity-error-message? call message)
  (let ((prefix (string-append (symbol->string (car call)) ": wrong number of arguments")))
    (and (string? message)
         (>= (string-length message) (string-length prefix))
         (string=? (substring message 0 (string-length prefix)) prefix))))

;; /**
;;  * The calls whose errors are not their procedures' arity errors.
;;  * @param {list} results - Pairs of a call, as a datum, and the message of
;;  *   its error, or #f for none.
;;  * @returns {list} The pairs whose message is not an arity error's.
;;  */
(define (calls-without-arity-errors results)
  (cond ((null? results) '())
        ((arity-error-message? (caar results) (cdar results))
         (calls-without-arity-errors (cdr results)))
        (else (cons (car results) (calls-without-arity-errors (cdr results))))))

;; /**
;;  * A test group of calls that must each raise an arity error: a `test-error`
;;  * for each call, and a test that every error is the procedure's arity error
;;  * -- `test-error` passes on any error, and a missing argument also raises a
;;  * type error.
;;  * @param {string} name - The group's name.
;;  * @param {...*} call - The calls.
;;  */
(define-syntax test-arity-errors
  (syntax-rules ()
    ((_ name call ...)
     (test-group name
       (test-error 'call "wrong number of arguments" call) ...
       (test "each error is the procedure's arity error"
         '()
         (calls-without-arity-errors
          (list (cons 'call (error-message (lambda () call))) ...)))))))

(test-arity-errors "one argument too many"
  (number? 1 2)
  (complex? 1 2)
  (real? 1 2)
  (rational? 1 2)
  (integer? 1 2)
  (exact-integer? 1 2)
  (exact? 1 2)
  (inexact? 1.0 2)
  (finite? 1 2)
  (infinite? 1 2)
  (nan? 1 2)
  (numerator 1 2)
  (denominator 1 2)
  (real-part 1 2)
  (imag-part 1 2)
  (magnitude 1 2)
  (angle 1 2)
  (make-rectangular 1 2 3)
  (make-polar 1 2 3)
  (abs 1 2)
  (floor 1 2)
  (ceiling 1 2)
  (truncate 1 2)
  (round 1 2)
  (sqrt 2.25 2)
  (exact-integer-sqrt 4 2)
  (expt 2 3 4)
  (exp 0 2)
  (log 2.5 2 3)
  (sin 0 2)
  (cos 0 2)
  (tan 0 2)
  (asin 0 2)
  (acos 0 2)
  (atan 0 1 2)
  (square 1 2)
  (inexact 1 2)
  (exact 1 2)
  (quotient 7 2 3)
  (remainder 7 2 3)
  (modulo 7 2 3)
  (floor/ 7 2 3)
  (floor-quotient 7 2 3)
  (floor-remainder 7 2 3)
  (truncate/ 7 2 3)
  (truncate-quotient 7 2 3)
  (truncate-remainder 7 2 3))

(test-arity-errors "one argument too few"
  (number?)
  (complex?)
  (real?)
  (rational?)
  (integer?)
  (exact-integer?)
  (exact?)
  (inexact?)
  (finite?)
  (infinite?)
  (nan?)
  (numerator)
  (denominator)
  (real-part)
  (imag-part)
  (magnitude)
  (angle)
  (make-rectangular 1)
  (make-polar 1)
  (abs)
  (floor)
  (ceiling)
  (truncate)
  (round)
  (sqrt)
  (exact-integer-sqrt)
  (expt 2)
  (exp)
  (log)
  (sin)
  (cos)
  (tan)
  (asin)
  (acos)
  (atan)
  (square)
  (inexact)
  (exact)
  (quotient 7)
  (remainder 7)
  (modulo 7)
  (floor/ 7)
  (floor-quotient 7)
  (floor-remainder 7)
  (truncate/ 7)
  (truncate-quotient 7)
  (truncate-remainder 7)
  (-)
  (/)
  (= 1)
  (< 1)
  (> 1)
  (<= 1)
  (>= 1))
