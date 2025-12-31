;; R7RS Compliance Tests - Section 6.2: Numbers (subset)
;; Adapted from Chibi Scheme's R7RS test suite by Alex Shinn
;; Note: Full numeric tower tests require complex/rational support

(test-group "6.2 Numbers"

  (test #t (complex? 3))
  (test #t (real? 3))
  (test #t (rational? 6/3))
  (test #t (integer? 3.0))
  (test #t (integer? 8/4))

  (test #t (exact-integer? 32))
  ;; 32.0 is a Number (inexact), so not an exact-integer
  (test #f (exact-integer? 32.0))
  (test #f (exact-integer? 32/5))

  (test #t (finite? 3))

  (test #t (= 1 1.0))
  (test #t (< 1 2 3))
  (test #f (< 1 1 2))
  (test #t (> 3.0 2.0 1.0))
  (test #f (> -3.0 2.0 1.0))
  (test #t (<= 1 1 2))
  (test #f (<= 1 2 1))
  (test #t (>= 2 1 1))
  (test #f (>= 1 2 1))
  (test '(#t #f) (list (<= 1 1 2) (<= 2 1 3)))

  (test #t (zero? 0))
  (test #t (zero? 0.0))
  (test #f (zero? 1))
  (test #f (zero? -1))

  (test #f (positive? 0))
  (test #f (positive? 0.0))
  (test #t (positive? 1))
  (test #t (positive? 1.0))
  (test #f (positive? -1))
  (test #f (positive? -1.0))

  (test #f (negative? 0))
  (test #f (negative? 0.0))
  (test #f (negative? 1))
  (test #f (negative? 1.0))
  (test #t (negative? -1))
  (test #t (negative? -1.0))

  (test #f (odd? 0))
  (test #t (odd? 1))
  (test #t (odd? -1))
  (test #f (odd? 102))

  (test #t (even? 0))
  (test #f (even? 1))
  (test #t (even? -2))
  (test #t (even? 102))

  (test 3 (max 3))
  (test 4 (max 3 4))
  (test 4.0 (max 3.9 4))
  (test 5.0 (max 5 3.9 4))
  (test 3 (min 3))
  (test 3 (min 3 4))
  (test 3.0 (min 3 3.1))

  (test 7 (+ 3 4))
  (test 3 (+ 3))
  (test 0 (+))
  (test 4 (* 4))
  (test 1 (*))

  (test -1 (- 3 4))
  (test -6 (- 3 4 5))
  (test -3 (- 3))

  (test 7 (abs -7))
  (test 7 (abs 7))

  (test 1 (modulo 13 4))
  (test 1 (remainder 13 4))

  (test 3 (modulo -13 4))
  (test -1 (remainder -13 4))

  (test -3 (modulo 13 -4))
  (test 1 (remainder 13 -4))

  (test -1 (modulo -13 -4))
  (test -1 (remainder -13 -4))

  (test 4 (gcd 32 -36))
  (test 0 (gcd))
  (test 288 (lcm 32 -36))
  (test 1 (lcm))

  (test -5.0 (floor -4.3))
  (test -4.0 (ceiling -4.3))
  (test -4.0 (truncate -4.3))
  (test -4.0 (round -4.3))

  (test 3.0 (floor 3.5))
  (test 4.0 (ceiling 3.5))
  (test 3.0 (truncate 3.5))
  (test 4.0 (round 3.5))

  (test 4 (round 7/2))
  (test 7 (round 7))

  (test 1764 (square 42))
  (test 4 (square 2))

  (test 3.0 (inexact (sqrt 9)))
  (test-skip "JS limitation: precision difference"
    (test 1.4142135623731 (sqrt 2)))

  (test (list 2 0) (call-with-values (lambda () (exact-integer-sqrt 4)) list))
  (test (list 2 1) (call-with-values (lambda () (exact-integer-sqrt 5)) list))

  (test 27 (expt 3 3))
  (test 1 (expt 0 0))
  (test 0 (expt 0 1))
  (test 1 (expt 0 0))
  (test 0 (expt 0 1))

  (test 1 (inexact 1))
  ;; This now works: BigInt is exact, Number is inexact
  (test #t (inexact? (inexact 1)))
  (test 1 (exact 1))
  (test #t (exact? (exact 1)))

  (test 100 (string->number "100"))
  (test 256 (string->number "100" 16))
  (test 100.0 (string->number "1e2"))
  (test #f (string->number "1 2"))
)
