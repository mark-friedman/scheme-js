;; Numeric Procedures
;; Comparison operators, predicates, and mathematical functions

;; =============================================================================
;; Variadic Comparison Operators
;; =============================================================================
;; `=`, `<`, `>`, `<=` and `>=` are now native primitives (see
;; `src/core/primitives/math.js`). They were previously defined here as variadic
;; Scheme procedures with rest parameters delegating to the `%num*` binary
;; primitives, which turned a single integer comparison into four nested
;; applications plus rest-list construction. Profiling attributed roughly half
;; the runtime of the `fib` benchmark to that expansion, and moving `<` alone to
;; a primitive was measured at 1.93x.
;;
;; This is one of the few places where the project's "Scheme over JS" rule is
;; deliberately overridden: these five procedures sit on the hot path of every
;; numeric program, and the Scheme definitions were also incorrect for
;; rationals, since they bottomed out in JavaScript's `<` and `===` applied
;; directly to `Rational` objects.

;; =============================================================================
;; Numeric Predicates
;; =============================================================================

;; /**
;;  * Zero predicate.
;;
;;  * @param {number} x - Number to check.
;;  * @returns {boolean} #t if x is zero.
;;  */
(define (zero? x)
  (if (not (number? x))
      (error "zero?: expected number" x))
  (= x 0))

;; /**
;;  * Positive predicate.
;;
;;  * @param {number} x - Number to check.
;;  * @returns {boolean} #t if x is positive.
;;  */
(define (positive? x)
  (if (not (number? x))
      (error "positive?: expected number" x))
  (> x 0))

;; /**
;;  * Negative predicate.
;;
;;  * @param {number} x - Number to check.
;;  * @returns {boolean} #t if x is negative.
;;  */
(define (negative? x)
  (if (not (number? x))
      (error "negative?: expected number" x))
  (< x 0))

;; /**
;;  * Odd predicate.
;;
;;  * @param {number} x - Integer to check.
;;  * @returns {boolean} #t if x is odd.
;;  */
(define (odd? x)
  (if (not (integer? x))
      (error "odd?: expected integer" x))
  (not (= (modulo x 2) 0)))

;; /**
;;  * Even predicate.
;;
;;  * @param {number} x - Integer to check.
;;  * @returns {boolean} #t if x is even.
;;  */
(define (even? x)
  (if (not (integer? x))
      (error "even?: expected integer" x))
  (= (modulo x 2) 0))

;; =============================================================================
;; Min/Max
;; =============================================================================

;; /**
;;  * Maximum. Returns the largest of its arguments.
;;
;;  * @param {number} x - First number.
;;  * @param {...number} rest - Additional numbers.
;;  * @returns {number} Maximum value.
;;  */
(define (m-max x . rest)
  (if (null? rest)
      x
      (let ((res (apply m-max rest)))
        (let ((m (if (> x res) x res)))
          ;; R7RS: If any argument is inexact, result is inexact
          (if (or (inexact? x) (inexact? res))
              (inexact m)
              m)))))

(define (max x . rest)
  (if (not (number? x))
      (error "max: expected number" x))
  (apply m-max x rest))

;; /**
;;  * Minimum. Returns the smallest of its arguments.
;;
;;  * @param {number} x - First number.
;;  * @param {...number} rest - Additional numbers.
;;  * @returns {number} Minimum value.
;;  */
(define (m-min x . rest)
  (if (null? rest)
      x
      (let ((res (apply m-min rest)))
        (let ((m (if (< x res) x res)))
          ;; R7RS: If any argument is inexact, result is inexact
          (if (or (inexact? x) (inexact? res))
              (inexact m)
              m)))))

(define (min x . rest)
  (if (not (number? x))
      (error "min: expected number" x))
  (apply m-min x rest))

;; =============================================================================
;; GCD/LCM
;; =============================================================================

;; /**
;;  * Greatest common divisor (binary helper).
;;
;;  * @param {integer} a - First integer.
;;  * @param {integer} b - Second integer.
;;  * @returns {integer} GCD of a and b.
;;  */
(define (%gcd2 a b)
  (let ((aa (abs a))
        (bb (abs b)))
    (if (= bb 0)
        aa
        (%gcd2 bb (modulo aa bb)))))

;; /**
;;  * Greatest common divisor.
;;
;;  * @param {...integer} args - Integers.
;;  * @returns {integer} GCD of all arguments, or 0 if no arguments.
;;  */
(define (gcd . args)
  (for-each (lambda (x)
              (if (not (integer? x))
                  (error "gcd: expected integer" x)))
            args)
  (if (null? args)
      0
      (let loop ((result (abs (car args)))
                 (rest (cdr args)))
        (if (null? rest)
            result
            (loop (%gcd2 result (car rest)) (cdr rest))))))

;; /**
;;  * Least common multiple.
;;
;;  * @param {...integer} args - Integers.
;;  * @returns {integer} LCM of all arguments, or 1 if no arguments.
;;  */
(define (lcm . args)
  (for-each (lambda (x)
              (if (not (integer? x))
                  (error "lcm: expected integer" x)))
            args)
  (if (null? args)
      1
      (let loop ((result (abs (car args)))
                 (rest (cdr args)))
        (if (null? rest)
            result
            (let ((b (abs (car rest))))
              (if (or (= result 0) (= b 0))
                  0
                  (loop (quotient (* result b) (%gcd2 result b))
                        (cdr rest))))))))

;; /**
;;  * The simplest rational in a closed interval of positive exact rationals:
;;  * the one with the smallest denominator, and of those the smallest
;;  * numerator (R7RS 6.2.6). An integer in the interval is the smallest one;
;;  * otherwise the interval's whole part, and the simplest rational of the
;;  * interval its fractional parts' reciprocals make -- a continued fraction.
;;  * @param {rational} lo - The lower bound, above zero.
;;  * @param {rational} hi - The upper bound, at least `lo`.
;;  * @returns {rational}
;;  */
(define (%simplest-positive lo hi)
  (let ((whole (floor lo)))
    (cond ((= whole lo) whole)
          ((< whole (floor hi)) (+ whole 1))
          (else (+ whole (/ (%simplest-positive (/ (- hi whole)) (/ (- lo whole)))))))))

;; /**
;;  * The simplest rational in a closed interval of exact rationals: zero if
;;  * the interval holds it, and otherwise the simplest of its positive
;;  * mirror, mirrored back.
;;  * @param {rational} lo - The lower bound.
;;  * @param {rational} hi - The upper bound, at least `lo`.
;;  * @returns {rational}
;;  */
(define (%simplest lo hi)
  (cond ((positive? lo) (%simplest-positive lo hi))
        ((negative? hi) (- (%simplest-positive (- hi) (- lo))))
        (else 0)))

;; /**
;;  * The simplest rational number differing from x by no more than y
;;  * (R7RS 6.2.6), exact when both are: `(rationalize (exact .3) 1/10)` is
;;  * 1/3, `(rationalize .3 1/10)` is #i1/3. It is found exactly, from the
;;  * exact values of an inexact x and y, and made inexact last. An infinite x
;;  * is its own answer; an infinite y makes any finite x zero.
;;  * @param {real} x - The number.
;;  * @param {real} y - The tolerance, whose sign does not matter.
;;  * @returns {real}
;;  */
(define (rationalize x y)
  (if (not (real? x)) (error "rationalize: expected real number" x))
  (if (not (real? y)) (error "rationalize: expected real number" y))
  (cond ((or (nan? x) (nan? y)) +nan.0)
        ((infinite? y) (if (infinite? x) +nan.0 0.0))
        ((infinite? x) x)
        (else
          (let* ((center (exact x))
                 (radius (abs (exact y)))
                 (simplest (%simplest (- center radius) (+ center radius))))
            (if (and (exact? x) (exact? y)) simplest (inexact simplest))))))


