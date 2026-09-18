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


