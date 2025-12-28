;; Numeric Procedures
;; Comparison operators, predicates, and mathematical functions

;; =============================================================================
;; Variadic Comparison Operators
;; =============================================================================
;; These build on the binary primitives (%num=, %num<, etc.) from JavaScript.

;; /**
;;  * Numeric equality. Returns #t if all arguments are equal.
;;
;;  * @param {number} x - First number.
;;  * @param {number} y - Second number.
;;  * @param {...number} rest - Additional numbers.
;;  * @returns {boolean} #t if all are equal.
;;  */
(define (= x y . rest)
  (if (not (%num= x y))
      #f
      (if (null? rest)
          #t
          (apply = y rest))))

;; /**
;;  * Less than. Returns #t if arguments are strictly increasing.
;;
;;  * @param {number} x - First number.
;;  * @param {number} y - Second number.
;;  * @param {...number} rest - Additional numbers.
;;  * @returns {boolean} #t if strictly increasing.
;;  */
(define (< x y . rest)
  (if (not (%num< x y))
      #f
      (if (null? rest)
          #t
          (apply < y rest))))

;; /**
;;  * Greater than. Returns #t if arguments are strictly decreasing.
;;
;;  * @param {number} x - First number.
;;  * @param {number} y - Second number.
;;  * @param {...number} rest - Additional numbers.
;;  * @returns {boolean} #t if strictly decreasing.
;;  */
(define (> x y . rest)
  (if (not (%num> x y))
      #f
      (if (null? rest)
          #t
          (apply > y rest))))

;; /**
;;  * Less than or equal. Returns #t if arguments are non-decreasing.
;;
;;  * @param {number} x - First number.
;;  * @param {number} y - Second number.
;;  * @param {...number} rest - Additional numbers.
;;  * @returns {boolean} #t if non-decreasing.
;;  */
(define (<= x y . rest)
  (if (not (%num<= x y))
      #f
      (if (null? rest)
          #t
          (apply <= y rest))))

;; /**
;;  * Greater than or equal. Returns #t if arguments are non-increasing.
;;
;;  * @param {number} x - First number.
;;  * @param {number} y - Second number.
;;  * @param {...number} rest - Additional numbers.
;;  * @returns {boolean} #t if non-increasing.
;;  */
(define (>= x y . rest)
  (if (not (%num>= x y))
      #f
      (if (null? rest)
          #t
          (apply >= y rest))))

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
(define (max x . rest)
  (if (not (number? x))
      (error "max: expected number" x))
  (if (null? rest)
      x
      (let ((next (car rest)))
        (if (not (number? next))
            (error "max: expected number" next))
        (apply max (if (> x next) x next) (cdr rest)))))

;; /**
;;  * Minimum. Returns the smallest of its arguments.
;;
;;  * @param {number} x - First number.
;;  * @param {...number} rest - Additional numbers.
;;  * @returns {number} Minimum value.
;;  */
(define (min x . rest)
  (if (not (number? x))
      (error "min: expected number" x))
  (if (null? rest)
      x
      (let ((next (car rest)))
        (if (not (number? next))
            (error "min: expected number" next))
        (apply min (if (< x next) x next) (cdr rest)))))

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

;; =============================================================================
;; Rounding (R7RS banker's rounding)
;; =============================================================================

;; /**
;;  * Round to nearest integer (R7RS: round half to even).
;;
;;  * @param {number} x - Number to round.
;;  * @returns {number} Rounded value.
;;  */
(define (round x)
  (if (not (number? x))
      (error "round: expected number" x))
  ;; Convert rational to inexact for arithmetic operations
  (let ((x (if (rational? x) (inexact x) x)))
    (let* ((fl (floor x))
           (frac (- x fl)))
      (cond
        ((< frac 0.5) fl)
        ((> frac 0.5) (+ fl 1))
        ;; frac = 0.5, round to even
        ((even? (inexact->exact fl)) fl)
        (else (+ fl 1))))))

;; Helper for round: convert inexact to exact (truncate to integer)
(define (inexact->exact x)
  (truncate x))
