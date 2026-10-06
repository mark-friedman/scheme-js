;; complex_functions_tests.scm -- the elementary functions of complex numbers,
;; and the predicates and procedures on numbers that complex numbers reach.
;;
;; R7RS 6.2.6 defines exp, log, sqrt, the trigonometric functions and their
;; inverses, and expt on every complex number, by formulas from exp and log
;; whose branch cuts it fixes: log z's imaginary part is in (-pi, pi], -pi
;; itself below the negative reals where -0.0 is distinguished, and sqrt z
;; has a positive real part, or a zero one and an imaginary part that is not
;; negative. A complex argument's value is computed on its parts as doubles
;; (src/core/primitives/math.js, "Functions of complex numbers"), so it is
;; compared here within a few units in the last place: the formulas compose
;; several of JavaScript's Math functions, whose last bits are not fixed. The
;; expected values are C99's (Python's cmath), which agrees with R7RS away
;; from the cuts.

;; /**
;;  * Whether a computed number is within a few units in the last place of an
;;  * expected one, part by part, and as exact as it.
;;  * @param {number} expected - The expected number.
;;  * @param {*} actual - The computed value.
;;  * @returns {boolean}
;;  */
(define (close? expected actual)
  (define (part-close? e a)
    (or (= e a) (<= (abs (- e a)) (* 1e-13 (max 1. (abs e))))))
  (and (number? actual)
       (eq? (exact? expected) (exact? actual))
       (part-close? (real-part expected) (real-part actual))
       (part-close? (imag-part expected) (imag-part actual))))

(test-group "the elementary functions of a complex argument"
  (test "exp" #t (close? (make-rectangular -1.1312043837568135 2.4717266720048188) (exp 1.0+2.0i)))
  (test "exp of an exact complex number is inexact" #t
        (close? (make-rectangular 0.5403023058681398 0.8414709848078965) (exp +i)))
  (test "log" #t (close? (make-rectangular 1.6094379124341003 0.9272952180016122) (log 3.0+4.0i)))
  (test "log of +i" #t (close? (make-rectangular 0. 1.5707963267948966) (log +i)))
  (test "log's imaginary part is pi above the negative reals" #t
        (close? (make-rectangular 0. 3.141592653589793) (log -1.0+0.0i)))
  (test "and -pi below them, an imaginary part of -0.0" #t
        (close? (make-rectangular 0. -3.141592653589793) (log -1.0-0.0i)))
  (test "log in a base" #t (close? (make-rectangular 3. 4.532360141827194) (log -8 2)))
  (test "log of a complex number in a complex base" #t (close? (make-rectangular 1. 0.) (log +i +i)))
  (test "sqrt" #t (close? (make-rectangular 1. 2.) (sqrt -3.0+4.0i)))
  (test "sqrt below the real axis" #t (close? (make-rectangular 2. -1.) (sqrt 3.0-4.0i)))
  (test "sqrt of +i" #t (close? (make-rectangular 0.7071067811865476 0.7071067811865475) (sqrt +i)))
  (test "sqrt with a zero real part has an imaginary part that is not negative, -0.0 or not"
        "0.0+1.0i" (number->string (sqrt -1.0-0.0i)))
  (test "sin" #t (close? (make-rectangular 1.2984575814159773 0.6349639147847361) (sin 1.0+1.0i)))
  (test "sin of +i" #t (close? (make-rectangular 0. 1.1752011936438014) (sin +i)))
  (test "cos" #t (close? (make-rectangular 0.8337300251311491 -0.9888977057628651) (cos 1.0+1.0i)))
  (test "tan" #t (close? (make-rectangular 0.2717525853195118 1.0839233273386943) (tan 1.0+1.0i)))
  (test "tan far from the real axis is finite" #t
        (close? (make-rectangular 1.473669946993437e-26 1.) (tan 0.5+30.0i)))
  (test "asin" #t (close? (make-rectangular 0.6662394324925153 1.0612750619050357) (asin 1.0+1.0i)))
  (test "asin below the real axis" #t
        (close? (make-rectangular 0.22101863562288385 -1.4657153519472905) (asin 0.5-2.0i)))
  (test "acos" #t (close? (make-rectangular 0.9045568943023814 -1.0612750619050357) (acos 1.0+1.0i)))
  (test "atan" #t (close? (make-rectangular 1.0172219678978514 0.40235947810852507) (atan 1.0+1.0i)))
  (test "atan below the real axis" #t
        (close? (make-rectangular 1.1265564408348223 -0.09641562020299617) (atan 2.0-0.5i)))
  (test "a complex argument's value is complex, though its imaginary part is zero" #f
        (real? (exp 0.0+0.0i)))
  (test "a real argument's is real where it can be, as before" '(1.0 0.0 2.0)
        (list (exp 0) (log 1.0) (sqrt 4.0)))
  (test-error "atan of two arguments takes real numbers only" "real number" (atan +i 1)))

(test-group "expt with a complex number"
  (test "an exact base to an exact integer power is exact" '(-1 -2+2i -i)
        (list (expt +i 2) (expt 1+i 3) (expt +i -1)))
  (test "an inexact base's integer power is a product" "0.0+2.0i" (number->string (expt 1.0+1.0i 2)))
  (test "to the power zero, one, exact as the base is" '(1 1.0) (list (expt 1+i 0) (expt 1.0+1.0i 0)))
  (test "a power that is not an integer" #t
        (close? (make-rectangular 1.0986841134678098 0.45508986056222733) (expt 1+i 1/2)))
  (test "a complex power" #t (close? (make-rectangular 0.7692389013639721 0.6389612763136348) (expt 2 +i)))
  (test "i to the i" #t (close? (make-rectangular 0.20787957635076193 0.) (expt +i +i)))
  (test "zero to a power whose real part is positive is zero, exact if both are" '(0 0.0 0.0)
        (list (expt 0 1+i) (expt 0.0 1+i) (expt 0 1.0+1.0i)))
  (test "zero to the power zero is one" '(1 1.0) (list (expt 0 (make-rectangular 0 0)) (expt 0.0 0+0.0i)))
  (test-error "zero to a power whose real part is not positive is an error" "expt" (expt 0 +i)))

(test-group "the predicates on numbers, on complex numbers"
  (test "an inexact zero imaginary part is not real, nor rational, nor an integer, as R7RS 6.2.6 has it"
        '(#f #f #f) (list (real? -2.5+0.0i) (rational? 2.0+0.0i) (integer? 2.0+0.0i)))
  (test "an exact zero imaginary part is the real number" '(#t #t #t)
        (list (real? -2.5+0i) (rational? 5+0i) (integer? 3+0i)))
  (test "every complex number is a number" '(#t #t) (list (complex? 2.0+0.0i) (number? 2.0+0.0i)))
  (test-error "an ordering of a number that is not real is an error" "real number" (< 1 2.0+0.0i))
  (test "= compares complex numbers" '(#t #f) (list (= 1 1.0+0.0i) (= 1.0 1.0+1.0i)))
  (test "finite?, infinite? and nan? of an exact complex number" '(#t #f #f)
        (list (finite? 1+2i) (infinite? 1+2i) (nan? 1+2i)))
  (test "and of one with an infinite part" '(#f #t #f)
        (list (finite? 3.0+inf.0i) (infinite? 3.0+inf.0i) (nan? 3.0+inf.0i))))

(test-group "writing a complex number whose real part is an exact zero"
  (test "the real part is left out, as R7RS writes (sqrt -1) => +i" '("+i" "-2i" "+1/2i" "+i")
        (map number->string (list +i (make-rectangular 0 -2) (make-rectangular 0 1/2) (sqrt -1))))
  (test "an inexact zero real part is written" "0.0+1.0i" (number->string (make-rectangular 0.0 1.0)))
  (test "and what is written reads back" #t (eqv? (string->number "+1/2i") (make-rectangular 0 1/2))))

(test-group "numerator and denominator of an inexact number"
  (test "are those of the exact number it is, inexact" '(11.0 2.0 5.0 1.0 -1.0 2.0)
        (list (numerator 5.5) (denominator 5.5) (numerator 5.0) (denominator 5.0)
              (numerator -0.5) (denominator -0.5)))
  (test "and are inexact" '(#f #f) (list (exact? (numerator 5.5)) (exact? (denominator 5.5))))
  (test-error "an infinity has none" "numerator" (numerator +inf.0)))
