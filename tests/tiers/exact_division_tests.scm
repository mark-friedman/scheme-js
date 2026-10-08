;; exact_division_tests.scm -- quotient, remainder and modulo, in either tier.
;;
;; Compiled code divides two integers held as numbers, with a divisor that is
;; not zero, inline, and calls the primitive for anything else ("Exact
;; division" in src/compiler/inline.scm). These check that the answers are the
;; interpreter's for every sign, for integers small and beyond V8's small
;; integers up to the largest a number holds exactly, and that what the inline
;; path does not take -- an inexact integer, a BigInt, a divisor of zero --
;; still gets the primitive's answer or its error.
;;
;; The file runs twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js), and each procedure is called twice
;; before it is tested, since the tier compiles a procedure on its second call.

;; /**
;;  * The quotient, remainder and modulo of two numbers.
;;  * @param {number} a - The dividend.
;;  * @param {number} b - The divisor.
;;  * @returns {list}
;;  */
(define (divisions a b)
  (list (quotient a b) (remainder a b) (modulo a b)))

;; /**
;;  * Whether dividing by a divisor raises an error.
;;  * @param {number} b - The divisor.
;;  * @returns {boolean}
;;  */
(define (refuses? b)
  (guard (e (#t #t)) (remainder 7 b) #f))

(divisions 7 2)
(divisions 7 2)

;; Each row is (a b quotient remainder modulo), computed apart.
(define rows
  '((7 2 3 1 1) (-7 2 -3 -1 1) (7 -2 -3 1 -1) (-7 -2 3 -1 -1) (6 3 2 0 0) (-6 3 -2 0 0)
    (1073741831 16384 65536 7 7) (-1073741831 16384 -65536 -7 16377) (1073741831 -16384 -65536 7 -16377)
    (2147483653 3 715827884 1 1) (-2147483653 7 -306783379 0 0)
    (4503599627370495 2147483659 2097151 2124414986 2124414986)
    (-4503599627370495 2147483659 -2097151 -2124414986 23068673)
    (4503599627370493 -12345 -364811634456 11173 -1172)
    (9007199254740991 1000003 9007172233 224292 224292)
    (-9007199254740991 -1000003 9007172233 -224292 -224292)
    (9007199254740993 10 900719925474099 3 3)))

(test-group "Exact division"
  (test "the tier compiled it" *tier-attached* (eq? #t (js-ref divisions "$compiled")))
  (for-each (lambda (row)
              (test (string-append "of " (number->string (car row)) " and " (number->string (cadr row)))
                    (cddr row)
                    (divisions (car row) (cadr row))))
            rows)
  ;; JavaScript's `%` gives -0 for a negative dividend; made inexact, an exact
  ;; zero must be 0.0, which `eqv?` tells from -0.0.
  (test "an exact zero is zero, not negative zero" '(#t #t)
        (map (lambda (r) (eqv? (inexact r) 0.)) (cdr (divisions -6 3))))
  (test "inexact integers, which the primitive takes" '(3. 1. 1.) (divisions 7. 2.))
  (test "an inexact integer and an exact one" '(-3. -1. 1.) (divisions -7. 2))
  (test "a divisor of zero is the primitive's error" #t (refuses? 0)))
