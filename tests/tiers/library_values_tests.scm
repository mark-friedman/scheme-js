;; library_values_tests.scm -- a library's procedures, held in values it made as
;; it loaded, in either tier.
;;
;; A shipped library loads from its source, interpreted, and its prebuilt table
;; is installed once it has: each procedure the table holds is bound in place
;; of the closure the source made (src/compiler/prebuilt.js). Values the
;; library made while it loaded can hold those closures -- SRFI 128's default
;; comparators are records holding `default-hash` and the rest -- and must hold
;; the compiled procedures too, or a procedure would not be `eq?` to itself and
;; would run interpreted wherever the value was used. The file runs twice, the
;; program interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js); both runs install every shipped
;; library from its table, as a page does.

(import (srfi 128))

;; /**
;;  * Whether a procedure is compiled.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {boolean}
;;  */
(define (compiled? procedure)
  (eq? #t (js-ref procedure "$compiled")))

;; /**
;;  * Whether every comparator hashes with a given procedure. Loops, so the tier
;;  * compiles it when it is bound.
;;  * @param {procedure} hash - The hash function.
;;  * @param {list} comparators - The comparators.
;;  * @returns {boolean}
;;  */
(define (all-hash-with? hash comparators)
  (let loop ((comparators comparators))
    (or (null? comparators)
        (and (eq? hash (comparator-hash-function (car comparators)))
             (loop (cdr comparators))))))

(test-group "A shipped library's procedures, held in values it made as it loaded"
  (test "SRFI 128 is installed from its prebuilt table" #t (compiled? default-hash))
  (test "the default comparator's hash is default-hash" #t
        (eq? default-hash (comparator-hash-function (make-default-comparator))))
  (test "so are the eq?, eqv? and equal? comparators'" #t
        (all-hash-with? default-hash
                        (list (make-eq-comparator) (make-eqv-comparator) (make-equal-comparator))))
  (test "every procedure the default comparator holds is compiled" #t
        (let ((d (make-default-comparator)))
          (and (compiled? (comparator-type-test-predicate d))
               (compiled? (comparator-equality-predicate d))
               (compiled? (comparator-ordering-predicate d))
               (compiled? (comparator-hash-function d)))))
  (test "a comparator made now holds what it is given" #t
        (all-hash-with? default-hash (list (make-comparator #t equal? #f default-hash)))))

;; A library that is not shipped has no table. With the tier attached, its
;; procedures are compiled at their tenth call once it has loaded
;; (src/compiler/tier.scm), and the list it made as it loaded holds what it
;; held before: the procedure `doubled` is bound to.
(define-library (test library-values)
  (import (scheme base))
  (export doubled procedures)
  (begin
    (define (doubled x) (* 2 x))
    (define procedures (list doubled (vector doubled)))))

(import (test library-values))

(for-each doubled '(1 2 3 4 5 6 7 8 9 10))

(test-group "A library's own procedures, held in values it made as it loaded"
  (test "the tier compiled the procedure, and only in the run with it attached"
        *tier-attached* (compiled? doubled))
  (test "a list it made holds the procedure its name is bound to" #t
        (eq? doubled (car procedures)))
  (test "and so does a vector inside the list" #t
        (eq? doubled (vector-ref (cadr procedures) 0)))
  (test "which computes as before" 6 ((car procedures) 3)))

;; A program's own procedure, compiled by the tier on its second call, stays
;; the object every holder of it has and runs compiled: a list made before the
;; compile holds the procedure its name is bound to, though nothing searched
;; the program's data, which is as large as the program makes it.
(define (tripled x) (* 3 x))

(define program-procedures (list tripled))

(tripled 1)
(tripled 2)

(test-group "A program's own procedure, held in a value it made before the tier compiled it"
  (test "the tier compiled the procedure, and only in the run with it attached"
        *tier-attached* (compiled? tripled))
  (test "a list the program made holds the procedure its name is bound to" #t
        (eq? tripled (car program-procedures)))
  (test "and runs it compiled" *tier-attached* (compiled? (car program-procedures))))
