;; SRFI 128: Comparators
;;
;; Comparators are what SRFI 125 hash tables are built from, so these tests
;; concentrate on the properties a hash table relies on -- equal objects hash
;; equally, and the four procedures are the ones supplied -- as well as the
;; ordering rules the SRFI fixes for the default comparator.

(import (scheme base)
        (scheme char)
        (scheme complex)
        (srfi 128))

;; /**
;;  * Whether a value is a valid hash: an exact non-negative integer.
;;  * @param {*} h - The value.
;;  * @returns {boolean}
;;  */
(define (valid-hash? h)
  (and (exact-integer? h) (>= h 0)))

(test-group "comparator objects"
  (define cmp (make-comparator string? string=? string<? string-hash))
  (test "comparator? on a comparator" #t (comparator? cmp))
  (test "comparator? on a procedure" #f (comparator? string=?))
  (test "comparator? on a list" #f (comparator? '(1 2)))
  (test "type test is the one supplied" #t
    (eq? string? (comparator-type-test-predicate cmp)))
  (test "equality is the one supplied" #t
    (eq? string=? (comparator-equality-predicate cmp)))
  (test "ordering is the one supplied" #t
    (eq? string<? (comparator-ordering-predicate cmp)))
  (test "hash is the one supplied" #t
    (eq? string-hash (comparator-hash-function cmp)))
  (test "ordered when an ordering is supplied" #t (comparator-ordered? cmp))
  (test "hashable when a hash is supplied" #t (comparator-hashable? cmp))

  (test "comparator-test-type accepts" #t (comparator-test-type cmp "a"))
  (test "comparator-test-type rejects" #f (comparator-test-type cmp 'a))
  (test "comparator-check-type accepts" #t (comparator-check-type cmp "a"))
  (test-error "comparator-check-type signals on the wrong type" ""
    (comparator-check-type cmp 'a))
  (test "comparator-hash applies the hash function" (string-hash "abc")
    (comparator-hash cmp "abc")))

(test-group "missing ordering or hash"
  (define cmp (make-comparator number? = #f #f))
  (test "not ordered" #f (comparator-ordered? cmp))
  (test "not hashable" #f (comparator-hashable? cmp))
  (test "the ordering is still a procedure" #t
    (procedure? (comparator-ordering-predicate cmp)))
  (test-error "calling the missing ordering signals" ""
    ((comparator-ordering-predicate cmp) 1 2))
  (test-error "calling the missing hash signals" ""
    (comparator-hash cmp 1)))

(test-group "standard hash functions"
  (test "boolean-hash" #t (valid-hash? (boolean-hash #t)))
  (test "boolean-hash distinguishes" #f (= (boolean-hash #t) (boolean-hash #f)))
  (test "char-hash" #t (valid-hash? (char-hash #\a)))
  (test "char-hash of equal chars" #t
    (= (char-hash (integer->char 955)) (char-hash (integer->char 955))))
  (test "char-ci-hash ignores case" #t (= (char-ci-hash #\a) (char-ci-hash #\A)))
  (test "string-hash" #t (valid-hash? (string-hash "hello")))
  (test "string-hash of equal strings" #t
    (= (string-hash (string #\a #\b)) (string-hash "ab")))
  (test "string-hash of the empty string" #t (valid-hash? (string-hash "")))
  (test "string-ci-hash ignores case" #t
    (= (string-ci-hash "Hello") (string-ci-hash "hELLO")))
  (test "symbol-hash" #t (valid-hash? (symbol-hash 'abc)))
  (test "symbol-hash of the same symbol" #t
    (= (symbol-hash 'abc) (symbol-hash (string->symbol "abc"))))
  (test "number-hash of an exact integer" #t (valid-hash? (number-hash 42)))
  (test "number-hash of a negative number" #t (valid-hash? (number-hash -42)))
  (test "number-hash of a flonum" #t (valid-hash? (number-hash 2.5)))
  (test "number-hash of a rational" #t (valid-hash? (number-hash 1/3)))
  (test "number-hash of a complex" #t (valid-hash? (number-hash 1+2i)))
  (test "number-hash of an infinity" #t (valid-hash? (number-hash +inf.0)))
  (test "number-hash of a large integer" #t
    (valid-hash? (number-hash 123456789012345678901234567890)))
  ;; A number comparator's equality is `=`, under which exactness does not
  ;; matter, so neither may the hash.
  (test "number-hash agrees with = across exactness" #t
    (= (number-hash 1) (number-hash 1.0)))
  (test "number-hash agrees with = on a dyadic rational" #t
    (= (number-hash 1/2) (number-hash 0.5)))
  (test "number-hash agrees with = on signed zeros" #t
    (= (number-hash 0.0) (number-hash -0.0))))

(test-group "hash bounds and salt"
  (test "hash-bound is a positive exact integer" #t
    (and (exact-integer? (hash-bound)) (> (hash-bound) 0)))
  (test "hash-salt is an exact integer below the bound" #t
    (and (exact-integer? (hash-salt)) (>= (hash-salt) 0) (< (hash-salt) (hash-bound)))))

(test-group "eq, eqv and equal comparators"
  (define eq-cmp (make-eq-comparator))
  (define eqv-cmp (make-eqv-comparator))
  (define equal-cmp (make-equal-comparator))
  (test "eq comparator equality is eq?" #t
    (eq? eq? (comparator-equality-predicate eq-cmp)))
  (test "eqv comparator equality is eqv?" #t
    (eq? eqv? (comparator-equality-predicate eqv-cmp)))
  (test "equal comparator equality is equal?" #t
    (eq? equal? (comparator-equality-predicate equal-cmp)))
  (test "their type test accepts anything" #t
    (and (comparator-test-type eq-cmp 'a)
         (comparator-test-type eqv-cmp "s")
         (comparator-test-type equal-cmp '(1))))
  (test "their hash is default-hash" #t
    (eq? default-hash (comparator-hash-function equal-cmp)))
  (test "equal comparator on structures" #t (=? equal-cmp '(1 #(2 "x")) '(1 #(2 "x")))))

(test-group "pair comparator"
  (define num (make-comparator number? = < number-hash))
  (define str (make-comparator string? string=? string<? string-hash))
  (define cmp (make-pair-comparator num str))
  (test "type test checks both halves" #t (comparator-test-type cmp '(1 . "a")))
  (test "type test rejects a wrong car" #f (comparator-test-type cmp '("a" . "a")))
  (test "type test rejects a non-pair" #f (comparator-test-type cmp 1))
  (test "equality" #t (=? cmp '(1 . "a") '(1.0 . "a")))
  (test "inequality in the cdr" #f (=? cmp '(1 . "a") '(1 . "b")))
  (test "ordering by car first" #t (<? cmp '(1 . "z") '(2 . "a")))
  (test "ordering by cdr when the cars are equal" #t (<? cmp '(1 . "a") '(1 . "b")))
  (test "hash agrees with equality" #t
    (= (comparator-hash cmp '(1 . "a")) (comparator-hash cmp '(1.0 . "a")))))

(test-group "list comparator"
  (define num (make-comparator number? = < number-hash))
  (define cmp (make-list-comparator num list? null? car cdr))
  (test "type test" #t (comparator-test-type cmp '(1 2 3)))
  (test "type test checks the elements" #f (comparator-test-type cmp '(1 a)))
  (test "equal lists" #t (=? cmp '(1 2 3) '(1 2 3)))
  (test "unequal lists" #f (=? cmp '(1 2 3) '(1 2 4)))
  (test "the empty list precedes others" #t (<? cmp '() '(1)))
  (test "a prefix precedes its extension" #t (<? cmp '(1 2) '(1 2 3)))
  (test "lexicographic order" #t (<? cmp '(1 3) '(2)))
  (test "hash agrees with equality" #t
    (= (comparator-hash cmp '(1 2)) (comparator-hash cmp '(1.0 2.0)))))

(test-group "vector comparator"
  (define num (make-comparator number? = < number-hash))
  (define cmp (make-vector-comparator num vector? vector-length vector-ref))
  (define bytes (make-vector-comparator (make-comparator exact-integer? = < number-hash)
                                        bytevector? bytevector-length bytevector-u8-ref))
  (test "equal vectors" #t (=? cmp #(1 2) #(1 2)))
  (test "a shorter vector precedes a longer one" #t (<? cmp #(9 9) #(1 1 1)))
  (test "equal lengths compare elementwise" #t (<? cmp #(1 2) #(1 3)))
  (test "type test checks the elements" #f (comparator-test-type cmp #(1 a)))
  (test "a bytevector comparator" #t (<? bytes (bytevector 1 2) (bytevector 1 3)))
  (test "hash agrees with equality" #t
    (= (comparator-hash cmp #(1 2)) (comparator-hash cmp #(1.0 2.0)))))

(test-group "comparison predicates"
  (define num (make-comparator number? = < number-hash))
  (test "=? with two" #t (=? num 1 1))
  (test "=? with three" #t (=? num 1 1.0 1))
  (test "=? fails" #f (=? num 1 1 2))
  (test "<? chain" #t (<? num 1 2 3))
  (test "<? fails on equal neighbours" #f (<? num 1 2 2))
  (test ">? chain" #t (>? num 3 2 1))
  (test "<=? chain" #t (<=? num 1 1 2))
  (test "<=? fails" #f (<=? num 2 1))
  (test ">=? chain" #t (>=? num 2 2 1))
  (test ">=? fails" #f (>=? num 1 2)))

(test-group "comparator-if<=>"
  (define num (make-comparator number? = < number-hash))
  (test "less than" 'less (comparator-if<=> num 1 2 'less 'equal 'greater))
  (test "equal to" 'equal (comparator-if<=> num 2 2 'less 'equal 'greater))
  (test "greater than" 'greater (comparator-if<=> num 3 2 'less 'equal 'greater))
  (test "without a comparator uses the default" 'less
    (comparator-if<=> "a" "b" 'less 'equal 'greater))
  (test "only the chosen branch is evaluated" 'ok
    (comparator-if<=> num 1 2 'ok (error "evaluated") (error "evaluated"))))

(test-group "default comparator"
  (define d (make-default-comparator))
  (test "is a comparator" #t (comparator? d))
  (test "is ordered and hashable" #t
    (and (comparator-ordered? d) (comparator-hashable? d)))
  (test "its hash is default-hash" #t (eq? default-hash (comparator-hash-function d)))
  (test "the empty list precedes pairs" #t (<? d '() '(1)))
  (test "booleans: #f before #t" #t (<? d #f #t))
  (test "characters in code point order" #t (<? d #\a #\b))
  (test "strings" #t (<? d "abc" "abd"))
  (test "symbols" #t (<? d 'apple 'banana))
  (test "real numbers use =" #t (=? d 1 1.0))
  (test "real numbers use <" #t (<? d 1 1.5))
  (test "complex: by real part first" #t (<? d 1+5i 2+0i))
  (test "complex: then by imaginary part" #t (<? d 1+1i 1+2i))
  (test "vectors: shorter first" #t (<? d #(9) #(1 1)))
  (test "vectors: then elementwise" #t (<? d #(1 2) #(1 3)))
  (test "bytevectors" #t (<? d (bytevector 1) (bytevector 2)))
  (test "pairs compare car then cdr" #t (<? d '(1 . 2) '(1 . 3)))
  (test "lists compare lexicographically" #t (<? d '(1 2) '(1 3)))
  (test "structures are equal by content" #t (=? d '(1 #(2 "x")) '(1 #(2 "x"))))

  ;; Disjoint types are totally ordered against each other: exactly one of <,
  ;; =, > holds for any two values, and it is never = across types.
  (let ((samples (list '() '(1) #t #\a "s" 'sym 1 #(1) (bytevector 1))))
    (test "values of different types are never equal" #t
      (let outer ((xs samples))
        (or (null? xs)
            (and (let inner ((ys (cdr xs)))
                   (or (null? ys)
                       (and (not (=? d (car xs) (car ys)))
                            (not (eq? (<? d (car xs) (car ys))
                                      (<? d (car ys) (car xs))))
                            (inner (cdr ys)))))
                 (outer (cdr xs)))))))

  (test "default-hash of a string is string-hash" (string-hash "abc") (default-hash "abc"))
  (test "default-hash of a symbol is symbol-hash" (symbol-hash 'abc) (default-hash 'abc))
  (test "default-hash of a number is number-hash" (number-hash 7) (default-hash 7))
  (test "default-hash of a char is char-hash" (char-hash #\x) (default-hash #\x))
  (test "default-hash of a boolean is boolean-hash" (boolean-hash #f) (default-hash #f))
  (test "default-hash of equal structures" #t
    (= (default-hash (list 1 (vector 2 "x"))) (default-hash (list 1 (vector 2 "x")))))
  (test "default-hash of the empty list" #t (valid-hash? (default-hash '())))
  (test "default-hash of a bytevector" #t (valid-hash? (default-hash (bytevector 1 2)))))

(test-group "registering a type with the default comparator"
  (define-record-type point (make-point x y) point? (x point-x) (y point-y))
  ;; A default comparator consults the registry when it compares, not when it
  ;; is made, so one made before registering sees the registered type.
  (define d (make-default-comparator))
  (comparator-register-default!
    (make-comparator point?
                     (lambda (a b) (and (= (point-x a) (point-x b)) (= (point-y a) (point-y b))))
                     (lambda (a b) (< (point-x a) (point-x b)))
                     (lambda (p) (number-hash (point-x p)))))
  (test "a registered type compares equal by its own equality" #t
    (=? d (make-point 1 2) (make-point 1 2)))
  (test "a registered type orders by its own ordering" #t
    (<? d (make-point 1 2) (make-point 2 0)))
  (test "a registered type hashes by its own hash" (number-hash 5)
    (default-hash (make-point 5 0)))
  (test "a registered type is not equal to a built-in value" #f
    (=? d (make-point 1 2) 1)))
