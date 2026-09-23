;; SRFI 128 comparators.
;;
;; A comparator is a record of four procedures. Everything here is ordinary
;; Scheme over that record, apart from three hash functions that are JavaScript
;; primitives (see src/extras/primitives/hash_table.js): hashing a string needs
;; its code units and hashing a float needs its bits.

;; ============================================================================
;; The comparator record
;; ============================================================================

(define-record-type <comparator>
  (make-raw-comparator type-test equality ordering hash ordered? hashable?)
  comparator?
  (type-test comparator-type-test-predicate)
  (equality comparator-equality-predicate)
  (ordering comparator-ordering-predicate)
  (hash comparator-hash-function)
  (ordered? comparator-ordered?)
  (hashable? comparator-hashable?))

;; /**
;;  * Accepts anything.
;;  * @param {*} x - Ignored.
;;  * @returns {boolean} #t.
;;  */
(define (any? x) #t)

;; /**
;;  * Bundles four procedures as a comparator.
;;  *
;;  * An ordering or hash of #f is replaced by a procedure that signals an error,
;;  * and the comparator reports itself unordered or unhashable. As in the SRFI's
;;  * sample implementation, a type test of #t accepts anything and an equality
;;  * of #t is derived from the ordering.
;;  *
;;  * @param {procedure|#t} type-test - Whether a value is of the right type.
;;  * @param {procedure|#t} equality - Whether two values are the same.
;;  * @param {procedure|#f} ordering - Whether one value precedes another.
;;  * @param {procedure|#f} hash - A hash consistent with equality.
;;  * @returns {comparator}
;;  */
(define (make-comparator type-test equality ordering hash)
  (make-raw-comparator
    (if (eq? type-test #t) any? type-test)
    (if (eq? equality #t)
        (lambda (a b) (not (or (ordering a b) (ordering b a))))
        equality)
    (or ordering
        (lambda (a b) (error "make-comparator: comparator has no ordering" a b)))
    (or hash
        (lambda (x . ignored) (error "make-comparator: comparator has no hash function" x)))
    (if ordering #t #f)
    (if hash #t #f)))

;; ============================================================================
;; Invokers and comparison predicates
;; ============================================================================

;; /**
;;  * Applies a comparator's type test.
;;  * @param {comparator} comparator - The comparator.
;;  * @param {*} obj - The value.
;;  * @returns {boolean}
;;  */
(define (comparator-test-type comparator obj)
  ((comparator-type-test-predicate comparator) obj))

;; /**
;;  * Applies a comparator's type test, signalling an error if it fails.
;;  * @param {comparator} comparator - The comparator.
;;  * @param {*} obj - The value.
;;  * @returns {boolean} #t.
;;  */
(define (comparator-check-type comparator obj)
  (if ((comparator-type-test-predicate comparator) obj)
      #t
      (error "comparator-check-type: value has the wrong type" obj)))

;; /**
;;  * Applies a comparator's hash function.
;;  * @param {comparator} comparator - The comparator.
;;  * @param {*} obj - The value.
;;  * @returns {integer}
;;  */
(define (comparator-hash comparator obj)
  ((comparator-hash-function comparator) obj))

;; /**
;;  * Whether a relation holds between each value and the next.
;;  * @param {procedure} relation - A binary relation.
;;  * @param {*} a - The first value.
;;  * @param {*} b - The second value.
;;  * @param {list} rest - Any further values.
;;  * @returns {boolean}
;;  */
(define (chain relation a b rest)
  (and (relation a b)
       (or (null? rest)
           (chain relation b (car rest) (cdr rest)))))

;; /**
;;  * Whether all the values are equal under a comparator.
;;  * @param {comparator} comparator - The comparator.
;;  * @returns {boolean}
;;  */
(define (=? comparator a b . rest)
  (chain (comparator-equality-predicate comparator) a b rest))

;; /**
;;  * Whether the values are strictly increasing under a comparator.
;;  * @param {comparator} comparator - The comparator.
;;  * @returns {boolean}
;;  */
(define (<? comparator a b . rest)
  (chain (comparator-ordering-predicate comparator) a b rest))

;; /**
;;  * Whether the values are strictly decreasing under a comparator.
;;  * @param {comparator} comparator - The comparator.
;;  * @returns {boolean}
;;  */
(define (>? comparator a b . rest)
  (let ((less? (comparator-ordering-predicate comparator)))
    (chain (lambda (x y) (less? y x)) a b rest)))

;; /**
;;  * Whether the values are non-decreasing under a comparator.
;;  * @param {comparator} comparator - The comparator.
;;  * @returns {boolean}
;;  */
(define (<=? comparator a b . rest)
  (let ((less? (comparator-ordering-predicate comparator)))
    (chain (lambda (x y) (not (less? y x))) a b rest)))

;; /**
;;  * Whether the values are non-increasing under a comparator.
;;  * @param {comparator} comparator - The comparator.
;;  * @returns {boolean}
;;  */
(define (>=? comparator a b . rest)
  (let ((less? (comparator-ordering-predicate comparator)))
    (chain (lambda (x y) (not (less? x y))) a b rest)))

;; /**
;;  * Evaluates one of three expressions according to how two values compare.
;;  * With no comparator, a default comparator is used.
;;  *
;;  * @syntax (comparator-if<=> [comparator] a b less-than equal-to greater-than)
;;  */
(define-syntax comparator-if<=>
  (syntax-rules ()
    ((_ a b less-than equal-to greater-than)
     (comparator-if<=> (make-default-comparator) a b less-than equal-to greater-than))
    ((_ comparator a b less-than equal-to greater-than)
     (let ((c comparator) (x a) (y b))
       (cond (((comparator-ordering-predicate c) x y) less-than)
             (((comparator-equality-predicate c) x y) equal-to)
             (else greater-than))))))

;; ============================================================================
;; Hash functions
;; ============================================================================
;;
;; `string-hash`, `string-ci-hash` and `number-hash` are primitives. Every hash
;; function accepts and ignores a second argument, which SRFI 125 requires of
;; the versions it re-exports.

;; /**
;;  * The bound on the hashes the standard hash functions return.
;;  * @syntax (hash-bound)
;;  */
(define-syntax hash-bound
  (syntax-rules ()
    ((_) (%hash-bound))))

;; /**
;;  * A salt chosen afresh on each run, below `(hash-bound)`. The standard hash
;;  * functions do not use it; it is there for hash functions users write.
;;  * @syntax (hash-salt)
;;  */
(define-syntax hash-salt
  (syntax-rules ()
    ((_) (%hash-salt))))

(define bound (%hash-bound))

;; /**
;;  * Mixes one hash into another.
;;  * @param {integer} h - The hash so far.
;;  * @param {integer} x - The hash to mix in.
;;  * @returns {integer} A hash below the bound.
;;  */
(define (combine h x)
  (modulo (+ (* h 31) x) bound))

;; /**
;;  * Hashes a boolean.
;;  * @param {boolean} b - The boolean.
;;  * @returns {integer}
;;  */
(define (boolean-hash b . ignored)
  (if (boolean? b)
      (if b 1 0)
      (error "boolean-hash: not a boolean" b)))

;; /**
;;  * Hashes a character by its code point.
;;  * @param {char} c - The character.
;;  * @returns {integer}
;;  */
(define (char-hash c . ignored)
  (char->integer c))

;; /**
;;  * Hashes a character so that characters equal under `char-ci=?` agree.
;;  * @param {char} c - The character.
;;  * @returns {integer}
;;  */
(define (char-ci-hash c . ignored)
  (char->integer (char-foldcase c)))

;; /**
;;  * Hashes a symbol by its name.
;;  * @param {symbol} s - The symbol.
;;  * @returns {integer}
;;  */
(define (symbol-hash s . ignored)
  (string-hash (symbol->string s)))

;; /**
;;  * Hashes the elements of a sequence together.
;;  * @param {procedure} element-hash - Hashes one element.
;;  * @param {integer} n - The number of elements.
;;  * @param {procedure} ref - Returns the element at an index.
;;  * @returns {integer}
;;  */
(define (hash-sequence element-hash n ref)
  (let loop ((i 0) (h n))
    (if (= i n)
        h
        (loop (+ i 1) (combine h (element-hash (ref i)))))))

;; /**
;;  * The hash function of default comparators, consistent with both their
;;  * equality and with `equal?`.
;;  *
;;  * A list is walked iteratively rather than recursed down, so a long list does
;;  * not cost stack depth. Values of types registered with
;;  * `comparator-register-default!` are hashed by their comparator; any other
;;  * value `equal?` compares by identity is hashed by identity.
;;  *
;;  * @param {*} obj - The value.
;;  * @returns {integer}
;;  */
(define (default-hash obj . ignored)
  (cond ((pair? obj)
         (let loop ((x obj) (h 7))
           (if (pair? x)
               (loop (cdr x) (combine h (default-hash (car x))))
               (combine h (default-hash x)))))
        ((null? obj) 3)
        ((boolean? obj) (boolean-hash obj))
        ((char? obj) (char-hash obj))
        ((string? obj) (string-hash obj))
        ((symbol? obj) (symbol-hash obj))
        ((number? obj) (number-hash obj))
        ((vector? obj)
         (hash-sequence default-hash (vector-length obj)
                        (lambda (i) (vector-ref obj i))))
        ((bytevector? obj)
         (hash-sequence (lambda (b) b) (bytevector-length obj)
                        (lambda (i) (bytevector-u8-ref obj i))))
        ((registered-comparator obj)
         => (lambda (comparator) (comparator-hash comparator obj)))
        (else (%identity-hash obj))))

;; ============================================================================
;; Comparators built from others
;; ============================================================================

;; /**
;;  * A comparator for pairs, comparing cars and then cdrs.
;;  * @param {comparator} car-comparator - For the cars.
;;  * @param {comparator} cdr-comparator - For the cdrs.
;;  * @returns {comparator}
;;  */
(define (make-pair-comparator car-comparator cdr-comparator)
  (let ((car-test (comparator-type-test-predicate car-comparator))
        (cdr-test (comparator-type-test-predicate cdr-comparator))
        (car=? (comparator-equality-predicate car-comparator))
        (cdr=? (comparator-equality-predicate cdr-comparator))
        (car<? (comparator-ordering-predicate car-comparator))
        (cdr<? (comparator-ordering-predicate cdr-comparator)))
    (make-raw-comparator
      (lambda (x) (and (pair? x) (car-test (car x)) (cdr-test (cdr x))))
      (lambda (a b) (and (car=? (car a) (car b)) (cdr=? (cdr a) (cdr b))))
      (lambda (a b)
        (if (car=? (car a) (car b))
            (cdr<? (cdr a) (cdr b))
            (car<? (car a) (car b))))
      (lambda (x . ignored)
        (combine (comparator-hash car-comparator (car x))
                 (comparator-hash cdr-comparator (cdr x))))
      (and (comparator-ordered? car-comparator) (comparator-ordered? cdr-comparator))
      (and (comparator-hashable? car-comparator) (comparator-hashable? cdr-comparator)))))

;; /**
;;  * A comparator for sequences accessed like lists, in lexicographic order.
;;  * @param {comparator} element-comparator - For the elements.
;;  * @param {procedure} type-test - Whether a value is such a sequence.
;;  * @param {procedure} empty? - Whether a sequence is empty.
;;  * @param {procedure} head - The first element of a non-empty sequence.
;;  * @param {procedure} tail - The rest of a non-empty sequence.
;;  * @returns {comparator}
;;  */
(define (make-list-comparator element-comparator type-test empty? head tail)
  (let ((element-test (comparator-type-test-predicate element-comparator))
        (element=? (comparator-equality-predicate element-comparator))
        (element<? (comparator-ordering-predicate element-comparator)))
    (make-raw-comparator
      (lambda (x)
        (and (type-test x)
             (let loop ((x x))
               (or (empty? x)
                   (and (element-test (head x)) (loop (tail x)))))))
      (lambda (a b)
        (let loop ((a a) (b b))
          (cond ((empty? a) (empty? b))
                ((empty? b) #f)
                (else (and (element=? (head a) (head b))
                           (loop (tail a) (tail b)))))))
      (lambda (a b)
        (let loop ((a a) (b b))
          (cond ((empty? b) #f)
                ((empty? a) #t)
                ((element=? (head a) (head b)) (loop (tail a) (tail b)))
                (else (element<? (head a) (head b))))))
      (lambda (x . ignored)
        (let loop ((x x) (h 5))
          (if (empty? x)
              h
              (loop (tail x) (combine h (comparator-hash element-comparator (head x)))))))
      (comparator-ordered? element-comparator)
      (comparator-hashable? element-comparator))))

;; /**
;;  * A comparator for sequences accessed by index: shorter sequences first, and
;;  * sequences of equal length in lexicographic order.
;;  * @param {comparator} element-comparator - For the elements.
;;  * @param {procedure} type-test - Whether a value is such a sequence.
;;  * @param {procedure} length - The length of a sequence.
;;  * @param {procedure} ref - The element at an index.
;;  * @returns {comparator}
;;  */
(define (make-vector-comparator element-comparator type-test length ref)
  (let ((element-test (comparator-type-test-predicate element-comparator))
        (element=? (comparator-equality-predicate element-comparator))
        (element<? (comparator-ordering-predicate element-comparator)))
    (make-raw-comparator
      (lambda (x)
        (and (type-test x)
             (let ((n (length x)))
               (let loop ((i 0))
                 (or (= i n)
                     (and (element-test (ref x i)) (loop (+ i 1))))))))
      (lambda (a b)
        (let ((n (length a)))
          (and (= n (length b))
               (let loop ((i 0))
                 (or (= i n)
                     (and (element=? (ref a i) (ref b i)) (loop (+ i 1))))))))
      (lambda (a b)
        (let ((na (length a)) (nb (length b)))
          (cond ((< na nb) #t)
                ((> na nb) #f)
                (else
                 (let loop ((i 0))
                   (cond ((= i na) #f)
                         ((element=? (ref a i) (ref b i)) (loop (+ i 1)))
                         (else (element<? (ref a i) (ref b i)))))))))
      (lambda (x . ignored)
        (hash-sequence (comparator-hash-function element-comparator)
                       (length x) (lambda (i) (ref x i))))
      (comparator-ordered? element-comparator)
      (comparator-hashable? element-comparator))))

;; ============================================================================
;; The default comparator
;; ============================================================================

;; Comparators registered with `comparator-register-default!`, oldest first.
(define registered '())

;; /**
;;  * The registered comparator whose type test accepts a value.
;;  * @param {*} obj - The value.
;;  * @returns {comparator|#f}
;;  */
(define (registered-comparator obj)
  (let loop ((cs registered))
    (cond ((null? cs) #f)
          ((comparator-test-type (car cs) obj) (car cs))
          (else (loop (cdr cs))))))

;; /**
;;  * Extends default comparators to a type they do not otherwise order.
;;  * @param {comparator} comparator - Its type test must accept no built-in type.
;;  */
(define (comparator-register-default! comparator)
  (set! registered (append registered (list comparator))))

;; /**
;;  * Where a value's type sits in the default order.
;;  *
;;  * The empty list precedes pairs, as the SRFI requires; the rest of the order
;;  * between types is this implementation's choice. Registered types follow the
;;  * built-in ones in registration order, and anything else comes last.
;;  *
;;  * @param {*} x - The value.
;;  * @returns {integer}
;;  */
(define (type-rank x)
  (cond ((null? x) 0)
        ((pair? x) 1)
        ((boolean? x) 2)
        ((char? x) 3)
        ((string? x) 4)
        ((symbol? x) 5)
        ((number? x) 6)
        ((vector? x) 7)
        ((bytevector? x) 8)
        (else
         (let loop ((cs registered) (rank 9))
           (cond ((null? cs) 1000000)
                 ((comparator-test-type (car cs) x) rank)
                 (else (loop (cdr cs) (+ rank 1))))))))

;; /**
;;  * Compares two numbers, ordering complex numbers by real part and then by
;;  * imaginary part.
;;  * @param {number} a - The first number.
;;  * @param {number} b - The second number.
;;  * @returns {integer} -1, 0 or 1.
;;  */
(define (compare-numbers a b)
  (if (and (real? a) (real? b))
      (cond ((< a b) -1) ((= a b) 0) (else 1))
      (let ((ra (real-part a)) (rb (real-part b)))
        (cond ((< ra rb) -1)
              ((> ra rb) 1)
              (else (let ((ia (imag-part a)) (ib (imag-part b)))
                      (cond ((< ia ib) -1) ((= ia ib) 0) (else 1))))))))

;; /**
;;  * Compares two indexed sequences: shorter first, then elementwise.
;;  * @returns {integer} -1, 0 or 1.
;;  */
(define (compare-sequences a b length ref compare)
  (let ((na (length a)) (nb (length b)))
    (cond ((< na nb) -1)
          ((> na nb) 1)
          (else
           (let loop ((i 0))
             (if (= i na)
                 0
                 (let ((c (compare (ref a i) (ref b i))))
                   (if (= c 0) (loop (+ i 1)) c))))))))

;; /**
;;  * Compares two values of any types in the default order.
;;  *
;;  * Values of a type this implementation cannot order -- procedures, records
;;  * nobody registered -- are equal when `eqv?` and otherwise have no order.
;;  *
;;  * @param {*} a - The first value.
;;  * @param {*} b - The second value.
;;  * @returns {integer} -1, 0 or 1.
;;  */
(define (default-compare a b)
  (let ((ra (type-rank a)) (rb (type-rank b)))
    (cond ((< ra rb) -1)
          ((> ra rb) 1)
          ((pair? a)
           (let ((c (default-compare (car a) (car b))))
             (if (= c 0) (default-compare (cdr a) (cdr b)) c)))
          ((null? a) 0)
          ((boolean? a) (cond ((eq? a b) 0) (a 1) (else -1)))
          ((char? a) (cond ((char<? a b) -1) ((char=? a b) 0) (else 1)))
          ((string? a) (cond ((string<? a b) -1) ((string=? a b) 0) (else 1)))
          ((symbol? a)
           (let ((sa (symbol->string a)) (sb (symbol->string b)))
             (cond ((string<? sa sb) -1) ((string=? sa sb) 0) (else 1))))
          ((number? a) (compare-numbers a b))
          ((vector? a) (compare-sequences a b vector-length vector-ref default-compare))
          ((bytevector? a)
           (compare-sequences a b bytevector-length bytevector-u8-ref
                              (lambda (x y) (cond ((< x y) -1) ((= x y) 0) (else 1)))))
          ((registered-comparator a)
           => (lambda (comparator)
                (cond (((comparator-equality-predicate comparator) a b) 0)
                      (((comparator-ordering-predicate comparator) a b) -1)
                      (else 1))))
          ((eqv? a b) 0)
          (else (error "default comparator: these values have no order" a b)))))

;; /**
;;  * The equality of default comparators. Separate from `default-compare` so
;;  * that asking whether two unorderable values are equal is not an error.
;;  * @param {*} a - The first value.
;;  * @param {*} b - The second value.
;;  * @returns {boolean}
;;  */
(define (default-equality a b)
  (let ((ra (type-rank a)) (rb (type-rank b)))
    (and (= ra rb)
         (if (< ra 1000000)
             (= (default-compare a b) 0)
             (eqv? a b)))))

(define default-comparator
  (make-raw-comparator any?
                       default-equality
                       (lambda (a b) (< (default-compare a b) 0))
                       default-hash
                       #t
                       #t))

;; /**
;;  * The default comparator. It consults the registry when it compares, so
;;  * types registered after it was made are still covered.
;;  * @returns {comparator}
;;  */
(define (make-default-comparator)
  default-comparator)

;; /**
;;  * The ordering of the eq?, eqv? and equal? comparators: the default order.
;;  * The SRFI leaves it to the implementation and allows it to signal an error,
;;  * which it does for values that have no default order.
;;  */
(define default-ordering (comparator-ordering-predicate default-comparator))

(define eq-comparator (make-raw-comparator any? eq? default-ordering default-hash #t #t))
(define eqv-comparator (make-raw-comparator any? eqv? default-ordering default-hash #t #t))
(define equal-comparator (make-raw-comparator any? equal? default-ordering default-hash #t #t))

;; /**
;;  * A comparator whose equality is `eq?`.
;;  * @returns {comparator}
;;  */
(define (make-eq-comparator) eq-comparator)

;; /**
;;  * A comparator whose equality is `eqv?`.
;;  * @returns {comparator}
;;  */
(define (make-eqv-comparator) eqv-comparator)

;; /**
;;  * A comparator whose equality is `equal?`.
;;  * @returns {comparator}
;;  */
(define (make-equal-comparator) equal-comparator)
