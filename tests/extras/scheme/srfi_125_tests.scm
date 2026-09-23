;; SRFI 125: Intermediate hash tables
;;
;; The procedures process associations in an unspecified order, so any result
;; that lists keys or values is sorted before it is compared.
;;
;; Tables built on eq?, eqv?, string=? and string-ci=? are stored differently
;; from those built on anything else -- see src/extras/scheme/hash_table.scm --
;; so most behaviour is checked for both kinds.

(import (scheme base)
        (scheme char)
        (srfi 125)
        (srfi 128))

;; /**
;;  * Sorts a list of real numbers into ascending order.
;;  * @param {list} xs - The numbers.
;;  * @returns {list} A new, sorted list.
;;  */
(define (sort-numbers xs)
  (define (insert x sorted)
    (cond ((null? sorted) (list x))
          ((<= x (car sorted)) (cons x sorted))
          (else (cons (car sorted) (insert x (cdr sorted))))))
  (let loop ((xs xs) (sorted '()))
    (if (null? xs) sorted (loop (cdr xs) (insert (car xs) sorted)))))

;; /**
;;  * Sorts a list of strings into ascending order.
;;  * @param {list} xs - The strings.
;;  * @returns {list} A new, sorted list.
;;  */
(define (sort-strings xs)
  (define (insert x sorted)
    (cond ((null? sorted) (list x))
          ((string<=? x (car sorted)) (cons x sorted))
          (else (cons (car sorted) (insert x (cdr sorted))))))
  (let loop ((xs xs) (sorted '()))
    (if (null? xs) sorted (loop (cdr xs) (insert (car xs) sorted)))))

;; /**
;;  * Sorts an alist with real-number keys by key.
;;  * @param {list} alist - The alist.
;;  * @returns {list} A new alist, sorted by key.
;;  */
(define (sort-alist alist)
  (map (lambda (k) (assv k alist)) (sort-numbers (map car alist))))

;; A comparator whose equality predicate the library cannot recognise, so a
;; table built from it takes the general path: numbers equal when they agree
;; modulo 10.
(define mod10-comparator
  (make-comparator exact-integer?
                   (lambda (a b) (= (modulo a 10) (modulo b 10)))
                   #f
                   (lambda (n) (modulo n 10))))

;; The same equivalence with a hash function that sends everything to one
;; bucket, so every lookup has to search past colliding keys.
(define colliding-comparator
  (make-comparator exact-integer?
                   (lambda (a b) (= (modulo a 10) (modulo b 10)))
                   #f
                   (lambda (n) 0)))

(define number-comparator (make-comparator number? = < number-hash))

;; ---------------------------------------------------------------------------

(test-group "constructors"
  (test "make-hash-table with a comparator" #t
    (hash-table? (make-hash-table (make-equal-comparator))))
  (test "make-hash-table with eq?" #t (hash-table? (make-hash-table eq?)))
  (test "make-hash-table with eqv?" #t (hash-table? (make-hash-table eqv?)))
  (test "make-hash-table with equal?" #t (hash-table? (make-hash-table equal?)))
  (test "make-hash-table with string=?" #t (hash-table? (make-hash-table string=?)))
  (test "make-hash-table with string-ci=?" #t (hash-table? (make-hash-table string-ci=?)))
  (test "make-hash-table with a predicate and a hash function" #t
    (hash-table? (make-hash-table (lambda (a b) (= a b)) number-hash)))
  (test-error "an unknown predicate without a hash function is an error" ""
    (make-hash-table (lambda (a b) (= a b))))
  (test "a capacity and implementation flags are accepted" #t
    (hash-table? (make-hash-table eq? 100 'weak-keys 'thread-safe)))
  (test "a comparator with extra arguments" #t
    (hash-table? (make-hash-table (make-eqv-comparator) 100)))
  (test "a new table is empty" 0 (hash-table-size (make-hash-table equal?)))
  (test "a new table is mutable" #t (hash-table-mutable? (make-hash-table equal?)))

  (let ((ht (hash-table (make-equal-comparator) 'a 1 'b 2)))
    (test "hash-table adds its associations" 2 (hash-table-ref/default ht 'b #f))
    (test "hash-table has the right size" 2 (hash-table-size ht))
    (test "hash-table returns an immutable table" #f (hash-table-mutable? ht)))
  (test "hash-table with no associations" 0
    (hash-table-size (hash-table (make-eqv-comparator))))

  (let ((ht (hash-table-unfold (lambda (i) (= i 5))
                               (lambda (i) (values i (* i i)))
                               (lambda (i) (+ i 1))
                               0
                               (make-eqv-comparator))))
    (test "hash-table-unfold size" 5 (hash-table-size ht))
    (test "hash-table-unfold contents" 16 (hash-table-ref ht 4))
    (test "hash-table-unfold stops before the stop seed" #f (hash-table-contains? ht 5)))

  (let ((ht (alist->hash-table '((a . 1) (b . 2) (a . 3)) (make-eq-comparator))))
    (test "alist->hash-table with a comparator" 2 (hash-table-ref ht 'b))
    (test "earlier associations take precedence" 1 (hash-table-ref ht 'a))
    (test "duplicates are one key" 2 (hash-table-size ht)))
  (let ((ht (alist->hash-table '(("x" . 1) ("x" . 2)) equal?)))
    (test "alist->hash-table with a predicate" 1 (hash-table-ref ht "x")))
  (let ((ht (alist->hash-table '((1 . a) (11 . b)) (lambda (a b) (= a b)) number-hash)))
    (test "alist->hash-table with a predicate and hash" 'b (hash-table-ref ht 11))))

(test-group "predicates"
  (define ht (hash-table (make-equal-comparator) "a" 1))
  (test "hash-table? on a table" #t (hash-table? ht))
  (test "hash-table? on an alist" #f (hash-table? '((a . 1))))
  (test "hash-table? on a vector" #f (hash-table? (vector)))
  (test "hash-table? on a comparator" #f (hash-table? (make-equal-comparator)))
  (test "hash-table-contains? present" #t (hash-table-contains? ht "a"))
  (test "hash-table-contains? absent" #f (hash-table-contains? ht "b"))
  (test "hash-table-exists? is a synonym" #t (hash-table-exists? ht "a"))
  (test "hash-table-empty? on a non-empty table" #f (hash-table-empty? ht))
  (test "hash-table-empty? on an empty table" #t (hash-table-empty? (make-hash-table eq?)))
  (test-error "hash-table-contains? checks its argument" ""
    (hash-table-contains? '((a . 1)) 'a)))

(test-group "hash-table=?"
  (define cmp (make-eqv-comparator))
  (define a (hash-table cmp 1 "one" 2 "two"))
  (test "equal tables" #t
    (hash-table=? (make-equal-comparator) a (hash-table cmp 2 "two" 1 "one")))
  (test "a different value" #f
    (hash-table=? (make-equal-comparator) a (hash-table cmp 1 "one" 2 "deux")))
  (test "a missing key" #f (hash-table=? (make-equal-comparator) a (hash-table cmp 1 "one")))
  (test "an extra key" #f
    (hash-table=? (make-equal-comparator) a (hash-table cmp 1 "one" 2 "two" 3 "three")))
  (test "values compared by the value comparator" #t
    (hash-table=? number-comparator (hash-table cmp 'x 1) (hash-table cmp 'x 1.0)))
  (test "two empty tables" #t
    (hash-table=? (make-equal-comparator) (make-hash-table eqv?) (make-hash-table eqv?))))

(test-group "accessors"
  (define ht (alist->hash-table '((a . 1) (b . 2)) eq?))
  (test "hash-table-ref finds a value" 1 (hash-table-ref ht 'a))
  (test "hash-table-ref calls failure when absent" 'none
    (hash-table-ref ht 'z (lambda () 'none)))
  (test "hash-table-ref ignores failure when present" 2
    (hash-table-ref ht 'b (lambda () 'none)))
  (test "hash-table-ref calls success on the value" 20
    (hash-table-ref ht 'b (lambda () 'none) (lambda (v) (* v 10))))
  (test "hash-table-ref calls failure, not success, when absent" 'none
    (hash-table-ref ht 'z (lambda () 'none) (lambda (v) 'found)))
  (test-error "hash-table-ref without failure on a missing key is an error" ""
    (hash-table-ref ht 'z))
  (test "hash-table-ref/default present" 1 (hash-table-ref/default ht 'a 0))
  (test "hash-table-ref/default absent" 0 (hash-table-ref/default ht 'z 0))
  (test "a stored #f is found, not defaulted" #f
    (begin (hash-table-set! ht 'f #f) (hash-table-ref/default ht 'f 'default))))

(test-group "mutators"
  (define ht (make-hash-table equal?))
  (hash-table-set! ht 'a 1)
  (test "hash-table-set! adds" 1 (hash-table-ref ht 'a))
  (hash-table-set! ht 'a 10)
  (test "hash-table-set! replaces" 10 (hash-table-ref ht 'a))
  (test "replacing does not grow the table" 1 (hash-table-size ht))
  (hash-table-set! ht 'b 2 'c 3 'b 20)
  (test "hash-table-set! takes several pairs, left to right" 20 (hash-table-ref ht 'b))
  (test "size after several pairs" 3 (hash-table-size ht))

  (test "hash-table-delete! returns how many keys it found" 2
    (hash-table-delete! ht 'a 'c 'zzz))
  (test "deleted keys are gone" #f (hash-table-contains? ht 'a))
  (test "others remain" 20 (hash-table-ref ht 'b))
  (test "hash-table-delete! with no keys" 0 (hash-table-delete! ht))

  (test "hash-table-intern! returns an existing value" 20
    (hash-table-intern! ht 'b (lambda () 'unused)))
  (test "hash-table-intern! stores and returns a new value" 99
    (hash-table-intern! ht 'n (lambda () 99)))
  (test "the interned value was stored" 99 (hash-table-ref ht 'n))

  (hash-table-update! ht 'n (lambda (v) (+ v 1)))
  (test "hash-table-update! applies the updater" 100 (hash-table-ref ht 'n))
  (hash-table-update! ht 'new (lambda (v) (cons 'x v)) (lambda () '()))
  (test "hash-table-update! uses failure when absent" '(x) (hash-table-ref ht 'new))
  (hash-table-update! ht 'n (lambda (v) v) (lambda () 0) (lambda (v) (* v 2)))
  (test "hash-table-update! applies success before the updater" 200 (hash-table-ref ht 'n))
  (test-error "hash-table-update! on a missing key without failure is an error" ""
    (hash-table-update! ht 'missing (lambda (v) v)))

  (hash-table-update!/default ht 'count (lambda (v) (+ v 1)) 0)
  (hash-table-update!/default ht 'count (lambda (v) (+ v 1)) 0)
  (test "hash-table-update!/default" 2 (hash-table-ref ht 'count))

  (test-error "hash-table-pop! on an empty table is an error" ""
    (hash-table-pop! (make-hash-table eqv?)))
  (let ((one (make-hash-table eqv?)))
    (hash-table-set! one 7 'seven)
    (test "hash-table-pop! returns the key and value" '(7 seven)
      (call-with-values (lambda () (hash-table-pop! one)) list))
    (test "hash-table-pop! removes the association" 0 (hash-table-size one)))
  (let ((many (alist->hash-table '((1 . a) (2 . b) (3 . c)) eqv?)))
    (let-values (((k v) (hash-table-pop! many)))
      (test "a popped association is one that was there" v
        (cdr (assv k '((1 . a) (2 . b) (3 . c)))))
      (test "and it is gone" #f (hash-table-contains? many k))))

  (hash-table-clear! ht)
  (test "hash-table-clear!" 0 (hash-table-size ht))
  (test "a cleared table is usable" 'v
    (begin (hash-table-set! ht 'k 'v) (hash-table-ref ht 'k))))

(test-group "immutable tables"
  (define ht (hash-table (make-eqv-comparator) 1 'one))
  (test-error "hash-table-set! on an immutable table is an error" ""
    (hash-table-set! ht 2 'two))
  (test-error "hash-table-delete! on an immutable table is an error" ""
    (hash-table-delete! ht 1))
  (test-error "hash-table-clear! on an immutable table is an error" ""
    (hash-table-clear! ht))
  (test-error "hash-table-update! on an immutable table is an error" ""
    (hash-table-update!/default ht 1 (lambda (v) v) 0))
  (test "an immutable table is still readable" 'one (hash-table-ref ht 1))
  (test "hash-table-copy with #t makes it mutable" 'two
    (let ((copy (hash-table-copy ht #t)))
      (hash-table-set! copy 2 'two)
      (hash-table-ref copy 2)))
  (test "copying did not change the original" #f (hash-table-contains? ht 2)))

(test-group "the whole hash table"
  (define ht (alist->hash-table '((1 . 10) (2 . 20) (3 . 30)) eqv?))
  (test "hash-table-size" 3 (hash-table-size ht))
  (test "hash-table-keys" '(1 2 3) (sort-numbers (hash-table-keys ht)))
  (test "hash-table-values" '(10 20 30) (sort-numbers (hash-table-values ht)))
  (test "hash-table-entries returns keys and values in the same order" '((1 . 10) (2 . 20) (3 . 30))
    (call-with-values (lambda () (hash-table-entries ht))
      (lambda (ks vs) (sort-alist (map cons ks vs)))))
  (test "hash-table-keys of an empty table" '() (hash-table-keys (make-hash-table eq?)))
  (test "hash-table-find returns what proc returns" 200
    (hash-table-find (lambda (k v) (and (= k 2) (* v 10))) ht (lambda () 'none)))
  (test "hash-table-find calls failure when nothing matches" 'none
    (hash-table-find (lambda (k v) #f) ht (lambda () 'none)))
  (test "hash-table-count" 2 (hash-table-count (lambda (k v) (> v 15)) ht))
  (test "the keys list is newly allocated" 3
    (let ((ks (hash-table-keys ht)))
      (set-car! ks 'clobbered)
      (hash-table-size ht))))

(test-group "mapping and folding"
  (define ht (alist->hash-table '((1 . 10) (2 . 20) (3 . 30)) eqv?))
  (let ((mapped (hash-table-map (lambda (v) (* v 2)) (make-eqv-comparator) ht)))
    (test "hash-table-map applies proc to the values" 40 (hash-table-ref mapped 2))
    (test "hash-table-map keeps the keys" '(1 2 3) (sort-numbers (hash-table-keys mapped)))
    (test "hash-table-map leaves the original alone" 20 (hash-table-ref ht 2)))
  (let ((merged (hash-table-map (lambda (v) v) mod10-comparator
                                (alist->hash-table '((1 . a) (11 . a)) eqv?))))
    (test "hash-table-map collapses keys its comparator identifies" 1
      (hash-table-size merged)))

  (test "hash-table-for-each visits every association" 66
    (let ((sum 0))
      (hash-table-for-each (lambda (k v) (set! sum (+ sum k v))) ht)
      sum))
  (test "hash-table-walk takes the table first" 66
    (let ((sum 0))
      (hash-table-walk ht (lambda (k v) (set! sum (+ sum k v))))
      sum))

  (test "hash-table-map->list" '(11 22 33)
    (sort-numbers (hash-table-map->list (lambda (k v) (+ k v)) ht)))
  (test "hash-table-fold" 66 (hash-table-fold (lambda (k v acc) (+ k v acc)) 0 ht))
  (test "hash-table-fold with the deprecated argument order" 66
    (hash-table-fold ht (lambda (k v acc) (+ k v acc)) 0))
  (test "hash-table-fold over an empty table returns the seed" 'seed
    (hash-table-fold (lambda (k v acc) 'no) 'seed (make-hash-table eq?)))

  (let ((copy (hash-table-copy ht #t)))
    (hash-table-map! (lambda (k v) (+ k v)) copy)
    (test "hash-table-map! replaces each value" '((1 . 11) (2 . 22) (3 . 33))
      (sort-alist (hash-table->alist copy))))
  (let ((copy (hash-table-copy ht #t)))
    (hash-table-prune! (lambda (k v) (odd? k)) copy)
    (test "hash-table-prune! removes what proc accepts" '(2)
      (hash-table-keys copy))))

(test-group "copying and conversion"
  (define ht (alist->hash-table '((1 . a) (2 . b)) eqv?))
  (let ((copy (hash-table-copy ht)))
    (test "hash-table-copy has the associations" 'b (hash-table-ref copy 2))
    (test "hash-table-copy is immutable by default" #f (hash-table-mutable? copy)))
  (let ((copy (hash-table-copy ht #t)))
    (hash-table-set! copy 1 'changed)
    (test "a copy is independent of its original" 'a (hash-table-ref ht 1)))
  (let ((empty (hash-table-empty-copy ht)))
    (test "hash-table-empty-copy is empty" 0 (hash-table-size empty))
    (test "hash-table-empty-copy is mutable" #t (hash-table-mutable? empty))
    (hash-table-set! empty 3.0 'x)
    (test "hash-table-empty-copy keeps the equivalence" #f (hash-table-contains? empty 3)))
  (test "hash-table->alist" '((1 . a) (2 . b)) (sort-alist (hash-table->alist ht)))
  (test "hash-table->alist returns fresh pairs" 'a
    (let ((alist (hash-table->alist ht)))
      (for-each (lambda (p) (set-cdr! p 'clobbered)) alist)
      (hash-table-ref ht 1)))
  (test "copying a general table" 'b
    (hash-table-ref (hash-table-copy (alist->hash-table '((2 . b)) mod10-comparator)) 12))
  ;; A general table's buckets hold mutable pairs; a copy that shared them
  ;; would see the original's updates.
  (let* ((original (alist->hash-table '((2 . b)) mod10-comparator))
         (copy (hash-table-copy original #t)))
    (hash-table-set! copy 2 'changed)
    (test "a copy of a general table is independent" 'b (hash-table-ref original 2))))

(test-group "hash tables as sets"
  (define (table . alist) (alist->hash-table alist eqv?))
  (let ((a (table '(1 . a1) '(2 . a2))))
    (test "hash-table-union! returns its first argument" #t
      (eq? a (hash-table-union! a (table '(2 . b2) '(3 . b3)))))
    (test "hash-table-union! adds the missing keys, keeping the first table's values"
      '((1 . a1) (2 . a2) (3 . b3)) (sort-alist (hash-table->alist a))))
  (let ((a (table '(1 . a1))))
    (hash-table-merge! a (table '(4 . b4)))
    (test "hash-table-merge! is a synonym" '(1 4) (sort-numbers (hash-table-keys a))))
  (let ((a (table '(1 . a1) '(2 . a2) '(3 . a3))))
    (test "hash-table-intersection! keeps the shared keys" '((2 . a2) (3 . a3))
      (sort-alist (hash-table->alist (hash-table-intersection! a (table '(2 . x) '(3 . y) '(4 . z)))))))
  (let ((a (table '(1 . a1) '(2 . a2) '(3 . a3))))
    (test "hash-table-difference! removes the shared keys" '((1 . a1))
      (hash-table->alist (hash-table-difference! a (table '(2 . x) '(3 . y) '(4 . z))))))
  (let ((a (table '(1 . a1) '(2 . a2))))
    (test "hash-table-xor! keeps the keys in exactly one table" '((1 . a1) (3 . b3))
      (sort-alist (hash-table->alist (hash-table-xor! a (table '(2 . b2) '(3 . b3))))))))

(test-group "equivalence: eq?"
  (define ht (make-hash-table eq?))
  (define key (list 1 2))
  (hash-table-set! ht key 'found)
  (hash-table-set! ht 'sym 'symbol)
  (test "the same object is found" 'found (hash-table-ref/default ht key #f))
  (test "an equal but distinct object is not" #f (hash-table-ref/default ht (list 1 2) #f))
  (test "symbols are found" 'symbol (hash-table-ref/default ht (string->symbol "sym") #f)))

(test-group "equivalence: eqv?"
  (define ht (make-hash-table eqv?))
  (hash-table-set! ht 2 'exact)
  (hash-table-set! ht 2.0 'inexact)
  (hash-table-set! ht 0.0 'zero)
  (hash-table-set! ht -0.0 'negative-zero)
  (hash-table-set! ht (integer->char 97) 'char)
  (hash-table-set! ht 1/3 'third)
  (hash-table-set! ht "str" 'string)
  (test "exact and inexact are different keys" '(exact inexact)
    (list (hash-table-ref ht 2) (hash-table-ref ht 2.0)))
  (test "0.0 and -0.0 are different keys" '(zero negative-zero)
    (list (hash-table-ref ht 0.0) (hash-table-ref ht -0.0)))
  (test "characters are found by value" 'char (hash-table-ref ht #\a))
  (test "a character is not the number of its code point" #f
    (hash-table-ref/default ht 97 #f))
  (test "exact rationals are found by value" 'third (hash-table-ref ht (/ 2 6)))
  (test "a bignum is found by value" 'big
    (begin (hash-table-set! ht (expt 10 30) 'big)
           (hash-table-ref ht (* (expt 10 15) (expt 10 15)))))
  (test "the table counts every distinct key" 8 (hash-table-size ht))
  (test "keys come back as stored" #t
    (and (memv #\a (hash-table-keys ht)) (memv -0.0 (hash-table-keys ht)) #t))
  (test "deleting a character key" 1 (hash-table-delete! ht #\a))
  (test "deleting -0.0 leaves 0.0" 'zero
    (begin (hash-table-delete! ht -0.0) (hash-table-ref ht 0.0))))

(test-group "equivalence: equal?"
  (define ht (make-hash-table equal?))
  (hash-table-set! ht (list 1 2) 'list)
  (hash-table-set! ht (vector 1 "a") 'vector)
  (hash-table-set! ht (string #\h #\i) 'string)
  (hash-table-set! ht 5 'five)
  (test "an equal list is found" 'list (hash-table-ref ht (list 1 2)))
  (test "an equal vector is found" 'vector (hash-table-ref ht (vector 1 "a")))
  (test "an equal string is found" 'string (hash-table-ref ht "hi"))
  (test "numbers use eqv? within equal?" #f (hash-table-ref/default ht 5.0 #f))
  (test "size" 4 (hash-table-size ht))
  (test "replacing an equal key does not grow the table" 4
    (begin (hash-table-set! ht (list 1 2) 'again) (hash-table-size ht)))
  (test "delete an equal key" 1 (hash-table-delete! ht (vector 1 "a")))
  (test "it is gone" #f (hash-table-contains? ht (vector 1 "a"))))

(test-group "equivalence: string=? and string-ci=?"
  (define cs (make-hash-table string=?))
  (define ci (make-hash-table string-ci=?))
  (hash-table-set! cs "Key" 1)
  (hash-table-set! ci "Key" 1)
  (test "string=? distinguishes case" #f (hash-table-ref/default cs "KEY" #f))
  (test "string-ci=? ignores case" 1 (hash-table-ref/default ci "KEY" #f))
  (hash-table-set! ci "kEy" 2)
  (test "string-ci=? replaces under a differently cased key" '(1 2)
    (list (hash-table-size ci) (hash-table-ref ci "key")))
  (test "string-ci=? keeps a stored key" #t
    (string-ci=? "key" (car (hash-table-keys ci))))
  (test-error "a string table rejects a non-string key" ""
    (hash-table-set! cs 'key 1)))

(test-group "equivalence: a comparator the library does not recognise"
  (define ht (make-hash-table mod10-comparator))
  (hash-table-set! ht 3 'three)
  (test "an equivalent key is found" 'three (hash-table-ref ht 13))
  (hash-table-set! ht 23 'replaced)
  (test "an equivalent key replaces" '(1 replaced) (list (hash-table-size ht) (hash-table-ref ht 3)))
  (test "the stored key is kept" '(3) (hash-table-keys ht))
  (hash-table-set! ht 4 'four 5 'five)
  (test "size" 3 (hash-table-size ht))
  (test "delete by an equivalent key" 1 (hash-table-delete! ht 15))
  (test "deleting reduces the size" 2 (hash-table-size ht))
  (test "the equivalence function is reported" #t
    (procedure? (hash-table-equivalence-function ht)))
  (let-values (((k v) (hash-table-pop! ht)))
    (test "hash-table-pop! on a general table" 1 (hash-table-size ht)))
  (hash-table-clear! ht)
  (test "hash-table-clear! on a general table" 0 (hash-table-size ht)))

(test-group "equivalence: every key in one bucket"
  (define ht (make-hash-table colliding-comparator))
  (let loop ((i 0))
    (when (< i 10)
      (hash-table-set! ht i (* i i))
      (loop (+ i 1))))
  (test "all ten keys are stored" 10 (hash-table-size ht))
  (test "each is found past the others" '(0 1 4 9 16 25 36 49 64 81)
    (map (lambda (i) (hash-table-ref ht (+ i 10))) '(0 1 2 3 4 5 6 7 8 9)))
  (test "deleting from the middle of a bucket" 1 (hash-table-delete! ht 5))
  (test "the rest of the bucket survives" '(9 4 36)
    (map (lambda (i) (hash-table-ref/default ht i #f)) '(3 2 6)))
  (test "size after deletion" 9 (hash-table-size ht))
  (test "hash-table-fold sees every entry" 260
    (hash-table-fold (lambda (k v acc) (+ v acc)) 0 ht)))

(test-group "reflection"
  (test "the equivalence function of an equal? table" #t
    (eq? equal? (hash-table-equivalence-function (make-hash-table equal?))))
  (test "the equivalence function of a comparator table" #t
    (eq? eqv? (hash-table-equivalence-function (make-hash-table (make-eqv-comparator)))))
  (test "the hash function is a procedure or #f" #t
    (let ((h (hash-table-hash-function (make-hash-table equal?))))
      (or (procedure? h) (eq? h #f)))))

(test-group "deprecated hash functions"
  (test "hash accepts and ignores a bound" (hash '(1 2)) (hash '(1 2) 100))
  (test "hash of equal structures" #t (= (hash (list "a" 1)) (hash (list "a" 1))))
  (test "string-hash accepts a bound" (string-hash "abc") (string-hash "abc" 100))
  (test "string-ci-hash accepts a bound" (string-ci-hash "ABC") (string-ci-hash "abc" 100))
  (test "hash-by-identity accepts a bound" #t
    (exact-integer? (hash-by-identity 'a 100))))

(test-group "many keys"
  (define eqv-table (make-hash-table eqv?))
  (define equal-table (make-hash-table equal?))
  (let loop ((i 0))
    (when (< i 2000)
      (hash-table-set! eqv-table i (* 2 i))
      (hash-table-set! equal-table (list i) i)
      (loop (+ i 1))))
  (test "an eqv? table holds 2000 keys" 2000 (hash-table-size eqv-table))
  (test "an equal? table holds 2000 keys" 2000 (hash-table-size equal-table))
  (test "every eqv? key is found" #t
    (let loop ((i 0))
      (or (= i 2000)
          (and (= (hash-table-ref eqv-table i) (* 2 i)) (loop (+ i 1))))))
  (test "every equal? key is found" #t
    (let loop ((i 0))
      (or (= i 2000)
          (and (= (hash-table-ref equal-table (list i)) i) (loop (+ i 1))))))
  (let loop ((i 0))
    (when (< i 2000)
      (when (even? i) (hash-table-delete! equal-table (list i)))
      (loop (+ i 1))))
  (test "half deleted" 1000 (hash-table-size equal-table))
  (test "the odd keys remain" 1999 (hash-table-ref equal-table (list 1999))))

(test-group "strings as keys"
  (define ht (make-hash-table (make-comparator string? string=? string<? string-hash)))
  (hash-table-set! ht "apple" 1 "banana" 2)
  (test "a string comparator table" 2 (hash-table-ref ht (string-append "ban" "ana")))
  (test "keys" '("apple" "banana") (sort-strings (hash-table-keys ht))))
