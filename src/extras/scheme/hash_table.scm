;; SRFI 125 hash tables.
;;
;; ## Two kinds of table
;;
;; A table whose equality predicate is `eq?`, `eqv?`, `string=?` or
;; `string-ci=?` (or `symbol=?` or `char=?`, which agree with `eq?` and `eqv?`
;; on the keys they accept) is *native*: its associations live directly in a
;; JavaScript store that already compares keys that way, so a lookup is one
;; primitive call and never runs a Scheme predicate. The SRFI allows the user's
;; hash function to be ignored for any predicate at least as fine as `equal?`,
;; and for these the store needs none.
;;
;; Any other table is *general*. The store is keyed by hash value and holds a
;; bucket per hash: a list of `(key . value)` pairs searched with the table's
;; own equality predicate. `equal?` tables are general, since comparing
;; structure needs `equal?` itself. Keeping the bucket search in Scheme is
;; deliberate: the predicate and hash function can be any Scheme procedures,
;; and calling them from here rather than from JavaScript means they behave as
;; they would anywhere else -- including under `call/cc`.
;;
;; A general table keeps its own count, because the store counts buckets.

;; ============================================================================
;; The table record
;; ============================================================================

(define-record-type <hash-table>
  (make-raw-hash-table store kind equality hash comparator mutable? count)
  hash-table?
  ;; The JavaScript store.
  (store table-store)
  ;; The store's kind for a native table; #f for a general one.
  (kind table-kind)
  (equality table-equality)
  ;; The hash function, or #f where a native table was given none.
  (hash table-hash)
  ;; The comparator the table was made from, or #f.
  (comparator table-comparator)
  (mutable? table-mutable? set-table-mutable!)
  ;; The number of associations, for a general table only.
  (count table-count set-table-count!))

;; A value no caller can store, returned by lookups to mean "absent".
(define missing (list 'missing))

;; /**
;;  * The store kind that compares keys as an equality predicate does.
;;  * @param {procedure} equality - An equality predicate.
;;  * @returns {symbol|#f} A store kind, or #f if a store cannot.
;;  */
(define (native-kind equality)
  (cond ((eq? equality eq?) 'eq)
        ((eq? equality eqv?) 'eqv)
        ((eq? equality string=?) 'string)
        ((eq? equality string-ci=?) 'string-ci)
        ((eq? equality symbol=?) 'eq)
        ((eq? equality char=?) 'eqv)
        (else #f)))

;; /**
;;  * The hash function to report for a native table given none.
;;  * @param {symbol} kind - The store kind.
;;  * @returns {procedure|#f}
;;  */
(define (standard-hash kind)
  (case kind
    ((string) string-hash)
    ((string-ci) string-ci-hash)
    (else #f)))

;; /**
;;  * Makes an empty, mutable table.
;;  * @param {string} who - The calling procedure, for error messages.
;;  * @param {procedure} equality - The equality predicate.
;;  * @param {procedure|#f} hash - The hash function, if one was given.
;;  * @param {comparator|#f} comparator - The comparator, if one was given.
;;  * @returns {hash-table}
;;  */
(define (make-table who equality hash comparator)
  (unless (procedure? equality)
    (error (string-append who ": not an equality predicate or comparator") equality))
  (let ((kind (native-kind equality)))
    (cond (kind
           (make-raw-hash-table (%make-hash-store kind) kind equality
                                (or hash (standard-hash kind)) comparator #t 0))
          ((or hash (and (eq? equality equal?) default-hash))
           => (lambda (hash)
                (make-raw-hash-table (%make-hash-store 'eqv) #f equality hash comparator #t 0)))
          (else
           (error (string-append who ": no hash function was given for this equality predicate")
                  equality)))))

;; /**
;;  * Makes an empty table from a constructor's leading arguments: either a
;;  * comparator, or an equality predicate optionally followed by a hash
;;  * function. Anything after those -- a capacity, `weak-keys` -- is accepted
;;  * and ignored, as the SRFI permits.
;;  * @param {string} who - The calling procedure, for error messages.
;;  * @param {comparator|procedure} spec - A comparator or equality predicate.
;;  * @param {list} args - The arguments after it.
;;  * @returns {hash-table}
;;  */
(define (table-from who spec args)
  (if (comparator? spec)
      (make-table who
                  (comparator-equality-predicate spec)
                  (and (comparator-hashable? spec) (comparator-hash-function spec))
                  spec)
      (make-table who spec
                  (and (pair? args) (procedure? (car args)) (car args))
                  #f)))

;; /**
;;  * Signals an error unless a value is a hash table.
;;  * @param {string} who - The calling procedure.
;;  * @param {*} ht - The value.
;;  */
(define (check-table who ht)
  (unless (hash-table? ht)
    (error (string-append who ": not a hash table") ht)))

;; /**
;;  * Signals an error unless a value is a mutable hash table.
;;  * @param {string} who - The calling procedure.
;;  * @param {*} ht - The value.
;;  */
(define (check-mutable who ht)
  (check-table who ht)
  (unless (table-mutable? ht)
    (error (string-append who ": hash table is immutable") ht)))

;; ============================================================================
;; The primitive operations, for both kinds
;; ============================================================================

;; /**
;;  * The pair holding a key's association in a general table.
;;  * @param {hash-table} ht - A general table.
;;  * @param {*} key - The key.
;;  * @returns {pair|#f}
;;  */
(define (general-entry ht key)
  (let ((same? (table-equality ht)))
    (let loop ((bucket (%hash-store-ref (table-store ht) ((table-hash ht) key) '())))
      (cond ((null? bucket) #f)
            ((same? (caar bucket) key) (car bucket))
            (else (loop (cdr bucket)))))))

;; /**
;;  * A key's value, or `missing`.
;;  * @param {hash-table} ht - The table.
;;  * @param {*} key - The key.
;;  * @returns {*}
;;  */
(define (lookup ht key)
  (if (table-kind ht)
      (%hash-store-ref (table-store ht) key missing)
      (let ((entry (general-entry ht key)))
        (if entry (cdr entry) missing))))

;; /**
;;  * Associates a value with a key, replacing any previous value. Assumes the
;;  * table has been checked for mutability.
;;  * @param {hash-table} ht - The table.
;;  * @param {*} key - The key.
;;  * @param {*} value - The value.
;;  */
(define (put! ht key value)
  (if (table-kind ht)
      (%hash-store-set! (table-store ht) key value)
      (let* ((store (table-store ht))
             (h ((table-hash ht) key))
             (bucket (%hash-store-ref store h '()))
             (same? (table-equality ht)))
        (let loop ((b bucket))
          (cond ((null? b)
                 (%hash-store-set! store h (cons (cons key value) bucket))
                 (set-table-count! ht (+ (table-count ht) 1)))
                ((same? (caar b) key) (set-cdr! (car b) value))
                (else (loop (cdr b))))))))

;; /**
;;  * Removes a key's association. Assumes the table has been checked for
;;  * mutability.
;;  * @param {hash-table} ht - The table.
;;  * @param {*} key - The key.
;;  * @returns {boolean} Whether there was one.
;;  */
(define (remove! ht key)
  (if (table-kind ht)
      (%hash-store-delete! (table-store ht) key)
      (let* ((store (table-store ht))
             (h ((table-hash ht) key))
             (same? (table-equality ht)))
        (let loop ((b (%hash-store-ref store h '())) (kept '()))
          (cond ((null? b) #f)
                ((same? (caar b) key)
                 (let ((rest (append-reverse kept (cdr b))))
                   (if (null? rest)
                       (%hash-store-delete! store h)
                       (%hash-store-set! store h rest)))
                 (set-table-count! ht (- (table-count ht) 1))
                 #t)
                (else (loop (cdr b) (cons (car b) kept))))))))

;; /**
;;  * Prepends the reverse of one list to another.
;;  * @param {list} reversed - The list to reverse.
;;  * @param {list} tail - The list to prepend to.
;;  * @returns {list}
;;  */
(define (append-reverse reversed tail)
  (if (null? reversed)
      tail
      (append-reverse (cdr reversed) (cons (car reversed) tail))))

;; /**
;;  * The associations of a table, as newly allocated `(key . value)` pairs.
;;  *
;;  * Whole-table operations walk this rather than the store, so a procedure
;;  * that mutates the table while it is being walked -- which `map!` and
;;  * `prune!` do on purpose -- cannot disturb the walk.
;;  *
;;  * @param {hash-table} ht - The table.
;;  * @returns {list}
;;  */
(define (entries ht)
  (let ((store (table-store ht)))
    (if (table-kind ht)
        (map cons (%hash-store-keys store) (%hash-store-values store))
        (let loop ((buckets (%hash-store-values store)) (result '()))
          (if (null? buckets)
              result
              (loop (cdr buckets)
                    (let collect ((b (car buckets)) (result result))
                      (if (null? b)
                          result
                          (collect (cdr b) (cons (cons (caar b) (cdar b)) result))))))))))

;; ============================================================================
;; Constructors
;; ============================================================================

;; /**
;;  * A new, empty, mutable hash table.
;;  * @param {comparator|procedure} spec - A comparator, or an equality
;;  *   predicate optionally followed by a hash function.
;;  * @returns {hash-table}
;;  */
(define (make-hash-table spec . args)
  (table-from "make-hash-table" spec args))

;; /**
;;  * A new, immutable hash table holding the given associations.
;;  * @param {comparator} comparator - The comparator.
;;  * @param {...*} args - Alternating keys and values.
;;  * @returns {hash-table}
;;  */
(define (hash-table comparator . args)
  (let ((ht (table-from "hash-table" comparator '())))
    (let loop ((args args))
      (cond ((null? args) #t)
            ((null? (cdr args)) (error "hash-table: a key has no value" (car args)))
            (else (put! ht (car args) (cadr args)) (loop (cddr args)))))
    (set-table-mutable! ht #f)
    ht))

;; /**
;;  * A new hash table filled by unfolding a seed.
;;  * @param {procedure} stop? - Whether to stop at a seed.
;;  * @param {procedure} mapper - Returns a key and value for a seed.
;;  * @param {procedure} successor - The seed after a seed.
;;  * @param {*} seed - The first seed.
;;  * @param {comparator} comparator - The comparator.
;;  * @returns {hash-table}
;;  */
(define (hash-table-unfold stop? mapper successor seed comparator . args)
  (let ((ht (table-from "hash-table-unfold" comparator args)))
    (let loop ((seed seed))
      (if (stop? seed)
          ht
          (call-with-values (lambda () (mapper seed))
            (lambda (key value)
              (put! ht key value)
              (loop (successor seed))))))))

;; /**
;;  * A new hash table holding an alist's associations. Earlier associations
;;  * take precedence over later ones for the same key.
;;  * @param {list} alist - The associations.
;;  * @param {comparator|procedure} spec - As for `make-hash-table`.
;;  * @returns {hash-table}
;;  */
(define (alist->hash-table alist spec . args)
  (let ((ht (table-from "alist->hash-table" spec args)))
    (for-each (lambda (pair)
                (when (eq? (lookup ht (car pair)) missing)
                  (put! ht (car pair) (cdr pair))))
              alist)
    ht))

;; ============================================================================
;; Predicates
;; ============================================================================

;; /**
;;  * Whether a key has an association.
;;  * @param {hash-table} ht - The table.
;;  * @param {*} key - The key.
;;  * @returns {boolean}
;;  */
(define (hash-table-contains? ht key)
  (check-table "hash-table-contains?" ht)
  (if (table-kind ht)
      (%hash-store-contains? (table-store ht) key)
      (if (general-entry ht key) #t #f)))

(define hash-table-exists? hash-table-contains?)

;; /**
;;  * Whether a table has no associations.
;;  * @param {hash-table} ht - The table.
;;  * @returns {boolean}
;;  */
(define (hash-table-empty? ht)
  (= (hash-table-size ht) 0))

;; /**
;;  * Whether two tables have the same keys, with values equal under a
;;  * comparator.
;;  * @param {comparator} value-comparator - Compares values.
;;  * @param {hash-table} ht1 - The first table.
;;  * @param {hash-table} ht2 - The second table.
;;  * @returns {boolean}
;;  */
(define (hash-table=? value-comparator ht1 ht2)
  (check-table "hash-table=?" ht1)
  (check-table "hash-table=?" ht2)
  (let ((same? (comparator-equality-predicate value-comparator)))
    (and (= (hash-table-size ht1) (hash-table-size ht2))
         (let loop ((es (entries ht1)))
           (or (null? es)
               (let ((other (lookup ht2 (caar es))))
                 (and (not (eq? other missing))
                      (same? (cdar es) other)
                      (loop (cdr es)))))))))

;; /**
;;  * Whether a table can be changed.
;;  * @param {hash-table} ht - The table.
;;  * @returns {boolean}
;;  */
(define (hash-table-mutable? ht)
  (check-table "hash-table-mutable?" ht)
  (table-mutable? ht))

;; ============================================================================
;; Accessors
;; ============================================================================

;; /**
;;  * A key's value. If the key is absent, calls `failure` with no arguments and
;;  * returns its result, or signals an error if there is no `failure`. If the
;;  * key is present and `success` is given, returns `success` applied to the
;;  * value.
;;  * @param {hash-table} ht - The table.
;;  * @param {*} key - The key.
;;  * @param {procedure} [failure] - Called when the key is absent.
;;  * @param {procedure} [success] - Applied to the value when present.
;;  * @returns {*}
;;  */
(define hash-table-ref
  (case-lambda
    ((ht key)
     (check-table "hash-table-ref" ht)
     (let ((value (lookup ht key)))
       (if (eq? value missing)
           (error "hash-table-ref: key not found" key)
           value)))
    ((ht key failure)
     (check-table "hash-table-ref" ht)
     (let ((value (lookup ht key)))
       (if (eq? value missing) (failure) value)))
    ((ht key failure success)
     (check-table "hash-table-ref" ht)
     (let ((value (lookup ht key)))
       (if (eq? value missing) (failure) (success value))))))

;; /**
;;  * A key's value, or a default when the key is absent.
;;  * @param {hash-table} ht - The table.
;;  * @param {*} key - The key.
;;  * @param {*} default - Returned when the key is absent.
;;  * @returns {*}
;;  */
(define (hash-table-ref/default ht key default)
  (check-table "hash-table-ref/default" ht)
  (if (table-kind ht)
      (%hash-store-ref (table-store ht) key default)
      (let ((entry (general-entry ht key)))
        (if entry (cdr entry) default))))

;; ============================================================================
;; Mutators
;; ============================================================================

;; /**
;;  * Associates values with keys, replacing any previous values.
;;  * @param {hash-table} ht - The table.
;;  * @param {...*} args - Alternating keys and values, processed left to right.
;;  */
(define hash-table-set!
  (case-lambda
    ((ht key value)
     (check-mutable "hash-table-set!" ht)
     (put! ht key value))
    ((ht . args)
     (check-mutable "hash-table-set!" ht)
     (let loop ((args args))
       (cond ((null? args) #t)
             ((null? (cdr args)) (error "hash-table-set!: a key has no value" (car args)))
             (else (put! ht (car args) (cadr args)) (loop (cddr args))))))))

;; /**
;;  * Removes the associations of the given keys.
;;  * @param {hash-table} ht - The table.
;;  * @param {...*} keys - The keys.
;;  * @returns {integer} How many of the keys had associations.
;;  */
(define (hash-table-delete! ht . keys)
  (check-mutable "hash-table-delete!" ht)
  (let loop ((keys keys) (n 0))
    (if (null? keys)
        n
        (loop (cdr keys) (if (remove! ht (car keys)) (+ n 1) n)))))

;; /**
;;  * A key's value; if absent, stores and returns the result of `failure`.
;;  * @param {hash-table} ht - The table.
;;  * @param {*} key - The key.
;;  * @param {procedure} failure - Computes the value for an absent key.
;;  * @returns {*}
;;  */
(define (hash-table-intern! ht key failure)
  (check-mutable "hash-table-intern!" ht)
  (let ((value (lookup ht key)))
    (if (eq? value missing)
        (let ((value (failure)))
          (put! ht key value)
          value)
        value)))

;; /**
;;  * Replaces a key's value with the result of applying `updater` to what
;;  * `hash-table-ref` with the same arguments returns.
;;  * @param {hash-table} ht - The table.
;;  * @param {*} key - The key.
;;  * @param {procedure} updater - Computes the new value.
;;  * @param {procedure} [failure] - As for `hash-table-ref`.
;;  * @param {procedure} [success] - As for `hash-table-ref`.
;;  */
(define hash-table-update!
  (case-lambda
    ((ht key updater)
     (check-mutable "hash-table-update!" ht)
     (put! ht key (updater (hash-table-ref ht key))))
    ((ht key updater failure)
     (check-mutable "hash-table-update!" ht)
     (put! ht key (updater (hash-table-ref ht key failure))))
    ((ht key updater failure success)
     (check-mutable "hash-table-update!" ht)
     (put! ht key (updater (hash-table-ref ht key failure success))))))

;; /**
;;  * Replaces a key's value with the result of applying `updater` to it, or to
;;  * a default when the key is absent.
;;  * @param {hash-table} ht - The table.
;;  * @param {*} key - The key.
;;  * @param {procedure} updater - Computes the new value.
;;  * @param {*} default - Stands in for an absent value.
;;  */
(define (hash-table-update!/default ht key updater default)
  (check-mutable "hash-table-update!/default" ht)
  (let ((value (lookup ht key)))
    (put! ht key (updater (if (eq? value missing) default value)))))

;; /**
;;  * Removes an arbitrary association and returns its key and value.
;;  * @param {hash-table} ht - A non-empty table.
;;  * @returns {values} The key and the value.
;;  */
(define (hash-table-pop! ht)
  (check-mutable "hash-table-pop!" ht)
  (let* ((store (table-store ht))
         (some (%hash-store-some-key store missing)))
    (cond ((eq? some missing)
           (error "hash-table-pop!: hash table is empty" ht))
          ((table-kind ht)
           (let ((value (%hash-store-ref store some missing)))
             (%hash-store-delete! store some)
             (values some value)))
          (else
           ;; `some` is a hash; the association is the head of its bucket.
           (let ((entry (car (%hash-store-ref store some '()))))
             (remove! ht (car entry))
             (values (car entry) (cdr entry)))))))

;; /**
;;  * Removes every association.
;;  * @param {hash-table} ht - The table.
;;  */
(define (hash-table-clear! ht)
  (check-mutable "hash-table-clear!" ht)
  (%hash-store-clear! (table-store ht))
  (set-table-count! ht 0))

;; ============================================================================
;; The whole hash table
;; ============================================================================

;; /**
;;  * The number of associations.
;;  * @param {hash-table} ht - The table.
;;  * @returns {integer}
;;  */
(define (hash-table-size ht)
  (check-table "hash-table-size" ht)
  (if (table-kind ht)
      (%hash-store-size (table-store ht))
      (table-count ht)))

;; /**
;;  * The keys, as a newly allocated list.
;;  * @param {hash-table} ht - The table.
;;  * @returns {list}
;;  */
(define (hash-table-keys ht)
  (check-table "hash-table-keys" ht)
  (if (table-kind ht)
      (%hash-store-keys (table-store ht))
      (map car (entries ht))))

;; /**
;;  * The values, as a newly allocated list.
;;  * @param {hash-table} ht - The table.
;;  * @returns {list}
;;  */
(define (hash-table-values ht)
  (check-table "hash-table-values" ht)
  (if (table-kind ht)
      (%hash-store-values (table-store ht))
      (map cdr (entries ht))))

;; /**
;;  * The keys and the values, as two lists in corresponding order.
;;  * @param {hash-table} ht - The table.
;;  * @returns {values}
;;  */
(define (hash-table-entries ht)
  (check-table "hash-table-entries" ht)
  (let ((es (entries ht)))
    (values (map car es) (map cdr es))))

;; /**
;;  * The first true result of `proc` applied to a key and value, or the result
;;  * of `failure` if there is none.
;;  * @param {procedure} proc - Applied to each key and value.
;;  * @param {hash-table} ht - The table.
;;  * @param {procedure} failure - Called if `proc` never returns true.
;;  * @returns {*}
;;  */
(define (hash-table-find proc ht failure)
  (check-table "hash-table-find" ht)
  (let loop ((es (entries ht)))
    (if (null? es)
        (failure)
        (or (proc (caar es) (cdar es))
            (loop (cdr es))))))

;; /**
;;  * How many associations satisfy a predicate of key and value.
;;  * @param {procedure} pred - The predicate.
;;  * @param {hash-table} ht - The table.
;;  * @returns {integer}
;;  */
(define (hash-table-count pred ht)
  (check-table "hash-table-count" ht)
  (let loop ((es (entries ht)) (n 0))
    (if (null? es)
        n
        (loop (cdr es) (if (pred (caar es) (cdar es)) (+ n 1) n)))))

;; ============================================================================
;; Mapping and folding
;; ============================================================================

;; /**
;;  * A new table, made from a comparator, with each key's value replaced by
;;  * `proc` applied to it.
;;  * @param {procedure} proc - Applied to each value.
;;  * @param {comparator} comparator - For the new table.
;;  * @param {hash-table} ht - The table.
;;  * @returns {hash-table}
;;  */
(define (hash-table-map proc comparator ht)
  (check-table "hash-table-map" ht)
  (let ((result (table-from "hash-table-map" comparator '())))
    (for-each (lambda (e) (put! result (car e) (proc (cdr e)))) (entries ht))
    result))

;; /**
;;  * Applies `proc` to each key and value, for effect. The SRFI 69 argument
;;  * order, table first, is accepted too.
;;  * @param {procedure} proc - Applied to each key and value.
;;  * @param {hash-table} ht - The table.
;;  */
(define (hash-table-for-each proc ht)
  (if (hash-table? proc)
      (hash-table-for-each ht proc)
      (begin
        (check-table "hash-table-for-each" ht)
        (for-each (lambda (e) (proc (car e) (cdr e))) (entries ht)))))

;; /**
;;  * `hash-table-for-each` with the table first. Deprecated.
;;  * @param {hash-table} ht - The table.
;;  * @param {procedure} proc - Applied to each key and value.
;;  */
(define (hash-table-walk ht proc)
  (hash-table-for-each proc ht))

;; /**
;;  * Replaces each value with `proc` applied to its key and value.
;;  * @param {procedure} proc - Computes each new value.
;;  * @param {hash-table} ht - The table.
;;  */
(define (hash-table-map! proc ht)
  (check-mutable "hash-table-map!" ht)
  (for-each (lambda (e) (put! ht (car e) (proc (car e) (cdr e)))) (entries ht)))

;; /**
;;  * The results of applying `proc` to each key and value.
;;  * @param {procedure} proc - Applied to each key and value.
;;  * @param {hash-table} ht - The table.
;;  * @returns {list}
;;  */
(define (hash-table-map->list proc ht)
  (check-table "hash-table-map->list" ht)
  (map (lambda (e) (proc (car e) (cdr e))) (entries ht)))

;; /**
;;  * Folds `proc` over the associations: each call receives a key, its value
;;  * and the accumulated result, starting from `seed`. The SRFI 69 argument
;;  * order, `(hash-table-fold ht proc seed)`, is accepted too.
;;  * @param {procedure} proc - Combines a key, value and accumulator.
;;  * @param {*} seed - The initial accumulator.
;;  * @param {hash-table} ht - The table.
;;  * @returns {*}
;;  */
(define (hash-table-fold proc seed ht)
  (if (hash-table? proc)
      (hash-table-fold seed ht proc)
      (begin
        (check-table "hash-table-fold" ht)
        (let loop ((es (entries ht)) (acc seed))
          (if (null? es)
              acc
              (loop (cdr es) (proc (caar es) (cdar es) acc)))))))

;; /**
;;  * Removes the associations whose key and value satisfy `proc`.
;;  * @param {procedure} proc - The predicate.
;;  * @param {hash-table} ht - The table.
;;  */
(define (hash-table-prune! proc ht)
  (check-mutable "hash-table-prune!" ht)
  (for-each (lambda (e) (when (proc (car e) (cdr e)) (remove! ht (car e))))
            (entries ht)))

;; ============================================================================
;; Copying and conversion
;; ============================================================================

;; /**
;;  * A new table like `ht`, with a copy of its store.
;;  * @param {hash-table} ht - The table.
;;  * @param {boolean} mutable? - Whether the copy is mutable.
;;  * @returns {hash-table}
;;  */
(define (copy-table ht mutable?)
  (let ((store (table-store ht)))
    (make-raw-hash-table
      (if (table-kind ht)
          (%hash-store-copy store)
          ;; Buckets hold mutable pairs, so they are copied, not shared.
          (let ((copy (%make-hash-store 'eqv)))
            (for-each (lambda (h bucket)
                        (%hash-store-set! copy h
                                          (map (lambda (e) (cons (car e) (cdr e))) bucket)))
                      (%hash-store-keys store)
                      (%hash-store-values store))
            copy))
      (table-kind ht)
      (table-equality ht)
      (table-hash ht)
      (table-comparator ht)
      mutable?
      (table-count ht))))

;; /**
;;  * A copy of a table, immutable unless `mutable?` is true.
;;  * @param {hash-table} ht - The table.
;;  * @param {boolean} [mutable?] - Whether the copy is mutable.
;;  * @returns {hash-table}
;;  */
(define hash-table-copy
  (case-lambda
    ((ht) (hash-table-copy ht #f))
    ((ht mutable?)
     (check-table "hash-table-copy" ht)
     (copy-table ht (if mutable? #t #f)))))

;; /**
;;  * An empty, mutable table with the same equivalence as `ht`.
;;  * @param {hash-table} ht - The table.
;;  * @returns {hash-table}
;;  */
(define (hash-table-empty-copy ht)
  (check-table "hash-table-empty-copy" ht)
  (make-raw-hash-table (%make-hash-store (or (table-kind ht) 'eqv))
                       (table-kind ht)
                       (table-equality ht)
                       (table-hash ht)
                       (table-comparator ht)
                       #t
                       0))

;; /**
;;  * The associations as a newly allocated alist.
;;  * @param {hash-table} ht - The table.
;;  * @returns {list}
;;  */
(define (hash-table->alist ht)
  (check-table "hash-table->alist" ht)
  (entries ht))

;; ============================================================================
;; Hash tables as sets
;; ============================================================================

;; /**
;;  * Adds to `ht1` the associations of `ht2` whose keys it lacks.
;;  * @param {hash-table} ht1 - Updated and returned.
;;  * @param {hash-table} ht2 - The other table.
;;  * @returns {hash-table} `ht1`.
;;  */
(define (hash-table-union! ht1 ht2)
  (check-mutable "hash-table-union!" ht1)
  (check-table "hash-table-union!" ht2)
  (for-each (lambda (e)
              (when (eq? (lookup ht1 (car e)) missing)
                (put! ht1 (car e) (cdr e))))
            (entries ht2))
  ht1)

(define hash-table-merge! hash-table-union!)

;; /**
;;  * Removes from `ht1` the associations whose keys `ht2` lacks.
;;  * @param {hash-table} ht1 - Updated and returned.
;;  * @param {hash-table} ht2 - The other table.
;;  * @returns {hash-table} `ht1`.
;;  */
(define (hash-table-intersection! ht1 ht2)
  (check-mutable "hash-table-intersection!" ht1)
  (check-table "hash-table-intersection!" ht2)
  (for-each (lambda (e)
              (when (eq? (lookup ht2 (car e)) missing)
                (remove! ht1 (car e))))
            (entries ht1))
  ht1)

;; /**
;;  * Removes from `ht1` the associations whose keys `ht2` has.
;;  * @param {hash-table} ht1 - Updated and returned.
;;  * @param {hash-table} ht2 - The other table.
;;  * @returns {hash-table} `ht1`.
;;  */
(define (hash-table-difference! ht1 ht2)
  (check-mutable "hash-table-difference!" ht1)
  (check-table "hash-table-difference!" ht2)
  (for-each (lambda (e)
              (unless (eq? (lookup ht2 (car e)) missing)
                (remove! ht1 (car e))))
            (entries ht1))
  ht1)

;; /**
;;  * Leaves in `ht1` the associations whose keys are in exactly one of the two
;;  * tables.
;;  * @param {hash-table} ht1 - Updated and returned.
;;  * @param {hash-table} ht2 - The other table.
;;  * @returns {hash-table} `ht1`.
;;  */
(define (hash-table-xor! ht1 ht2)
  (check-mutable "hash-table-xor!" ht1)
  (check-table "hash-table-xor!" ht2)
  (for-each (lambda (e)
              (if (eq? (lookup ht1 (car e)) missing)
                  (put! ht1 (car e) (cdr e))
                  (remove! ht1 (car e))))
            (entries ht2))
  ht1)

;; ============================================================================
;; Hash functions and reflectivity (deprecated)
;; ============================================================================
;;
;; `string-hash` and `string-ci-hash` come from SRFI 128; they already accept
;; and ignore the second argument SRFI 125 requires.

(define hash default-hash)
(define hash-by-identity default-hash)

;; /**
;;  * The equality predicate a table was made with.
;;  * @param {hash-table} ht - The table.
;;  * @returns {procedure}
;;  */
(define (hash-table-equivalence-function ht)
  (check-table "hash-table-equivalence-function" ht)
  (table-equality ht))

;; /**
;;  * The hash function a table was made with, or #f where a native table was
;;  * given none and uses none.
;;  * @param {hash-table} ht - The table.
;;  * @returns {procedure|#f}
;;  */
(define (hash-table-hash-function ht)
  (check-table "hash-table-hash-function" ht)
  (table-hash ht))
