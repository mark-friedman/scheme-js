;; List Procedures
;; Higher-order list operations and searching

;; /**
;;  * Native hook for reporting test results.
;;  * This is intended to be overridden by the environment (Node/Browser) if needed,
;;  * or used by the test harness to communicate results to the host.
;;  *
;;  * @param {...*} args - Test result arguments.
;;  * @returns {boolean} #f (default implementation).
;;  */
(define (native-report-test-result . args) #f)

;; /**
;;  * Applies a function to corresponding elements of one or more lists.
;;  * Returns a new list of results. When multiple lists are given,
;;  * the result is the same length as the shortest list (R7RS).
;;  * Supports circular lists when used with multiple lists (stops at shortest).
;;
;;  * @param {procedure} proc - The function to apply.
;;  * @param {list} lst - First list.
;;  * @param {...list} lsts - Additional lists (optional).
;;  * @returns {list} A new list containing the results.
;;  */
(define (map proc lst . lsts)
  (if (not (procedure? proc))
      (error "map: expected procedure" proc))
  ;; For proper error messages, check that first arg is a list or null
  ;; But for circular list support, we only check pair-ness during iteration
  (if (and (not (null? lst)) (not (pair? lst)))
      (error "map: expected list" lst))
  (if (null? lsts)
      ;; Single list case - works with proper lists
      ;; (circular single list would loop forever - that's expected behavior)
      (letrec ((loop (lambda (l)
                       (if (null? l)
                           '()
                           (cons (proc (car l))
                                 (loop (cdr l)))))))
        (loop lst))
      ;; Multiple lists case - stop at shortest (works with circular lists)
      (begin
        (for-each (lambda (l)
                    (if (and (not (null? l)) (not (pair? l)))
                        (error "map: expected list" l)))
                  lsts)
        (letrec ((any-null? (lambda (lists)
                              (if (null? lists)
                                  #f
                                  (or (null? (car lists))
                                      (any-null? (cdr lists))))))
                 (all-cars (lambda (lists)
                             (if (null? lists)
                                 '()
                                 (cons (caar lists) (all-cars (cdr lists))))))
                 (all-cdrs (lambda (lists)
                             (if (null? lists)
                                 '()
                                 (cons (cdar lists) (all-cdrs (cdr lists))))))
                 (loop (lambda (first-list rest-lists)
                         (if (or (null? first-list) (any-null? rest-lists))
                             '()
                             (cons (apply proc (cons (car first-list) (all-cars rest-lists)))
                                   (loop (cdr first-list) (all-cdrs rest-lists)))))))
          (loop lst lsts)))))

;; /**
;;  * Applies a procedure to each element of one or more lists for side effects.
;;  * When multiple lists are given, iterates until the shortest is exhausted (R7RS).
;;  * Supports circular lists when used with multiple lists.
;;
;;  * @param {procedure} proc - The procedure to apply.
;;  * @param {list} lst - First list.
;;  * @param {...list} lsts - Additional lists (optional).
;;  * @returns {undefined} Unspecified.
;;  */
(define (for-each proc lst . lsts)
  (if (not (procedure? proc))
      (error "for-each: expected procedure" proc))
  ;; Check first arg - allow circular lists (pair? but not list?)
  (if (and (not (null? lst)) (not (pair? lst)))
      (error "for-each: expected list" lst))
  (if (null? lsts)
      ;; Single list case
      (letrec ((loop (lambda (l)
                       (if (not (null? l))
                           (begin
                             (proc (car l))
                             (loop (cdr l)))))))
        (loop lst))
      ;; Multiple lists case - stop at shortest (works with circular lists)
      (letrec ((any-null? (lambda (lists)
                            (if (null? lists)
                                #f
                                (or (null? (car lists))
                                    (any-null? (cdr lists))))))
               (all-cars (lambda (lists)
                           (if (null? lists)
                               '()
                               (cons (caar lists) (all-cars (cdr lists))))))
               (all-cdrs (lambda (lists)
                           (if (null? lists)
                               '()
                               (cons (cdar lists) (all-cdrs (cdr lists))))))
               (loop (lambda (first-list rest-lists)
                       (if (not (or (null? first-list) (any-null? rest-lists)))
                           (begin
                             (apply proc (cons (car first-list) (all-cars rest-lists)))
                             (loop (cdr first-list) (all-cdrs rest-lists)))))))
        (loop lst lsts))))

;; /**
;;  * Applies a procedure to corresponding characters of strings.
;;  * Returns a new string of the results. When multiple strings are given,
;;  * the result length is the same as the shortest string (R7RS).
;;
;;  * @param {procedure} proc - The procedure to apply.
;;  * @param {string} str - First string.
;;  * @param {...string} strs - Additional strings (optional).
;;  * @returns {string} A new string of the results.
;;  */
(define (string-map proc str . strs)
  (if (not (procedure? proc))
      (error "string-map: expected procedure" proc))
  (if (not (string? str))
      (error "string-map: expected string" str))
  ;; Calculate minimum length across all strings
  (let ((len (let loop ((min-len (string-length str)) (ss strs))
               (if (null? ss)
                   min-len
                   (begin
                     (if (not (string? (car ss)))
                         (error "string-map: expected string" (car ss)))
                     (loop (min min-len (string-length (car ss))) (cdr ss)))))))
    (if (null? strs)
        ;; Single string case
        (list->string
         (letrec ((loop (lambda (i acc)
                          (if (< i 0)
                              acc
                              (loop (- i 1) (cons (proc (string-ref str i)) acc))))))
           (loop (- len 1) '())))
        ;; Multiple strings case
        (list->string
         (letrec ((loop (lambda (i acc)
                          (if (< i 0)
                              acc
                              (let ((chars (cons (string-ref str i)
                                                 (map (lambda (s) (string-ref s i)) strs))))
                                (loop (- i 1) (cons (apply proc chars) acc)))))))
           (loop (- len 1) '()))))))

;; /**
;;  * Applies a procedure to corresponding characters of strings for side effects.
;;
;;  * @param {procedure} proc - The procedure to apply.
;;  * @param {string} str - First string.
;;  * @param {...string} strs - Additional strings (optional).
;;  * @returns {undefined} Unspecified.
;;  */
(define (string-for-each proc str . strs)
  (if (not (procedure? proc))
      (error "string-for-each: expected procedure" proc))
  (if (not (string? str))
      (error "string-for-each: expected string" str))
  (let ((len (string-length str)))
    (for-each (lambda (s)
                (if (not (string? s))
                    (error "string-for-each: expected string" s))
                (if (not (= (string-length s) len))
                    (error "string-for-each: strings must have same length")))
              strs)
    (if (null? strs)
        ;; Single string case
        (letrec ((loop (lambda (i)
                         (if (< i len)
                             (begin
                               (proc (string-ref str i))
                               (loop (+ i 1)))))))
          (loop 0))
        ;; Multiple strings case
        (letrec ((loop (lambda (i)
                         (if (< i len)
                             (let ((chars (cons (string-ref str i)
                                                (map (lambda (s) (string-ref s i)) strs))))
                               (apply proc chars)
                               (loop (+ i 1)))))))
          (loop 0)))))

;; /**
;;  * Applies a procedure to corresponding elements of vectors.
;;  * Returns a new vector of the results. When multiple vectors are given,
;;  * the result length is the same as the shortest vector (R7RS).
;;
;;  * @param {procedure} proc - The procedure to apply.
;;  * @param {vector} vec - First vector.
;;  * @param {...vector} vecs - Additional vectors (optional).
;;  * @returns {vector} A new vector of the results.
;;  */
(define (vector-map proc vec . vecs)
  (if (not (procedure? proc))
      (error "vector-map: expected procedure" proc))
  (if (not (vector? vec))
      (error "vector-map: expected vector" vec))
  ;; Calculate minimum length across all vectors
  (let ((len (let loop ((min-len (vector-length vec)) (vs vecs))
               (if (null? vs)
                   min-len
                   (begin
                     (if (not (vector? (car vs)))
                         (error "vector-map: expected vector" (car vs)))
                     (loop (min min-len (vector-length (car vs))) (cdr vs)))))))
    (if (null? vecs)
        ;; Single vector case
        (let ((result (make-vector len)))
          (letrec ((loop (lambda (i)
                           (if (< i len)
                               (begin
                                 (vector-set! result i (proc (vector-ref vec i)))
                                 (loop (+ i 1)))))))
            (loop 0))
          result)
        ;; Multiple vectors case
        (let ((result (make-vector len)))
          (letrec ((loop (lambda (i)
                           (if (< i len)
                               (let ((elems (cons (vector-ref vec i)
                                                  (map (lambda (v) (vector-ref v i)) vecs))))
                                 (vector-set! result i (apply proc elems))
                                 (loop (+ i 1)))))))
            (loop 0))
          result))))

;; /**
;;  * Applies a procedure to corresponding elements of vectors for side effects.
;;
;;  * @param {procedure} proc - The procedure to apply.
;;  * @param {vector} vec - First vector.
;;  * @param {...vector} vecs - Additional vectors (optional).
;;  * @returns {undefined} Unspecified.
;;  */
(define (vector-for-each proc vec . vecs)
  (if (not (procedure? proc))
      (error "vector-for-each: expected procedure" proc))
  (if (not (vector? vec))
      (error "vector-for-each: expected vector" vec))
  (let ((len (vector-length vec)))
    (for-each (lambda (v)
                (if (not (vector? v))
                    (error "vector-for-each: expected vector" v))
                (if (not (= (vector-length v) len))
                    (error "vector-for-each: vectors must have same length")))
              vecs)
    (if (null? vecs)
        ;; Single vector case
        (letrec ((loop (lambda (i)
                         (if (< i len)
                             (begin
                               (proc (vector-ref vec i))
                               (loop (+ i 1)))))))
          (loop 0))
        ;; Multiple vectors case
        (letrec ((loop (lambda (i)
                         (if (< i len)
                             (let ((elems (cons (vector-ref vec i)
                                                (map (lambda (v) (vector-ref v i)) vecs))))
                               (apply proc elems)
                               (loop (+ i 1)))))))
          (loop 0)))))

;; =============================================================================
;; Membership Procedures
;; =============================================================================

;; /**
;;  * Return the sublist of list whose car is eq? to obj.
;;  * Return #f if obj is not found.
;;
;;  * @param {any} obj - Object to find.
;;  * @param {list} lst - List to search.
;;  * @return {list|boolean} Sublist or #f.
;;  */
(define (memq obj lst)
  (if (not (list? lst))
      (error "memq: expected list" lst))
  (letrec ((loop (lambda (l)
                   (if (null? l)
                       #f
                       (if (eq? obj (car l))
                           l
                           (loop (cdr l)))))))
    (loop lst)))

;; /**
;;  * Return the sublist of list whose car is eqv? to obj.
;;  * Return #f if obj is not found.
;;
;;  * @param {any} obj - Object to find.
;;  * @param {list} lst - List to search.
;;  * @return {list|boolean} Sublist or #f.
;;  */
(define (memv obj lst)
  (if (not (list? lst))
      (error "memv: expected list" lst))
  (letrec ((loop (lambda (l)
                   (if (null? l)
                       #f
                       (if (eqv? obj (car l))
                           l
                           (loop (cdr l)))))))
    (loop lst)))

;; /**
;;  * Return the sublist of list whose car is equal? to obj.
;;  * Return #f if obj is not found.
;;  * Optionally takes a custom comparison procedure.
;;
;;  * @param {any} obj - Object to find.
;;  * @param {list} lst - List to search.
;;  * @param {procedure} [compare] - Optional comparison procedure (default equal?).
;;  * @return {list|boolean} Sublist or #f.
;;  */
(define (member obj lst . compare)
  (if (not (list? lst))
      (error "member: expected list" lst))
  (let ((cmp (if (null? compare) equal? (car compare))))
    (letrec ((loop (lambda (l)
                     (if (null? l)
                         #f
                         (if (cmp obj (car l))
                             l
                             (loop (cdr l)))))))
      (loop lst))))

;; =============================================================================
;; Association List Procedures
;; =============================================================================

;; /**
;;  * Return the first element of alist whose car is eq? to obj.
;;  * Return #f if not found.
;;
;;  * @param {any} obj - Key to find.
;;  * @param {list} alist - Association list to search.
;;  * @return {pair|boolean} Association pair or #f.
;;  */
(define (assq obj alist)
  (if (not (list? alist))
      (error "assq: expected list" alist))
  (letrec ((loop (lambda (l)
                   (if (null? l)
                       #f
                       (let ((pair (car l)))
                         (if (not (pair? pair))
                             (error "assq: expected pair in alist" pair))
                         (if (eq? obj (car pair))
                             pair
                             (loop (cdr l))))))))
    (loop alist)))

;; /**
;;  * Return the first element of alist whose car is eqv? to obj.
;;  * Return #f if not found.
;;
;;  * @param {any} obj - Key to find.
;;  * @param {list} alist - Association list to search.
;;  * @return {pair|boolean} Association pair or #f.
;;  */
(define (assv obj alist)
  (if (not (list? alist))
      (error "assv: expected list" alist))
  (letrec ((loop (lambda (l)
                   (if (null? l)
                       #f
                       (let ((pair (car l)))
                         (if (not (pair? pair))
                             (error "assv: expected pair in alist" pair))
                         (if (eqv? obj (car pair))
                             pair
                             (loop (cdr l))))))))
    (loop alist)))

;; /**
;;  * Return the first element of alist whose car is equal? to obj.
;;  * Return #f if not found. Optionally takes a custom comparison procedure.
;;
;;  * @param {any} obj - Key to find.
;;  * @param {list} alist - Association list to search.
;;  * @param {procedure} [compare] - Optional comparison procedure (default equal?).
;;  * @return {pair|boolean} Association pair or #f.
;;  */
(define (assoc obj alist . compare)
  (if (not (list? alist))
      (error "assoc: expected list" alist))
  (let ((cmp (if (null? compare) equal? (car compare))))
    (letrec ((loop (lambda (l)
                     (if (null? l)
                         #f
                         (let ((pair (car l)))
                           (if (not (pair? pair))
                               (error "assoc: expected pair in alist" pair))
                           (if (cmp obj (car pair))
                               pair
                               (loop (cdr l))))))))
      (loop alist))))

;; =============================================================================
;; List Accessors and Utilities
;; =============================================================================

;; /**
;;  * Returns the length of a proper list.
;;
;;  * @param {list} lst - A proper list.
;;  * @returns {integer} Length of the list.
;;  */
(define (length lst)
  (if (not (list? lst))
      (error "length: expected list" lst))
  (letrec ((loop (lambda (l count)
                   (if (null? l)
                       count
                       (loop (cdr l) (+ count 1))))))
    (loop lst 0)))

;; /**
;;  * Returns the kth element of a list (0-indexed).
;;
;;  * @param {pair} lst - A list.
;;  * @param {integer} k - Index.
;;  * @returns {*} The kth element.
;;  */
(define (list-ref lst k)
  (if (not (pair? lst))
      (error "list-ref: expected pair" lst))
  (if (not (integer? k))
      (error "list-ref: expected exact integer" k))
  (if (< k 0)
      (error "list-ref: index out of range" k))
  (letrec ((loop (lambda (l i)
                   (cond
                     ((null? l) (error "list-ref: index out of range" k))
                     ((not (pair? l)) (error "list-ref: improper list" l))
                     ((= i 0) (car l))
                     (else (loop (cdr l) (- i 1)))))))
    (loop lst k)))

;; /**
;;  * Returns the sublist starting at index k.
;;
;;  * @param {list} lst - A list.
;;  * @param {integer} k - Index.
;;  * @returns {list} The sublist.
;;  */
(define (list-tail lst k)
  (if (not (integer? k))
      (error "list-tail: expected exact integer" k))
  (if (< k 0)
      (error "list-tail: index out of range" k))
  (letrec ((loop (lambda (l i)
                   (cond
                     ((= i 0) l)
                     ((null? l) (error "list-tail: index out of range" k))
                     ((not (pair? l)) (error "list-tail: improper list" l))
                     (else (loop (cdr l) (- i 1)))))))
    (loop lst k)))

;; /**
;;  * Reverses a proper list.
;;
;;  * @param {list} lst - A proper list.
;;  * @returns {list} Reversed list.
;;  */
(define (reverse lst)
  (if (not (list? lst))
      (error "reverse: expected list" lst))
  (letrec ((loop (lambda (l result)
                   (if (null? l)
                       result
                       (loop (cdr l) (cons (car l) result))))))
    (loop lst '())))

;; /**
;;  * Creates a shallow copy of a list.
;;
;;  * @param {*} obj - An object (if pair, copies structure).
;;  * @returns {*} Copy of the object.
;;  */
(define (list-copy obj)
  (if (not (pair? obj))
      obj
      (cons (car obj) (list-copy (cdr obj)))))

;; /**
;;  * Creates a list of k elements, each initialized to fill (default is #f).
;;
;;  * @param {integer} k - The number of elements.
;;  * @param {*} fill - The value to fill (optional, default #f).
;;  * @returns {list} A new list of k elements.
;;  */
(define (make-list k . rest)
  (if (not (integer? k))
      (error "make-list: expected integer" k))
  (if (< k 0)
      (error "make-list: expected non-negative integer" k))
  (let ((fill (if (null? rest) #f (car rest))))
    (letrec ((loop (lambda (n)
                     (if (= n 0)
                         '()
                         (cons fill (loop (- n 1)))))))
      (loop k))))

;; /**
;;  * Stores obj in element k of list.
;;  * It is an error if k is not a valid index of list.
;;
;;  * @param {list} lst - A proper list.
;;  * @param {integer} k - The index.
;;  * @param {*} obj - The value to store.
;;  * @returns {unspecified}
;;  */
(define (list-set! lst k obj)
  (if (not (list? lst))
      (error "list-set!: expected list" lst))
  (if (not (integer? k))
      (error "list-set!: expected integer index" k))
  (if (or (< k 0) (>= k (length lst)))
      (error "list-set!: index out of range" k))
  (if (= k 0)
      (set-car! lst obj)
      (list-set! (cdr lst) (- k 1) obj)))

