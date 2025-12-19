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
;;  * Applies a function to every element of a list.
;;
;;  * @param {procedure} proc - The function to apply.
;;  * @param {list} lst - The list to iterate over.
;;  * @returns {list} A new list containing the results.
;;  */
(define (map proc lst)
  (if (not (procedure? proc))
      (error "map: expected procedure" proc))
  (if (not (list? lst))
      (error "map: expected list" lst))
  (letrec ((loop (lambda (l)
                   (if (null? l)
                       '()
                       (cons (proc (car l))
                             (loop (cdr l)))))))
    (loop lst)))

;; /**
;;  * Applies a procedure to each element of a list for side effects.
;;
;;  * @param {procedure} proc - The procedure to apply.
;;  * @param {list} lst - The list to iterate over.
;;  * @returns {undefined} Unspecified.
;;  */
(define (for-each proc lst)
  (if (not (procedure? proc))
      (error "for-each: expected procedure" proc))
  (if (not (list? lst))
      (error "for-each: expected list" lst))
  (letrec ((loop (lambda (l)
                   (if (not (null? l))
                       (begin
                         (proc (car l))
                         (loop (cdr l)))))))
    (loop lst)))

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
;;
;;  * @param {any} obj - Object to find.
;;  * @param {list} lst - List to search.
;;  * @return {list|boolean} Sublist or #f.
;;  */
(define (member obj lst)
  (if (not (list? lst))
      (error "member: expected list" lst))
  (letrec ((loop (lambda (l)
                   (if (null? l)
                       #f
                       (if (equal? obj (car l))
                           l
                           (loop (cdr l)))))))
    (loop lst)))

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
