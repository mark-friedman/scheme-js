;; Equality Procedures
;; Structural equality testing
;;
;; R7RS 6.1: equal? compares pairs, vectors, strings and bytevectors by their
;; unfoldings into (possibly infinite) trees, and must terminate even when
;; its arguments are circular. Comparing as trees is the fast way, and does
;; not terminate on a cycle, so it is tried first within a budget of pairs and
;; vectors; arguments larger than the budget, which are the only ones that
;; can be circular, are compared again as graphs. This is the scheme of
;; Adams and Dybvig, "Efficient Nondestructive Equality Checking for Trees
;; and Graphs" (ICFP 2008), without its interleaving.

;; /**
;;  * How many pairs and vectors equal? compares as trees before it compares
;;  * as graphs.
;;  * @type {integer}
;;  */
(define equal-tree-budget 1000)

;; /**
;;  * Deep equality check.
;;  * Recursively compares pairs, vectors, strings and bytevectors; uses eqv?
;;  * for other types. Terminates on circular arguments.
;;  *
;;  * @param {*} a - First object.
;;  * @param {*} b - Second object.
;;  * @returns {boolean} #t if objects are structurally equal, #f otherwise.
;;  */
(define (equal? a b)
  (let ((left (equal-as-trees a b equal-tree-budget)))
    (cond ((not left) #f)
          ((< left 0) (equal-as-graphs a b))
          (else #t))))

;; /**
;;  * Whether two objects differ in a way equal? sees without looking inside
;;  * them: as leaves, or as aggregates of different kinds or sizes.
;;  * Strings are compared by characters, since a string that may be changed
;;  * is an object, which eqv? would compare by identity.
;;  *
;;  * @param {*} a - First object.
;;  * @param {*} b - Second object.
;;  * @returns {symbol} same if eqv? or equal leaves, differ if not equal,
;;  *   pair or vector if both are one, of the same length for vectors.
;;  */
(define (equal-compare-shallow a b)
  (cond ((eqv? a b) 'same)
        ((and (pair? a) (pair? b)) 'pair)
        ((and (vector? a) (vector? b))
         (if (= (vector-length a) (vector-length b)) 'vector 'differ))
        ((and (string? a) (string? b))
         (if (string=? a b) 'same 'differ))
        ((and (bytevector? a) (bytevector? b))
         (if (equal-bytevectors? a b) 'same 'differ))
        (else 'differ)))

;; /**
;;  * Whether two bytevectors hold the same bytes.
;;  * @param {bytevector} a - First bytevector.
;;  * @param {bytevector} b - Second bytevector.
;;  * @returns {boolean}
;;  */
(define (equal-bytevectors? a b)
  (let ((len (bytevector-length a)))
    (and (= len (bytevector-length b))
         (let loop ((i 0))
           (or (= i len)
               (and (= (bytevector-u8-ref a i) (bytevector-u8-ref b i))
                    (loop (+ i 1))))))))

;; /**
;;  * Compares two objects as trees, spending one of `k` on each pair and
;;  * vector compared.
;;  *
;;  * @param {*} a - First object.
;;  * @param {*} b - Second object.
;;  * @param {integer} k - What is left of the budget.
;;  * @returns {integer|boolean} #f if they differ; otherwise what is left of
;;  *   the budget, which is negative if it ran out before they were compared.
;;  */
(define (equal-as-trees a b k)
  (if (< k 0)
      k
      (let ((kind (equal-compare-shallow a b)))
        (cond ((eq? kind 'same) k)
              ((eq? kind 'pair)
               (let ((k (equal-as-trees (car a) (car b) (- k 1))))
                 (if (and k (>= k 0))
                     (equal-as-trees (cdr a) (cdr b) k)
                     k)))
              ((eq? kind 'vector)
               (let ((len (vector-length a)))
                 (let loop ((i 0) (k (- k 1)))
                   (if (or (= i len) (not k) (< k 0))
                       k
                       (loop (+ i 1) (equal-as-trees (vector-ref a i) (vector-ref b i) k))))))
              (else #f)))))

;; /**
;;  * Compares two objects as graphs: by bisimulation, so that a cycle is
;;  * compared once around. Each pair or vector is put in a class with those
;;  * it has been assumed equal to, kept as a union-find forest over an eq?
;;  * store; a pair of objects already in one class is equal, as far as this
;;  * comparison can tell, and the comparison holds if no assumption is
;;  * contradicted.
;;  *
;;  * @param {*} a - First object.
;;  * @param {*} b - Second object.
;;  * @returns {boolean}
;;  */
(define (equal-as-graphs a b)
  (let ((classes (%make-hash-store 'eq)))
    ;; An object's class is a pair whose car is the class it was merged into,
    ;; or #f while it is a class's root.
    (define (class-of x)
      (or (%hash-store-ref classes x #f)
          (let ((class (cons #f '())))
            (%hash-store-set! classes x class)
            class)))
    (define (root class)
      (let ((up (car class)))
        (if up
            (let ((top (root up)))
              (set-car! class top)
              top)
            class)))
    ;; Whether x and y were already assumed equal; they are from now on.
    (define (assumed-equal! x y)
      (let ((rx (root (class-of x)))
            (ry (root (class-of y))))
        (or (eq? rx ry)
            (begin (set-car! rx ry) #f))))
    (let compare ((a a) (b b))
      (let ((kind (equal-compare-shallow a b)))
        (cond ((eq? kind 'same) #t)
              ((eq? kind 'pair)
               (or (assumed-equal! a b)
                   (and (compare (car a) (car b))
                        (compare (cdr a) (cdr b)))))
              ((eq? kind 'vector)
               (or (assumed-equal! a b)
                   (let ((len (vector-length a)))
                     (let loop ((i 0))
                       (or (= i len)
                           (and (compare (vector-ref a i) (vector-ref b i))
                                (loop (+ i 1))))))))
              (else #f))))))
