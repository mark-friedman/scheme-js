;; nqueens.scm -- count solutions to the n-queens problem.
;;
;; Allocation-heavy: builds and discards many short lists, so it exercises the
;; garbage collector and cons-cell representation rather than raw call speed.
;; Together with fib (calls) and oddeven (tail calls) this covers the three
;; non-continuation cost centres.
;;
;; Canonical size 12, matching Thivierge & Feeley `nqueens12`.

;; /**
;;  * Appends two lists (non-tail-recursive, deliberately).
;;  * @param {list} lst1 - First list.
;;  * @param {list} lst2 - Second list.
;;  * @returns {list} The concatenation.
;;  */
(define (app lst1 lst2)
  (if (pair? lst1)
      (cons (car lst1) (app (cdr lst1) lst2))
      lst2))

;; /**
;;  * Builds the list (1 2 ... n).
;;  * @param {exact-integer} n - Upper bound.
;;  * @returns {list} Ascending list of integers from 1 to n.
;;  */
(define (one-up-to n)
  (let loop ((i n) (lst '()))
    (if (= i 0)
        lst
        (loop (- i 1) (cons i lst)))))

;; /**
;;  * Tests whether a queen at `row` conflicts with any already-placed queen.
;;  * @param {exact-integer} row - Candidate row.
;;  * @param {exact-integer} dist - Column distance to the nearest placed queen.
;;  * @param {list} placed - Rows of queens already placed.
;;  * @returns {boolean} #t if the placement is safe.
;;  */
(define (ok? row dist placed)
  (if (pair? placed)
      (and (not (= (car placed) (+ row dist)))
           (not (= (car placed) (- row dist)))
           (ok? row (+ dist 1) (cdr placed)))
      #t))

;; /**
;;  * Explores the placement search tree, counting complete solutions.
;;  * @param {list} x - Rows still available in this column.
;;  * @param {list} y - Rows deferred to later columns.
;;  * @param {list} placed - Rows of queens already placed.
;;  * @returns {exact-integer} Number of solutions in this subtree.
;;  */
(define (explore x y placed)
  (if (pair? x)
      (+ (if (ok? (car x) 1 placed)
             (explore (app (cdr x) y)
                      '()
                      (cons (car x) placed))
             0)
         (explore (cdr x)
                  (cons (car x) y)
                  placed))
      (if (pair? y) 0 1)))

;; /**
;;  * Counts the solutions to the n-queens problem.
;;  * @param {exact-integer} n - Board size.
;;  * @returns {exact-integer} Number of distinct solutions.
;;  */
(define (nqueens n)
  (explore (one-up-to n) '() '()))

(define (bench-run) (nqueens bench-size))
