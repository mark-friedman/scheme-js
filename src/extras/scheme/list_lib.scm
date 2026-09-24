;; SRFI 1: List library
;;
;; The procedures SRFI 1 adds to R7RS-small. Those R7RS already defines
;; compatibly are re-exported by 1.sld instead.
;;
;; Two choices apply throughout:
;;
;;   - Every traversal is iterative. A compiled procedure's non-tail recursion
;;     runs on the JavaScript stack, so recursing once per element would fail
;;     on a long list; results are built in reverse and reversed once.
;;   - A linear-update procedure -- `take!`, `filter!`, `append!` and the rest
;;     -- is the same as its pure counterpart. SRFI 1 allows it to destroy its
;;     argument but does not require it, and sharing one definition means each
;;     behaviour is written and tested once.
;;
;; Where several lists are given, a procedure stops at the end of the shortest,
;; so a circular list can accompany a finite one.

;; ---------------------------------------------------------------------------
;; Argument checking
;; ---------------------------------------------------------------------------

;; /**
;;  * Signals an error unless an argument is a procedure.
;;  * @param {string} who - The procedure checking, for the message.
;;  * @param {*} f - The argument.
;;  * @returns {unspecified}
;;  */
(define (check-procedure who f)
  (if (not (procedure? f))
      (error (string-append who ": expected a procedure") f)))

;; /**
;;  * Signals an error unless an argument is a non-negative exact integer.
;;  * @param {string} who - The procedure checking, for the message.
;;  * @param {*} k - The argument.
;;  * @returns {unspecified}
;;  */
(define (check-count who k)
  (if (not (and (exact-integer? k) (>= k 0)))
      (error (string-append who ": expected a non-negative exact integer") k)))

;; ---------------------------------------------------------------------------
;; Walking several lists at once
;; ---------------------------------------------------------------------------

;; /**
;;  * The first element of each list, or #f if any list has run out.
;;  * @param {list} lists - The lists.
;;  * @returns {list|boolean} The elements, or #f.
;;  */
(define (cars-of lists)
  (let loop ((lists lists) (acc '()))
    (cond ((null? lists) (reverse acc))
          ((pair? (car lists)) (loop (cdr lists) (cons (car (car lists)) acc)))
          (else #f))))

;; /**
;;  * The rest of each list.
;;  * @param {list} lists - The lists, none of them empty.
;;  * @returns {list} Their cdrs.
;;  */
(define (cdrs-of lists) (map cdr lists))

;; ---------------------------------------------------------------------------
;; Constructors
;; ---------------------------------------------------------------------------

;; /**
;;  * `cons` with its arguments swapped.
;;  * @param {*} d - The cdr.
;;  * @param {*} a - The car.
;;  * @returns {pair} (a . d).
;;  */
(define (xcons d a) (cons a d))

;; /**
;;  * Like `list`, except that the last argument is the tail of the result.
;;  * @param {*} first - The first argument.
;;  * @param {...*} rest - The rest, the last of them the tail.
;;  * @returns {*} The list.
;;  */
(define (cons* first . rest)
  (if (null? rest)
      first
      (let ((backwards (reverse (cons first rest))))
        (fold cons (car backwards) (cdr backwards)))))

;; /**
;;  * A list of n elements, the ith being `(init i)`.
;;  * @param {integer} n - The length.
;;  * @param {procedure} init - Maps an index to its element.
;;  * @returns {list} The list.
;;  */
(define (list-tabulate n init)
  (check-count "list-tabulate" n)
  (check-procedure "list-tabulate" init)
  (let loop ((i (- n 1)) (acc '()))
    (if (< i 0) acc (loop (- i 1) (cons (init i) acc)))))

;; /**
;;  * A circular list of the arguments.
;;  * @param {*} first - The first element.
;;  * @param {...*} rest - The others.
;;  * @returns {pair} The list, whose last pair points back at its first.
;;  */
(define (circular-list first . rest)
  (let ((l (cons first rest)))
    (set-cdr! (last-pair l) l)
    l))

;; /**
;;  * The numbers start, start+step, ... -- count of them.
;;  * @param {integer} count - How many.
;;  * @param {number} [start=0] - The first.
;;  * @param {number} [step=1] - The difference between successive numbers.
;;  * @returns {list} The numbers.
;;  */
(define (iota count . options)
  (check-count "iota" count)
  (let ((start (if (pair? options) (car options) 0))
        (step (if (and (pair? options) (pair? (cdr options))) (cadr options) 1)))
    (let loop ((i (- count 1)) (acc '()))
      (if (< i 0) acc (loop (- i 1) (cons (+ start (* i step)) acc))))))

;; ---------------------------------------------------------------------------
;; Predicates
;; ---------------------------------------------------------------------------

;; /**
;;  * Whether an object is a finite list ending in the empty list.
;;  * @param {*} x - The object.
;;  * @returns {boolean}
;;  */
(define (proper-list? x) (list? x))

;; /**
;;  * Whether an object is a circular list. A slow and a fast pointer meet
;;  * exactly when the list loops.
;;  * @param {*} x - The object.
;;  * @returns {boolean}
;;  */
(define (circular-list? x)
  (let loop ((slow x) (fast x))
    (and (pair? fast)
         (let ((fast (cdr fast)))
           (and (pair? fast)
                (let ((fast (cdr fast)) (slow (cdr slow)))
                  (or (eq? fast slow) (loop slow fast))))))))

;; /**
;;  * Whether an object is a finite list ending in something other than the
;;  * empty list. Anything that is not a pair counts, as a list of no elements.
;;  * @param {*} x - The object.
;;  * @returns {boolean}
;;  */
(define (dotted-list? x)
  (let loop ((slow x) (fast x))
    (cond ((not (pair? fast)) (not (null? fast)))
          ((not (pair? (cdr fast))) (not (null? (cdr fast))))
          (else (let ((fast (cddr fast)) (slow (cdr slow)))
                  (and (not (eq? fast slow)) (loop slow fast)))))))

;; /**
;;  * @param {*} x - The object.
;;  * @returns {boolean} Whether it is not a pair.
;;  */
(define (not-pair? x) (not (pair? x)))

;; /**
;;  * Whether a list is empty, for the end test of a traversal that should
;;  * reject anything that is not a list.
;;  * @param {list} l - A proper or circular list.
;;  * @returns {boolean}
;;  */
(define (null-list? l)
  (cond ((pair? l) #f)
        ((null? l) #t)
        (else (error "null-list?: expected a list" l))))

;; /**
;;  * Whether lists are equal, element by element, under a given equality: each
;;  * list compared with the next.
;;  * @param {procedure} elt= - The element equality.
;;  * @param {...list} lists - The lists.
;;  * @returns {boolean}
;;  */
(define (list= elt= . lists)
  (define (equal-pair? a b)
    (or (eq? a b)
        (let loop ((a a) (b b))
          (cond ((not (pair? a)) (not (pair? b)))
                ((not (pair? b)) #f)
                (else (and (elt= (car a) (car b)) (loop (cdr a) (cdr b))))))))
  (check-procedure "list=" elt=)
  (or (null? lists)
      (let loop ((a (car lists)) (rest (cdr lists)))
        (or (null? rest)
            (and (equal-pair? a (car rest)) (loop (car rest) (cdr rest)))))))

;; ---------------------------------------------------------------------------
;; Selectors
;; ---------------------------------------------------------------------------

(define (first x) (car x))
(define (second x) (cadr x))
(define (third x) (caddr x))
(define (fourth x) (cadddr x))
(define (fifth x) (list-ref x 4))
(define (sixth x) (list-ref x 5))
(define (seventh x) (list-ref x 6))
(define (eighth x) (list-ref x 7))
(define (ninth x) (list-ref x 8))
(define (tenth x) (list-ref x 9))

;; /**
;;  * A pair's car and cdr, as two values.
;;  * @param {pair} p - The pair.
;;  * @returns {values} Its car, then its cdr.
;;  */
(define (car+cdr p) (values (car p) (cdr p)))

;; /**
;;  * The first k elements of a list.
;;  * @param {list} l - The list, which may be dotted or circular.
;;  * @param {integer} k - How many.
;;  * @returns {list} A new list.
;;  */
(define (take l k)
  (check-count "take" k)
  (let loop ((l l) (k k) (acc '()))
    (cond ((= k 0) (reverse acc))
          ((pair? l) (loop (cdr l) (- k 1) (cons (car l) acc)))
          (else (error "take: list has fewer elements than requested" k)))))

;; /**
;;  * All but the first k elements of a list: its kth tail, shared.
;;  * @param {list} l - The list.
;;  * @param {integer} k - How many to skip.
;;  * @returns {*} The tail.
;;  */
(define (drop l k)
  (check-count "drop" k)
  (let loop ((l l) (k k))
    (cond ((= k 0) l)
          ((pair? l) (loop (cdr l) (- k 1)))
          (else (error "drop: list has fewer elements than requested" k)))))

;; /**
;;  * The last k elements of a finite list, shared: found by running a second
;;  * pointer k elements ahead to the end.
;;  * @param {list} l - The list, which may be dotted.
;;  * @param {integer} k - How many.
;;  * @returns {*} The tail.
;;  */
(define (take-right l k)
  (let loop ((lag l) (lead (drop l k)))
    (if (pair? lead) (loop (cdr lag) (cdr lead)) lag)))

;; /**
;;  * All but the last k elements of a finite list.
;;  * @param {list} l - The list, which may be dotted.
;;  * @param {integer} k - How many to leave off.
;;  * @returns {list} A new list.
;;  */
(define (drop-right l k)
  (let loop ((lag l) (lead (drop l k)) (acc '()))
    (if (pair? lead)
        (loop (cdr lag) (cdr lead) (cons (car lag) acc))
        (reverse acc))))

(define (take! l k) (take l k))
(define (drop-right! l k) (drop-right l k))

;; /**
;;  * A list split after its first k elements.
;;  * @param {list} l - The list.
;;  * @param {integer} k - Where to split.
;;  * @returns {values} The first k elements, then the rest.
;;  */
(define (split-at l k) (values (take l k) (drop l k)))
(define (split-at! l k) (split-at l k))

;; /**
;;  * The last pair of a non-empty finite list.
;;  * @param {pair} l - The list.
;;  * @returns {pair} Its last pair.
;;  */
(define (last-pair l)
  (if (not (pair? l)) (error "last-pair: expected a non-empty list" l))
  (let loop ((l l))
    (if (pair? (cdr l)) (loop (cdr l)) l)))

;; /**
;;  * The last element of a non-empty finite list.
;;  * @param {pair} l - The list.
;;  * @returns {*} Its last element.
;;  */
(define (last l)
  (if (not (pair? l)) (error "last: expected a non-empty list" l))
  (car (last-pair l)))

;; ---------------------------------------------------------------------------
;; Miscellaneous
;; ---------------------------------------------------------------------------

;; /**
;;  * The length of a list, or #f if it is circular.
;;  * @param {list} x - A proper or circular list.
;;  * @returns {integer|boolean}
;;  */
(define (length+ x)
  (let loop ((slow x) (fast x) (n 0))
    (if (not (pair? fast))
        n
        (let ((fast (cdr fast)) (n (+ n 1)))
          (if (not (pair? fast))
              n
              (let ((fast (cdr fast)) (slow (cdr slow)) (n (+ n 1)))
                (if (eq? fast slow) #f (loop slow fast n))))))))

;; /**
;;  * The lists in a list, appended.
;;  * @param {list} lists - The lists; the last may be any object.
;;  * @returns {*} The result.
;;  */
(define (concatenate lists) (reduce-right append '() lists))
(define (concatenate! lists) (concatenate lists))
(define (append! . lists) (concatenate lists))
(define (reverse! l) (reverse l))

;; /**
;;  * The elements of one list, reversed, in front of another.
;;  * @param {list} rev-head - The list to reverse.
;;  * @param {*} tail - What follows it.
;;  * @returns {*} The result.
;;  */
(define (append-reverse rev-head tail) (fold cons tail rev-head))
(define (append-reverse! rev-head tail) (append-reverse rev-head tail))

;; /**
;;  * The lists' elements grouped by position: a list of lists.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {list} The groups, as many as the shortest list has elements.
;;  */
(define (zip l . lists) (apply map list l lists))

(define (unzip1 l) (map car l))
(define (unzip2 l) (values (map car l) (map cadr l)))
(define (unzip3 l) (values (map car l) (map cadr l) (map caddr l)))
(define (unzip4 l) (values (map car l) (map cadr l) (map caddr l) (map cadddr l)))
(define (unzip5 l) (values (map car l) (map cadr l) (map caddr l) (map cadddr l) (map fifth l)))

;; /**
;;  * How many elements, or groups of elements taken in step, satisfy a
;;  * predicate.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {integer} The count.
;;  */
(define (count pred l . lists)
  (check-procedure "count" pred)
  (apply fold (lambda args
                (let ((n (last args)))
                  (if (apply pred (drop-right args 1)) (+ n 1) n)))
         0 l lists))

;; ---------------------------------------------------------------------------
;; Fold, unfold and map
;; ---------------------------------------------------------------------------

;; /**
;;  * Folds a procedure over lists from the left: `(kons elt ... acc)` for each
;;  * element, or group of elements taken in step, starting from `knil`.
;;  * @param {procedure} kons - Takes the elements and the accumulator.
;;  * @param {*} knil - The initial accumulator.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {*} The final accumulator.
;;  */
(define (fold kons knil l . lists)
  (check-procedure "fold" kons)
  (if (null? lists)
      (let loop ((l l) (acc knil))
        (if (pair? l) (loop (cdr l) (kons (car l) acc)) acc))
      (let loop ((ls (cons l lists)) (acc knil))
        (let ((cars (cars-of ls)))
          (if cars
              (loop (cdrs-of ls) (apply kons (append cars (list acc))))
              acc)))))

;; /**
;;  * Folds a procedure over lists from the right: `(kons elt ... acc)` from the
;;  * last element to the first.
;;  * @param {procedure} kons - Takes the elements and the accumulator.
;;  * @param {*} knil - The initial accumulator.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {*} The final accumulator.
;;  */
(define (fold-right kons knil l . lists)
  (check-procedure "fold-right" kons)
  (if (null? lists)
      (fold kons knil (reverse l))
      (fold (lambda (group acc) (apply kons (append group (list acc))))
            knil
            (reverse (apply map list l lists)))))

;; /**
;;  * `fold` over a list's successive tails rather than its elements. The next
;;  * tail is taken before `kons` is called, so `kons` may alter the one it has.
;;  * @param {procedure} kons - Takes the tails and the accumulator.
;;  * @param {*} knil - The initial accumulator.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {*} The final accumulator.
;;  */
(define (pair-fold kons knil l . lists)
  (check-procedure "pair-fold" kons)
  (let loop ((ls (cons l lists)) (acc knil))
    (if (cars-of ls)
        (let ((next (cdrs-of ls)))
          (loop next (apply kons (append ls (list acc)))))
        acc)))

;; /**
;;  * `fold-right` over a list's successive tails.
;;  * @param {procedure} kons - Takes the tails and the accumulator.
;;  * @param {*} knil - The initial accumulator.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {*} The final accumulator.
;;  */
(define (pair-fold-right kons knil l . lists)
  (check-procedure "pair-fold-right" kons)
  (let collect ((ls (cons l lists)) (tails '()))
    (if (cars-of ls)
        (collect (cdrs-of ls) (cons ls tails))
        (fold (lambda (group acc) (apply kons (append group (list acc)))) knil tails))))

;; /**
;;  * `fold` without an initial value: the first element starts the
;;  * accumulator, and an empty list gives `ridentity`.
;;  * @param {procedure} f - Takes an element and the accumulator.
;;  * @param {*} ridentity - The result for an empty list.
;;  * @param {list} l - The list.
;;  * @returns {*} The result.
;;  */
(define (reduce f ridentity l)
  (check-procedure "reduce" f)
  (if (pair? l) (fold f (car l) (cdr l)) ridentity))

;; /**
;;  * `fold-right` without an initial value: `(f x1 (f x2 ... (f xn-1 xn)))`.
;;  * @param {procedure} f - Takes an element and the accumulator.
;;  * @param {*} ridentity - The result for an empty list.
;;  * @param {list} l - The list.
;;  * @returns {*} The result.
;;  */
(define (reduce-right f ridentity l)
  (check-procedure "reduce-right" f)
  (if (pair? l)
      (let ((backwards (reverse l)))
        (fold f (car backwards) (cdr backwards)))
      ridentity))

;; /**
;;  * Builds a list from a seed: elements `(f seed)`, `(f (g seed))`, ... until
;;  * `(p seed)` holds, when the tail is `(tail-gen seed)`, or empty.
;;  * @param {procedure} p - Whether to stop.
;;  * @param {procedure} f - Maps a seed to an element.
;;  * @param {procedure} g - Maps a seed to the next.
;;  * @param {*} seed - The first seed.
;;  * @param {procedure} [tail-gen] - Maps the last seed to the tail.
;;  * @returns {list} The list.
;;  */
(define (unfold p f g seed . tail-gen)
  (let loop ((seed seed) (acc '()))
    (if (p seed)
        (append-reverse acc (if (pair? tail-gen) ((car tail-gen) seed) '()))
        (loop (g seed) (cons (f seed) acc)))))

;; /**
;;  * Builds a list from a seed, from the right: the first element produced is
;;  * the last in the list, and the list ends in `tail`.
;;  * @param {procedure} p - Whether to stop.
;;  * @param {procedure} f - Maps a seed to an element.
;;  * @param {procedure} g - Maps a seed to the next.
;;  * @param {*} seed - The first seed.
;;  * @param {*} [tail='()] - What the list ends in.
;;  * @returns {list} The list.
;;  */
(define (unfold-right p f g seed . tail)
  (let loop ((seed seed) (acc (if (pair? tail) (car tail) '())))
    (if (p seed) acc (loop (g seed) (cons (f seed) acc)))))

;; /**
;;  * Maps a list-returning procedure over lists and appends the results.
;;  * @param {procedure} f - Returns a list per element or group of elements.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {list} The results, appended.
;;  */
(define (append-map f l . lists)
  (check-procedure "append-map" f)
  (concatenate (apply map-in-order f l lists)))
(define (append-map! f l . lists) (apply append-map f l lists))

;; /**
;;  * Applies a procedure to each successive tail of lists, for its effect. The
;;  * next tail is taken first, so the procedure may alter the one it has.
;;  * @param {procedure} f - Takes the tails.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {unspecified}
;;  */
(define (pair-for-each f l . lists)
  (check-procedure "pair-for-each" f)
  (let loop ((ls (cons l lists)))
    (if (cars-of ls)
        (let ((next (cdrs-of ls)))
          (apply f ls)
          (loop next)))))

;; /**
;;  * `map`, keeping only the true results.
;;  * @param {procedure} f - The procedure.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {list} The true results, in order.
;;  */
(define (filter-map f l . lists)
  (check-procedure "filter-map" f)
  (filter (lambda (x) x) (apply map-in-order f l lists)))

;; /**
;;  * `map`, with the procedure applied to the elements in order, first to last.
;;  * R7RS leaves `map`'s order unspecified, which matters when the procedure has
;;  * effects.
;;  * @param {procedure} f - The procedure.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {list} The results.
;;  */
(define (map-in-order f l . lists)
  (check-procedure "map-in-order" f)
  (if (null? lists)
      (let loop ((l l) (acc '()))
        (if (pair? l)
            (let ((y (f (car l)))) (loop (cdr l) (cons y acc)))
            (reverse acc)))
      (let loop ((ls (cons l lists)) (acc '()))
        (let ((cars (cars-of ls)))
          (if cars
              (let ((y (apply f cars))) (loop (cdrs-of ls) (cons y acc)))
              (reverse acc))))))

(define (map! f l . lists) (apply map f l lists))

;; ---------------------------------------------------------------------------
;; Filtering and partitioning
;; ---------------------------------------------------------------------------

;; /**
;;  * The elements of a list that satisfy a predicate, in order.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - The list.
;;  * @returns {list} The elements kept.
;;  */
(define (filter pred l)
  (check-procedure "filter" pred)
  (let loop ((l l) (acc '()))
    (cond ((not (pair? l)) (reverse acc))
          ((pred (car l)) (loop (cdr l) (cons (car l) acc)))
          (else (loop (cdr l) acc)))))

;; /**
;;  * The elements of a list that do not satisfy a predicate, in order.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - The list.
;;  * @returns {list} The elements left.
;;  */
(define (remove pred l)
  (check-procedure "remove" pred)
  (filter (lambda (x) (not (pred x))) l))

;; /**
;;  * A list divided by a predicate.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - The list.
;;  * @returns {values} The elements that satisfy it, then those that do not.
;;  */
(define (partition pred l)
  (check-procedure "partition" pred)
  (let loop ((l l) (in '()) (out '()))
    (cond ((not (pair? l)) (values (reverse in) (reverse out)))
          ((pred (car l)) (loop (cdr l) (cons (car l) in) out))
          (else (loop (cdr l) in (cons (car l) out))))))

(define (filter! pred l) (filter pred l))
(define (remove! pred l) (remove pred l))
(define (partition! pred l) (partition pred l))

;; ---------------------------------------------------------------------------
;; Searching
;; ---------------------------------------------------------------------------

;; /**
;;  * The first tail of a list whose car satisfies a predicate.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - The list.
;;  * @returns {pair|boolean} The tail, or #f.
;;  */
(define (find-tail pred l)
  (check-procedure "find-tail" pred)
  (let loop ((l l))
    (and (pair? l)
         (if (pred (car l)) l (loop (cdr l))))))

;; /**
;;  * The first element of a list that satisfies a predicate.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - The list.
;;  * @returns {*} The element, or #f.
;;  */
(define (find pred l)
  (let ((tail (find-tail pred l)))
    (and tail (car tail))))

;; /**
;;  * The first true result of a predicate over the elements of lists, or of
;;  * groups of elements taken in step. The last call is a tail call.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {*} The first true result, or #f.
;;  */
(define (any pred l . lists)
  (check-procedure "any" pred)
  (if (null? lists)
      (and (pair? l)
           (let loop ((l l))
             (if (pair? (cdr l))
                 (or (pred (car l)) (loop (cdr l)))
                 (pred (car l)))))
      (let loop ((ls (cons l lists)))
        (let ((cars (cars-of ls)))
          (and cars
               (let ((next (cdrs-of ls)))
                 (if (cars-of next)
                     (or (apply pred cars) (loop next))
                     (apply pred cars))))))))

;; /**
;;  * Whether a predicate holds for every element of lists, or group taken in
;;  * step: #f at the first failure, else the last result, or #t if there were
;;  * none. The last call is a tail call.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {*} The result.
;;  */
(define (every pred l . lists)
  (check-procedure "every" pred)
  (if (null? lists)
      (or (not (pair? l))
          (let loop ((l l))
            (if (pair? (cdr l))
                (and (pred (car l)) (loop (cdr l)))
                (pred (car l)))))
      (let loop ((ls (cons l lists)))
        (let ((cars (cars-of ls)))
          (or (not cars)
              (let ((next (cdrs-of ls)))
                (if (cars-of next)
                    (and (apply pred cars) (loop next))
                    (apply pred cars))))))))

;; /**
;;  * The index of the first element, or group taken in step, that satisfies a
;;  * predicate.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - A list.
;;  * @param {...list} lists - More lists.
;;  * @returns {integer|boolean} The index, or #f.
;;  */
(define (list-index pred l . lists)
  (check-procedure "list-index" pred)
  (let loop ((ls (cons l lists)) (i 0))
    (let ((cars (cars-of ls)))
      (and cars
           (if (apply pred cars) i (loop (cdrs-of ls) (+ i 1)))))))

;; /**
;;  * The longest prefix of a list whose elements satisfy a predicate.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - The list.
;;  * @returns {list} A new list.
;;  */
(define (take-while pred l)
  (check-procedure "take-while" pred)
  (let loop ((l l) (acc '()))
    (if (and (pair? l) (pred (car l)))
        (loop (cdr l) (cons (car l) acc))
        (reverse acc))))

;; /**
;;  * What is left of a list after its longest prefix satisfying a predicate.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - The list.
;;  * @returns {list} The tail, shared.
;;  */
(define (drop-while pred l)
  (check-procedure "drop-while" pred)
  (let loop ((l l))
    (if (and (pair? l) (pred (car l))) (loop (cdr l)) l)))

(define (take-while! pred l) (take-while pred l))

;; /**
;;  * A list split where a predicate first fails.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - The list.
;;  * @returns {values} The longest prefix satisfying it, then the rest.
;;  */
(define (span pred l) (values (take-while pred l) (drop-while pred l)))

;; /**
;;  * A list split where a predicate first holds.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} l - The list.
;;  * @returns {values} The longest prefix not satisfying it, then the rest.
;;  */
(define (break pred l)
  (check-procedure "break" pred)
  (span (lambda (x) (not (pred x))) l))

(define (span! pred l) (span pred l))
(define (break! pred l) (break pred l))

;; ---------------------------------------------------------------------------
;; Deleting
;; ---------------------------------------------------------------------------

;; /**
;;  * A list without the elements equal to x: each y for which `(= x y)`.
;;  * @param {*} x - The value to delete.
;;  * @param {list} l - The list.
;;  * @param {procedure} [=] - The equality, `equal?` by default.
;;  * @returns {list} The rest, in order.
;;  */
(define (delete x l . maybe=)
  (let ((same? (if (pair? maybe=) (car maybe=) equal?)))
    (filter (lambda (y) (not (same? x y))) l)))
(define (delete! x l . maybe=) (apply delete x l maybe=))

;; /**
;;  * A list with later duplicates removed, keeping the first of each in order.
;;  * Each comparison is `(= earlier later)`.
;;  * @param {list} l - The list.
;;  * @param {procedure} [=] - The equality, `equal?` by default.
;;  * @returns {list} The distinct elements.
;;  */
(define (delete-duplicates l . maybe=)
  (let ((same? (if (pair? maybe=) (car maybe=) equal?)))
    (let loop ((l l) (kept '()))
      (cond ((not (pair? l)) (reverse kept))
            ((any (lambda (k) (same? k (car l))) kept) (loop (cdr l) kept))
            (else (loop (cdr l) (cons (car l) kept)))))))
(define (delete-duplicates! l . maybe=) (apply delete-duplicates l maybe=))

;; ---------------------------------------------------------------------------
;; Association lists
;; ---------------------------------------------------------------------------

;; /**
;;  * An association list with one more association at its front.
;;  * @param {*} key - The key.
;;  * @param {*} value - The value.
;;  * @param {list} alist - The association list.
;;  * @returns {list} The new association list.
;;  */
(define (alist-cons key value alist) (cons (cons key value) alist))

;; /**
;;  * A copy of an association list, each association copied too.
;;  * @param {list} alist - The association list.
;;  * @returns {list} The copy.
;;  */
(define (alist-copy alist) (map (lambda (a) (cons (car a) (cdr a))) alist))

;; /**
;;  * An association list without the associations whose key is equal to a
;;  * given one: each for which `(= key (car association))`.
;;  * @param {*} key - The key.
;;  * @param {list} alist - The association list.
;;  * @param {procedure} [=] - The equality, `equal?` by default.
;;  * @returns {list} The rest, in order.
;;  */
(define (alist-delete key alist . maybe=)
  (let ((same? (if (pair? maybe=) (car maybe=) equal?)))
    (remove (lambda (a) (same? key (car a))) alist)))
(define (alist-delete! key alist . maybe=) (apply alist-delete key alist maybe=))

;; ---------------------------------------------------------------------------
;; Lists as sets
;; ---------------------------------------------------------------------------
;;
;; A set is a list, compared under a given equality. The order of a result is
;; not specified by SRFI 1; these follow its reference implementation, so an
;; element added to a set goes on the front.

;; /**
;;  * Whether each list is a subset of the next.
;;  * @param {procedure} = - The element equality.
;;  * @param {...list} lists - The lists.
;;  * @returns {boolean}
;;  */
(define (lset<= = . lists)
  (check-procedure "lset<=" =)
  (or (null? lists)
      (let loop ((a (car lists)) (rest (cdr lists)))
        (or (null? rest)
            (and (or (eq? a (car rest))
                     (every (lambda (x) (member x (car rest) =)) a))
                 (loop (car rest) (cdr rest)))))))

;; /**
;;  * Whether each list is the same set as the next.
;;  * @param {procedure} = - The element equality.
;;  * @param {...list} lists - The lists.
;;  * @returns {boolean}
;;  */
(define (lset= = . lists)
  (check-procedure "lset=" =)
  (or (null? lists)
      (let loop ((a (car lists)) (rest (cdr lists)))
        (or (null? rest)
            (and (or (eq? a (car rest))
                     (and (lset<= = a (car rest)) (lset<= = (car rest) a)))
                 (loop (car rest) (cdr rest)))))))

;; /**
;;  * A set with elements added, each only if it is not already a member.
;;  * @param {procedure} = - The element equality.
;;  * @param {list} l - The set.
;;  * @param {...*} elts - The elements to add.
;;  * @returns {list} The set.
;;  */
(define (lset-adjoin = l . elts)
  (check-procedure "lset-adjoin" =)
  (fold (lambda (elt set) (if (member elt set =) set (cons elt set))) l elts))

;; /**
;;  * The union of sets: the first, with each element of the others not already
;;  * present added.
;;  * @param {procedure} = - The element equality.
;;  * @param {...list} lists - The sets.
;;  * @returns {list} The union.
;;  */
(define (lset-union = . lists)
  (check-procedure "lset-union" =)
  (reduce (lambda (l set)
            (cond ((null? l) set)
                  ((null? set) l)
                  ((eq? l set) set)
                  (else (fold (lambda (elt set)
                                (if (any (lambda (x) (= x elt)) set) set (cons elt set)))
                              set l))))
          '()
          lists))

;; /**
;;  * The intersection of sets: the elements of the first that are in all the
;;  * others.
;;  * @param {procedure} = - The element equality.
;;  * @param {list} l - The first set.
;;  * @param {...list} lists - The others.
;;  * @returns {list} The intersection.
;;  */
(define (lset-intersection = l . lists)
  (check-procedure "lset-intersection" =)
  (let ((others (remove (lambda (x) (eq? x l)) lists)))
    (if (any null? others)
        '()
        (filter (lambda (x) (every (lambda (other) (member x other =)) others)) l))))

;; /**
;;  * The difference of sets: the elements of the first that are in none of the
;;  * others.
;;  * @param {procedure} = - The element equality.
;;  * @param {list} l - The first set.
;;  * @param {...list} lists - The others.
;;  * @returns {list} The difference.
;;  */
(define (lset-difference = l . lists)
  (check-procedure "lset-difference" =)
  (let ((others (filter pair? lists)))
    (if (any (lambda (x) (eq? x l)) others)
        '()
        (filter (lambda (x) (every (lambda (other) (not (member x other =))) others)) l))))

;; /**
;;  * The exclusive or of sets: the elements in an odd number of them.
;;  * @param {procedure} = - The element equality.
;;  * @param {...list} lists - The sets.
;;  * @returns {list} The result.
;;  */
(define (lset-xor = . lists)
  (check-procedure "lset-xor" =)
  (reduce (lambda (b a)
            (call-with-values
              (lambda () (lset-diff+intersection = a b))
              (lambda (a-b a*b)
                (cond ((null? a-b) (lset-difference = b a))
                      ((null? a*b) (append b a))
                      (else (fold (lambda (x set) (if (member x a*b =) set (cons x set)))
                                  a-b b))))))
          '()
          lists))

;; /**
;;  * The difference and the intersection of sets at once: the elements of the
;;  * first that are in none of the others, and those in at least one.
;;  * @param {procedure} = - The element equality.
;;  * @param {list} l - The first set.
;;  * @param {...list} lists - The others.
;;  * @returns {values} The difference, then the intersection.
;;  */
(define (lset-diff+intersection = l . lists)
  (check-procedure "lset-diff+intersection" =)
  (call-with-values
    (lambda () (partition (lambda (x) (any (lambda (other) (member x other =)) lists)) l))
    (lambda (in out) (values out in))))

(define (lset-union! = . lists) (apply lset-union = lists))
(define (lset-intersection! = l . lists) (apply lset-intersection = l lists))
(define (lset-difference! = l . lists) (apply lset-difference = l lists))
(define (lset-xor! = . lists) (apply lset-xor = lists))
(define (lset-diff+intersection! = l . lists) (apply lset-diff+intersection = l lists))
