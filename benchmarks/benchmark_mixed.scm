;; benchmark_mixed.scm
;; Mixed workload benchmarks - non-numeric operations for baseline comparison

;; Test 10: List processing (non-numeric, control flow)
(define (reverse-many n)
  (let loop ((i 0) (lst '(1 2 3 4 5)))
    (if (>= i n)
        lst
        (loop (+ i 1) (reverse lst)))))

(define (time-list-processing)
  (let ((start (current-jiffy)))
    (reverse-many 100000)
    (- (current-jiffy) start)))

;; Helper: build a list 0..n-1
(define (build-list n)
  (let loop ((i (- n 1)) (acc '()))
    (if (< i 0)
        acc
        (loop (- i 1) (cons i acc)))))

;; Test 11: Map/fold chain (avoid filter which may not be loaded)
(define (map-fold-chain n)
  (let ((lst (build-list n)))
    ;; Count even numbers after doubling, for numbers > 10
    (let loop ((lst lst) (count 0))
      (if (null? lst)
          count
          (let ((x (car lst)))
            (if (> x 10)
                (loop (cdr lst) (+ count (if (even? (* x 2)) 1 0)))
                (loop (cdr lst) count)))))))

(define (time-map-filter)
  (let ((start (current-jiffy)))
    (map-fold-chain 10000)
    (- (current-jiffy) start)))

;; Test 12: Append-heavy workload
(define (append-loop n)
  (let loop ((i 0) (acc '()))
    (if (>= i n)
        (length acc)
        (loop (+ i 1) (append acc (list i))))))

(define (time-append-loop)
  (let ((start (current-jiffy)))
    (append-loop 1000)  ; O(n^2) so keep small
    (- (current-jiffy) start)))

;; Test 13: Symbol comparison heavy
(define (symbol-lookup n)
  (let ((syms '(foo bar baz qux quux corge grault garply)))
    (let loop ((i 0) (count 0))
      (if (>= i n)
          count
          (loop (+ i 1)
                (if (memq 'qux syms) (+ count 1) count))))))

(define (time-symbol-lookup)
  (let ((start (current-jiffy)))
    (symbol-lookup 100000)
    (- (current-jiffy) start)))
