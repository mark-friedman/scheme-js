;; kernels.scm -- the programs `benchmarks/run_evaluator.js` runs on the
;; interpreter, as it is, and on an evaluator written in Scheme to the same
;; design (`benchmarks/run_evaluator.scm`), to measure what the evaluator
;; would cost written in Scheme and compiled (task 68's ceiling).
;;
;; Each kernel uses only what both evaluators run: literals, variables, `if`,
;; `begin`, `lambda`, `let` and named `let`, `set!`, `define` and calls --
;; of procedures they define and of the system's primitives. `kernels` lists
;; each with its label, a thunk running it, and the answer it must give.

(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(define (tak x y z)
  (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))

(define (count-to n)
  (let loop ((i 0) (sum 0))
    (if (< i n) (loop (+ i 1) (+ sum i)) sum)))

(define (build n)
  (if (= n 0) '() (cons n (build (- n 1)))))

(define (len l)
  (if (null? l) 0 (+ 1 (len (cdr l)))))

(define (lists n)
  (let loop ((i 0) (total 0))
    (if (< i n) (loop (+ i 1) (+ total (len (build 100)))) total)))

(define (lets n)
  (let loop ((i 0) (acc 0))
    (if (< i n)
        (let ((a (* i 2)))
          (let ((b (+ a 1)))
            (loop (+ i 1) (+ acc (- b a)))))
        acc)))

(define (make-counter)
  (let ((count 0))
    (lambda () (set! count (+ count 1)) count)))

(define (closures n)
  (let ((counter (make-counter)))
    (let loop ((i 0))
      (if (< i n) (begin (counter) (loop (+ i 1))) (counter)))))

(define kernels
  (list (list "fib 20: calls" (lambda () (fib 20)) 6765)
        (list "tak 18 12 6: calls" (lambda () (tak 18 12 6)) 7)
        (list "a named let of 100,000" (lambda () (count-to 100000)) 4999950000)
        (list "a list of 100 built and walked, 300 times" (lambda () (lists 300)) 30000)
        (list "two lets in a loop of 100,000" (lambda () (lets 100000)) 100000)
        (list "a closure that assigns, 100,000 calls" (lambda () (closures 100000)) 100001)))
