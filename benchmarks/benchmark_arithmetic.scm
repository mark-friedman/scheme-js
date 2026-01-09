;; benchmark_arithmetic.scm
;; Pure Scheme arithmetic benchmarks for comparing BigInt vs Number performance

;; Test 1: Small integer arithmetic (common case)
;; Sums 0 + 1 + 2 + ... + n
(define (sum-to n)
  (let loop ((i 0) (acc 0))
    (if (> i n)
        acc
        (loop (+ i 1) (+ acc i)))))

(define (time-sum-to)
  (let ((start (current-jiffy)))
    (sum-to 1000000)
    (- (current-jiffy) start)))

;; Test 2: Factorial - tests large integer promotion
;; factorial(100) is a 158-digit number - exceeds Number.MAX_SAFE_INTEGER
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define (time-factorial)
  (let ((start (current-jiffy)))
    ;; Run 1000 iterations to get measurable time
    (let loop ((i 0))
      (if (< i 1000)
          (begin
            (factorial 100)
            (loop (+ i 1)))))
    (- (current-jiffy) start)))

;; Test 3: Fibonacci - tests comparison + addition in recursive calls
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (time-fib)
  (let ((start (current-jiffy)))
    (fib 30)
    (- (current-jiffy) start)))

;; Test 4: Division chain (rational creation in BigInt branch)
(define (division-chain n)
  (let loop ((i 1) (acc 1))
    (if (> i n)
        acc
        (loop (+ i 1) (/ acc (+ i 1))))))

(define (time-division)
  (let ((start (current-jiffy)))
    (division-chain 1000)
    (- (current-jiffy) start)))

;; Test 5: Tight numeric loop with modulo
(define (collatz-steps n)
  (let loop ((n n) (steps 0))
    (cond
      ((= n 1) steps)
      ((even? n) (loop (quotient n 2) (+ steps 1)))
      (else (loop (+ (* 3 n) 1) (+ steps 1))))))

(define (time-collatz)
  (let ((start (current-jiffy)))
    ;; Sum collatz steps for first 10000 numbers
    (let loop ((i 1) (total 0))
      (if (> i 10000)
          total
          (loop (+ i 1) (+ total (collatz-steps i)))))
    (- (current-jiffy) start)))
