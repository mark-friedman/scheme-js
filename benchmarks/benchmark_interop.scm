;; benchmark_interop.scm
;; JS Interop benchmarks for comparing BigInt vs Number boundary crossing costs

;; Test 6: Repeated JS calls with numeric args
(define (js-call-loop n)
  (let loop ((i 0))
    (when (< i n)
      (js-eval "1 + 1")  ; Minimal JS work, measures call overhead
      (loop (+ i 1)))))

(define (time-js-calls)
  (let ((start (current-jiffy)))
    (js-call-loop 10000)
    (- (current-jiffy) start)))

;; Test 7: Pass numbers to JS and back
(define (js-roundtrip n)
  (let loop ((i 0) (sum 0))
    (if (>= i n)
        sum
        (let ((result (js-eval (string-append "(" (number->string i) " * 2)"))))
          (loop (+ i 1) (+ sum result))))))

(define (time-roundtrip)
  (let ((start (current-jiffy)))
    (js-roundtrip 10000)
    (- (current-jiffy) start)))

;; Test 8: Build vector and pass to JS (tests deep conversion)
;; Test 8: Deep conversion (Scheme -> JS) - Arguments
;; Measures cost of deep converting arguments when calling JS
(define (build-and-pass n)
  (let ((arr (make-vector n 0)))
    (do ((i 0 (+ i 1)))
        ((>= i n))
      (vector-set! arr i i))
    ;; Arguments are deeply converted by default
    (js-invoke benchmark-helper "noop" arr)))

(define (time-array-conversion)
  (let ((start (current-jiffy)))
    (build-and-pass 10000)
    (- (current-jiffy) start)))

;; Helper: build a list 0..n-1
(define (build-list-interop n)
  (let loop ((i (- n 1)) (acc '()))
    (if (< i 0)
        acc
        (loop (- i 1) (cons i acc)))))

;; Test 9: Nested deep conversion (Scheme -> JS) - Arguments
(define (build-nested-list depth width)
  (if (= depth 0)
      (build-list-interop width)
      (map (lambda (i) (build-nested-list (- depth 1) width))
           (build-list-interop width))))

(define (time-nested-conversion)
  (let ((start (current-jiffy)))
    (js-invoke benchmark-helper "noop" (build-nested-list 3 10))  ; 10^3 = 1000 leaves
    (- (current-jiffy) start)))

;; Test 10: JS -> Scheme Return Value (Shallow)
;; The 'echo' method returns the JS array/object as-is.
;; Currently this should be fast as it shallow-wraps.
(define (time-js-return-shallow)
  (let ((vec (make-vector 10000 0)))
    ;; Pre-convert to JS array so we only measure return cost
    (let ((js-arr (scheme->js-deep vec)))
      (let ((start (current-jiffy)))
        ;; 'echo' returns the JS array, which is shallow converted to a vector (or kept as js-object?)
        (js-invoke benchmark-helper "echo" js-arr)
        (- (current-jiffy) start)))))

;; Test 11: Full Roundtrip (Deep Args -> Shallow Return)
(define (time-roundtrip-conversion)
  (let ((vec (make-vector 10000 0)))
    (let ((start (current-jiffy)))
      (js-invoke benchmark-helper "echo" vec)
      (- (current-jiffy) start))))
