;; A Scheme program to verify constant space usage.
;; It runs a tail-recursive loop for many iterations.
;; Every 'chunk-size' iterations, it forces GC and checks heap size.
;; It prints none of the sizes: they differ from run to run, and a test file's
;; output must not, since `benchmarks/run_tier.js` compares a test file's
;; output with the tier attached against its output without.

(define chunk-size 100000)
(define max-iterations 1000000)

(define (check-heap-growth n initial-heap)
  (if (> n max-iterations)
      #t ;; Finished without crashing or growing unbounded
      (if (= (modulo n chunk-size) 0)
          (let ((current-heap (garbage-collect-and-get-heap-usage)))
              (if (= initial-heap 0) 
                  ;; GC not supported, just run the loop to check for crash
                  (check-heap-growth (+ n 1) initial-heap)
                  ;; GC supported, check for growth
                  (if (> current-heap (* initial-heap 2)) ;; generous 2x buffer
                      #f ;; heap grew unboundedly
                      (check-heap-growth (+ n 1) initial-heap))))
          (check-heap-growth (+ n 1) initial-heap))))

(test-group "TCO Tests"
    (test "no heap growth" #t
        (check-heap-growth 0 (garbage-collect-and-get-heap-usage))
    )
)
