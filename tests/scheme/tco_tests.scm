;; A Scheme program to verify constant space usage.
;; It runs a tail-recursive loop for many iterations.
;; Every 'chunk-size' iterations, it forces GC and checks heap size.

(define chunk-size 100000)
(define max-iterations 1000000)

(define (check-heap-growth n initial-heap)
  (if (> n max-iterations)
      (begin
        (display (garbage-collect-and-get-heap-usage))
        #t );; Finished without crashing or growing unbounded
      (if (= (modulo n chunk-size) 0)
          (let ((current-heap (garbage-collect-and-get-heap-usage)))
              ;(display current-heap)
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
        (let ((start-heap (garbage-collect-and-get-heap-usage)))
            (display start-heap)
            (check-heap-growth 0 start-heap))
    )
)
