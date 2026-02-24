;; manual_script.scm
;; Used for DevTools debugging manual verification

(display "--- Running External Script ---\n")

(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define result (fib 10))

(display (string-append "Fibonacci of 10 is: " (number->string result) "\n"))
