;; Minimal test library for library loader testing
(define-library (test hello)
  (export greet double)
  (import (scheme base))
  (begin
    (define greeting "Hello from library!")
    
    (define (greet name)
      (string-append greeting " " name))
    
    (define (double x)
      (+ x x))))
