(import (scheme base) (scheme write))

;; A simplified version of test-protect that uses 'e'
(define-syntax protect-with-e
  (syntax-rules ()
    ((_ expr)
     (guard (e (else (display "Caught: ") (display e) (newline)))
       expr))))

;; A simplified version that uses a unique name
(define-syntax protect-with-unique
  (syntax-rules ()
    ((_ expr)
     (guard (unique-var (else (display "Caught: ") (display unique-var) (newline)))
       expr))))

(display "--- Testing protect-with-unique ---\n")
(protect-with-unique (raise "success-unique"))

(display "--- Testing protect-with-e ---\n")
(protect-with-e (raise "success-e"))
