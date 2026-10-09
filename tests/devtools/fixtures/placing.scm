;; Scheme whose lines hold code that calls nothing -- a constant, a binding,
;; an assignment, the use of a macro the system defines -- which the DevTools
;; tests set breakpoints on and step through (tests/devtools/stepping_tests.js).
;; The tests find each line by its text.

(define (classify n)
  (cond ((< n 0) 'negative)
        ((= n 0) 'zero)
        (else 'positive)))

(define (assigning flag)
  (let ((count 0))
    (when flag
      (set! count 1))
    count))

(define (counting n)
  (do ((i 0 (+ i 1))
       (acc '() (cons i acc)))
      ((= i n) acc)))

(define (summing items)
  (let loop ((items items) (total 0))
    (if (null? items)
        total
        (loop (cdr items) (+ total (car items))))))
