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

;; Never called as the page starts, so compiled before its first call only
;; when every procedure is compiled as it is defined: it neither loops nor
;; makes a procedure, nor binds with `let`, whose expansion applies one.
(define (first-call x)
  (+ (* x 2)
     1))

;; A procedure a definition's value holds in data, bound to no name: compiled
;; before it first runs only when every procedure is, with the value.
(define handlers
  (list (lambda (x)
          (* x 3))))

(define (call-handler x)
  ((car handlers) x))
