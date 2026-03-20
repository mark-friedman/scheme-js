;; Test 1: Simple macro
(define-macro (my-when test . body)
  `(if ,test (begin ,@body)))

(my-when #t
  (display "OK")
  (newline))

;; Test 2: Macro returning a value
(define-macro (add-one x)
  `(+ ,x 1))

(if (= (add-one 5) 6)
    (display "Math OK")
    (display "Math Fail"))
(newline)

;; Test 3: define-macro name transformer syntax
(define-macro my-unless
  (lambda (test . body)
    `(if (not ,test) (begin ,@body))))

(my-unless #f
  (display "Unless OK")
  (newline))
