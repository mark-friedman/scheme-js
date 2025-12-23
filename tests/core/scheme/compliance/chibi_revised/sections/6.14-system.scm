(test-group "6.14 System interface"

;; 6.14 System interface

;; (test "/usr/local/bin:/usr/bin:/bin" (get-environment-variable "PATH"))

(test #t (string? (get-environment-variable "PATH")))

;; (test '(("USER" . "root") ("HOME" . "/")) (get-environment-variables))

(let ((env (get-environment-variables)))
  (define (env-pair? x)
    (and (pair? x) (string? (car x)) (string? (cdr x))))
  (define (all? pred ls)
    (or (null? ls) (and (pred (car ls)) (all? pred (cdr ls)))))
  (test #t (list? env))
  (test #t (all? env-pair? env)))

(test #t (list? (command-line)))

(test #t (real? (current-second)))
(test #t (inexact? (current-second)))
(test #t (exact? (current-jiffy)))
(test #t (exact? (jiffies-per-second)))

(test #t (list? (features)))
(test #t (and (memq 'r7rs (features)) #t))

(test #t (file-exists? "."))
(test #f (file-exists? " no such file "))

(test #t (file-error?
          (guard (exn (else exn))
            (delete-file " no such file "))))

)
