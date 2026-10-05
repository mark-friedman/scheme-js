;; Syntax errors
;;
;; R7RS 4.3.3: `syntax-error` signals an error as soon as it is expanded, so
;; that a macro's template can report a misuse of the macro wherever the
;; misuse is, whether or not the code it is in ever runs. And a core form
;; with the wrong number of operands is a syntax error, raised as one, with
;; the form's keyword in its message.

(define-syntax syntax-error-test-let
  (syntax-rules ()
    ((_ ((x . y) val) body)
     (syntax-error "expected an identifier but got" (x . y)))
    ((_ (name val) body)
     ((lambda (name) body) val))))

;; /**
;;  * The message and irritants of the error that evaluating a form at top
;;  * level raises, or #f if it raises none.
;;  * @param {*} form - The form.
;;  * @returns {list|boolean} (message irritant ...), or #f.
;;  */
(define (syntax-error-test-raised form)
  (guard (e ((error-object? e) (cons (error-object-message e) (error-object-irritants e)))
            (#t (list 'not-an-error-object)))
    (eval form (interaction-environment))
    #f))

;; /**
;;  * Whether a string begins with another.
;;  * @param {string} prefix - The other.
;;  * @param {string} s - The string.
;;  * @returns {boolean}
;;  */
(define (syntax-error-test-prefix? prefix s)
  (and (string? s)
       (>= (string-length s) (string-length prefix))
       (string=? prefix (substring s 0 (string-length prefix)))))

(test-group "syntax-error"

  (test "a macro used rightly is not an error"
    2
    (syntax-error-test-let (a 1) (+ a 1)))

  (test "used wrongly, it raises the message and irritants syntax-error was given"
    '("expected an identifier but got" (p . q))
    (syntax-error-test-raised '(syntax-error-test-let ((p . q) 1) 2)))

  (test "as the use is expanded, in a procedure never called"
    '("expected an identifier but got" (p . q))
    (syntax-error-test-raised '(define (syntax-error-test-never) (syntax-error-test-let ((p . q) 1) 2))))

  (test "and used directly, it raises at once"
    '("plain" 1 2)
    (syntax-error-test-raised '(syntax-error "plain" 1 2))))

(test-group "a core form with the wrong number of operands"

  (test "if with none"
    #t
    (syntax-error-test-prefix? "if:" (car (syntax-error-test-raised '(if)))))

  (test "if with one"
    #t
    (syntax-error-test-prefix? "if:" (car (syntax-error-test-raised '(if #t)))))

  (test "if with four"
    #t
    (syntax-error-test-prefix? "if:" (car (syntax-error-test-raised '(if #t 1 2 3)))))

  (test "quote with two"
    #t
    (syntax-error-test-prefix? "quote:" (car (syntax-error-test-raised '(quote a b)))))

  (test "set! with one"
    #t
    (syntax-error-test-prefix? "set!:" (car (syntax-error-test-raised '(set! syntax-error-test-x)))))

  (test "define of a variable with two expressions"
    #t
    (syntax-error-test-prefix? "define:" (car (syntax-error-test-raised '(define syntax-error-test-y 1 2)))))

  (test "define with nothing"
    #t
    (syntax-error-test-prefix? "define:" (car (syntax-error-test-raised '(define)))))

  (test "lambda with nothing"
    #t
    (syntax-error-test-prefix? "lambda:" (car (syntax-error-test-raised '(lambda)))))

  (test "lambda not a proper list"
    #t
    (syntax-error-test-prefix? "lambda:" (car (syntax-error-test-raised '(lambda . 1)))))

  (test "define-syntax with a name alone"
    #t
    (syntax-error-test-prefix? "define-syntax:" (car (syntax-error-test-raised '(define-syntax m)))))

  (test "let-syntax with nothing"
    #t
    (syntax-error-test-prefix? "let-syntax:" (car (syntax-error-test-raised '(let-syntax)))))

  (test "a let binding with no expression"
    #t
    (syntax-error-test-prefix? "let:" (car (syntax-error-test-raised '(let ((x)) x)))))

  (test "a procedure defined with no expression in its body is still an error"
    #t
    (pair? (syntax-error-test-raised '(define (syntax-error-test-f)))))

  (test "the right number is not"
    '(1 2)
    (list (if #t 1 2) (quote 2))))
