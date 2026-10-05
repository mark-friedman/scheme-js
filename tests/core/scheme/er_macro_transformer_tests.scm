;; er-macro-transformer: explicit renaming (Clinger, "Hygienic Macros Through
;; Explicit Renaming", 1991)
;;
;; A procedural macro: its transformer is a procedure of the use, `rename` and
;; `compare`. What it returns is what the use expands into, as written: a
;; symbol it makes up is the user's, found where the macro is used, and one it
;; renames is the macro's, found where the macro was defined, and binds
;; nothing of the user's. `compare` says whether two identifiers mean the
;; same where the macro is used.
;;
;; A transformer's procedure is evaluated where the macro is defined: it sees
;; the library's or program's imports and what it defined before. The names
;; are unusual, since test files share an environment.

(define-syntax er-swap!
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (car (cdr form)))
            (b (car (cdr (cdr form)))))
        (list (rename 'let) (list (list (rename 'tmp) a))
              (list (rename 'set!) a b)
              (list (rename 'set!) b (rename 'tmp)))))))

(define-syntax er-list-of
  (er-macro-transformer
    (lambda (form rename compare)
      (cons (rename 'list) (cdr form)))))

(define-syntax er-it
  (er-macro-transformer
    (lambda (form rename compare)
      'it)))

;; `(er-else-or-other clause)`: 'else if the clause's head means `else` where
;; the macro is used, 'other if it does not.
(define-syntax er-else-or-other
  (er-macro-transformer
    (lambda (form rename compare)
      (if (compare (car (car (cdr form))) (rename 'else))
          (list (rename 'quote) 'else)
          (list (rename 'quote) 'other)))))

(test-group "er-macro-transformer - renaming"
  (test "a binding it introduces captures nothing of the user's"
        '(2 1)
        (let ((tmp 1) (other 2))
          (er-swap! tmp other)
          (list tmp other)))
  (test "nor does a user's binding capture what it renamed"
        '(1 2)
        (let ((list vector))
          (er-list-of 1 2)))
  (test "a symbol it does not rename is the user's, where the macro is used"
        5
        (let ((it 5)) (er-it)))
  (test "renaming a local of where the macro was defined gives that local"
        'outer
        (let ((x 'outer))
          (let-syntax ((er-get-x (er-macro-transformer (lambda (form rename compare) (rename 'x)))))
            (let ((x 'inner))
              (er-get-x)))))
  (test "renaming one name twice gives identifiers that bind the same"
        'same
        (let-syntax ((er-twice (er-macro-transformer
                                 (lambda (form rename compare)
                                   (list (rename 'let) (list (list (rename 'v) ''same))
                                         (rename 'v))))))
          (let ((v 'user)) (er-twice)))))

(test-group "er-macro-transformer - comparing"
  (test "a user's else is the macro's else" 'else (er-else-or-other (else 1)))
  (test "another name is not" 'other (er-else-or-other (otherwise 1)))
  (test "nor is else bound locally where the macro is used"
        'other
        (let ((else #f)) (er-else-or-other (else 1))))
  (test "two of the user's identifiers of one name are the same"
        #t
        (let-syntax ((er-same? (er-macro-transformer
                                 (lambda (form rename compare)
                                   (compare (car (cdr form)) (car (cdr (cdr form))))))))
          (er-same? a a)))
  (test "and of two names are not"
        #f
        (let-syntax ((er-same? (er-macro-transformer
                                 (lambda (form rename compare)
                                   (compare (car (cdr form)) (car (cdr (cdr form))))))))
          (er-same? a b))))

(test-group "er-macro-transformer - where it is defined"
  (test "in letrec-syntax, a macro whose expansion uses itself"
        3
        (letrec-syntax ((er-count (er-macro-transformer
                                    (lambda (form rename compare)
                                      (if (null? (cdr form))
                                          0
                                          (list (rename '+) 1 (cons (rename 'er-count) (cdr (cdr form)))))))))
          (er-count a b c)))
  (test "in a body, defined with define-syntax"
        '(b a)
        (let ()
          (define-syntax er-flip
            (er-macro-transformer
              (lambda (form rename compare)
                (list (rename 'list) (car (cdr (cdr form))) (car (cdr form))))))
          (er-flip 'a 'b))))

;; A library's macro: what it renames is the library's, exported or not, and
;; a program that redefines the name afterwards changes nothing. A library has
;; er-macro-transformer if it imports it.
(define-library (er-macro-tests library)
  (export er-call-hidden er-call-car)
  (import (scheme base) (scheme-js procedural-macros))
  (begin
    (define (er-hidden x) (list 'hidden x))
    (define-syntax er-call-hidden
      (er-macro-transformer
        (lambda (form rename compare)
          (list (rename 'er-hidden) (car (cdr form))))))
    (define-syntax er-call-car
      (er-macro-transformer
        (lambda (form rename compare)
          (list (rename 'car) (car (cdr form))))))))

(import (er-macro-tests library))

(test-group "er-macro-transformer - a library's macro"
  (test "refers to the library's unexported procedure" '(hidden 1) (er-call-hidden 1))
  (test "and to what the library imported, though the program shadows it"
        1
        (let ((car cdr)) (er-call-car '(1 2)))))

(test-group "er-macro-transformer - errors"
  (test "a transformer's failure is the macro's syntax error"
        'raised
        (guard (e (#t 'raised))
          (eval '(let-syntax ((er-broken (er-macro-transformer (lambda (form rename compare) (car '())))))
                   (er-broken))
                (environment '(scheme base) '(scheme-js procedural-macros))))))

;; define-macro, a legacy extension: a transformer of the use's operands, whose
;; result is used as it is, nothing renamed.
(define-macro (er-legacy-twice x) (list 'begin x x))

(test-group "define-macro, on er-macro-transformer"
  (test "expands as it did"
        2
        (let ((n 0)) (er-legacy-twice (set! n (+ n 1))) n))
  (test "and what it makes up is the user's"
        7
        (let ((begin (lambda (a b) 7))) (er-legacy-twice 1))))

;; A procedure the file defines before the macro whose procedure calls it.
(define (er-doubled-form x) (list '* 2 x))

(define-syntax er-double
  (er-macro-transformer
    (lambda (form rename compare)
      (er-doubled-form (cadr form)))))

;; A library whose macro's procedure calls a procedure the library keeps to
;; itself.
(define-library (er-macro-tests helpers)
  (export er-reversed)
  (import (scheme base) (scheme-js procedural-macros))
  (begin
    (define (reversed-operands form) (reverse (cdr form)))
    (define-syntax er-reversed
      (er-macro-transformer
        (lambda (form rename compare)
          (cons (rename 'list) (reversed-operands form)))))))

(import (er-macro-tests helpers))

(define-macro (er-legacy-second . operands) (cadr operands))

(test-group "where a transformer's procedure runs"
  (test "it sees the standard library"
        '(2 4 6)
        (let-syntax ((er-doubles (er-macro-transformer
                                   (lambda (form rename compare)
                                     (cons (rename 'list) (map (lambda (x) (* 2 x)) (cdr form)))))))
          (er-doubles 1 2 3)))
  (test "and a procedure defined before the macro where it is defined" 10 (er-double 5))
  (test "a library's, a procedure the library does not export" '(3 2 1) (er-reversed 1 2 3))
  (test "a define-macro's too" 2 (er-legacy-second 1 2))
  (test "in an environment, what it imports"
        '(b a)
        (eval '(let-syntax ((swap-list (er-macro-transformer
                                         (lambda (form rename compare)
                                           (list (rename 'quote) (reverse (cadr form)))))))
                 (swap-list (a b)))
              (environment '(scheme base) '(scheme-js procedural-macros))))
  (test "an environment that does not import er-macro-transformer has none"
        'raised
        (guard (e (#t 'raised))
          (eval '(let-syntax ((m (er-macro-transformer (lambda (form rename compare) 1)))) (m))
                (environment '(scheme base))))))

(test-group "define-syntax's transformer"
  (test "one it does not know is a syntax error"
        'raised
        (guard (e (#t 'raised))
          (eval '(define-syntax m (er-macro-transformer (lambda (form rename compare) 1)))
                (environment '(scheme base))))))
