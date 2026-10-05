;; A library sees only what it imports
;;
;; R7RS 5.6.1: a library's body is evaluated in an environment of its import
;; sets, and an environment `environment` makes holds only the bindings of its
;; import sets (6.12). Neither sees the primitives a program's top level sees,
;; nor the macros the program defined. The libraries here have names no other
;; test uses, since test files share a registry.
;;
;; A name bound nowhere in a library still falls back to JavaScript's globals,
;; as in a program, so the macro here has a name no JavaScript global can
;; have: Chrome's `globalThis.when`, for one, is a procedure.

(define-syntax strict-test-when
  (syntax-rules ()
    ((_ test result) (if test result #f))))

(define-library (strict-test char-only)
  (export first-of)
  (import (scheme char) (only (scheme base) define))
  (begin (define (first-of xs) (car xs))))

(define-library (strict-test no-when)
  (export when-of)
  (import (only (scheme base) define quote))
  (begin (define (when-of) (strict-test-when #t 'yes))))

(define-library (strict-test own-apply)
  (export sum-of-two)
  (import (except (scheme base) apply))
  (begin
    (define (apply . xs) 'mine)
    (define (sum-of-two) (call-with-values (lambda () (values 1 2)) +))))

(import (prefix (strict-test char-only) strict-test:)
        (prefix (strict-test no-when) strict-test:)
        (prefix (strict-test own-apply) strict-test:))

;; /**
;;  * Whether calling a thunk raises.
;;  * @param {procedure} thunk - The thunk.
;;  * @returns {boolean}
;;  */
(define (strict-test-raises? thunk)
  (guard (e (#t #t))
    (thunk)
    #f))

(test-group "a library sees only what it imports"

  (test "a primitive it does not import is unbound in it"
    #t
    (strict-test-raises? (lambda () (strict-test:first-of '(1 2)))))

  (test "as is a macro the program defined"
    #t
    (strict-test-raises? strict-test:when-of))

  (test "call-with-values needs no apply imported, and is not its own apply"
    3
    (strict-test:sum-of-two)))

(test-group "an environment of import sets holds only their bindings"

  (test "a primitive its libraries do not export is unbound in it"
    #t
    (strict-test-raises? (lambda () (eval 'car (environment '(scheme char))))))

  (test "as is a macro the program defined"
    #t
    (strict-test-raises? (lambda () (eval '(strict-test-when #t 1) (environment '(scheme char))))))

  (test "or a library its import sets do not name"
    #t
    (strict-test-raises? (lambda () (eval '(let*-values (((a) 1)) a) (environment '(scheme char))))))

  (test "what they export is bound"
    #\A
    (eval '(char-upcase #\a) (environment '(scheme char))))

  (test "(scheme base) exports features, file-error? and read-error?"
    '(#t #f #f)
    (eval '(list (pair? (features)) (file-error? 'x) (read-error? 'x))
          (environment '(scheme base)))))

;; A special form is a syntactic keyword like any other: a library or an
;; environment that does not import it does not have it, and may bind its
;; name to something else.
(define-library (strict-test own-if)
  (export own-if-of)
  (import (except (scheme base) if))
  (begin
    (define (if a b c) (list 'mine a b c))
    (define (own-if-of) (if 1 2 3))))

(define-library (strict-test renamed-if)
  (export renamed-if-of)
  (import (rename (scheme base) (if when-true)))
  (begin
    (define (renamed-if-of) (when-true #f 'yes 'no))))

(import (prefix (strict-test own-if) strict-test:)
        (prefix (strict-test renamed-if) strict-test:))

(test-group "a special form is seen only where it is imported"

  (test "a library that does not import if may define it"
    '(mine 1 2 3)
    (strict-test:own-if-of))

  (test "one that imports it renamed has it by that name"
    'no
    (strict-test:renamed-if-of))

  (test "in an environment that does not import it, if is not a special form"
    #t
    (strict-test-raises? (lambda () (eval '(if #t 1 2) (environment '(scheme char))))))

  (test "nor are lambda and quote"
    '(#t #t)
    (list (strict-test-raises? (lambda () (eval '((lambda (x) x) 1) (environment '(scheme char)))))
          (strict-test-raises? (lambda () (eval '(quote x) (environment '(scheme char)))))))

  (test "an environment importing if renamed has it by that name only"
    '(1 #t)
    (let ((env (environment '(rename (only (scheme base) if) (if when-true)))))
      (list (eval '(when-true #t 1 2) env)
            (strict-test-raises? (lambda () (eval '(if #t 1 2) env))))))

  (test "a program's top level that imports nothing still has them all"
    1
    (if #t 1 2)))
