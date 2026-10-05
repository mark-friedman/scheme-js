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
  (import (scheme char))
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
