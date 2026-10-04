;; A definition shadows a macro of the same name
;;
;; A program's or a library's top-level definition of a name makes the name
;; that variable for what follows, as an internal definition already did --
;; though a macro of the name is defined for the whole process, or was
;; imported. A program that defines its own `assert`, or a library that
;; imports `(scheme base)` but for `when` and defines its own, calls its own.
;; The names here are the tests' own, so that the macros every other test
;; uses are left alone.

(define-syntax shadowing-probe
  (syntax-rules () ((_ x) (list 'macro x))))

(test-group "a definition shadows a macro"
  (test "before the definition, the macro" '(macro 1) (shadowing-probe 1)))

(define (shadowing-probe x) (list 'procedure x))

(test-group "a definition shadows a macro, after it"
  (test "the procedure is called" '(procedure 1) (shadowing-probe 1))
  (test "and is a value" #t (procedure? shadowing-probe)))

(define-syntax shadowing-probe
  (syntax-rules () ((_ x) (list 'macro-again x))))

(test-group "a macro defined again shadows the definition"
  (test "the macro again" '(macro-again 1) (shadowing-probe 1)))

;; A top-level `begin` splices its definitions into the top level.
(define-syntax shadowing-begin-probe
  (syntax-rules () ((_) 'macro)))

(begin (define (shadowing-begin-probe) 'procedure))

(test-group "a definition in a top-level begin shadows a macro"
  (test "after the begin" 'procedure (shadowing-begin-probe)))

;; A library leaving a macro out of its imports defines its own.
(define-syntax shadowing-library-probe
  (syntax-rules () ((_ x) (list 'macro x))))

(define-library (shadowing own)
  (import (scheme base))
  (export call-own own-value)
  (begin
    (define (shadowing-library-probe x) (list 'library-procedure x))
    (define (call-own) (shadowing-library-probe 2))
    (define own-value shadowing-library-probe)))

(import (shadowing own))

(test-group "a library's definition shadows a macro"
  (test "in the library" '(library-procedure 2) (call-own))
  (test "exported as the procedure" '(library-procedure 3) (own-value 3)))

;; A procedure imported under a macro's name is the procedure.
(define-library (shadowing exporter)
  (import (scheme base))
  (export shadowing-import-probe)
  (begin (define (shadowing-import-probe x) (list 'imported-procedure x))))

(define-syntax shadowing-import-probe
  (syntax-rules () ((_ x) (list 'macro x))))

(import (shadowing exporter))

(test-group "an imported procedure shadows a macro"
  (test "the imported procedure is called" '(imported-procedure 4) (shadowing-import-probe 4)))

;; A local binding already shadowed a macro, and still does.
(test-group "a local binding shadows a macro"
  (test "in a body" '(local 5)
        (let ()
          (define (shadowing-local-probe x) (list 'local x))
          (shadowing-local-probe 5)))
  (test "a zero-argument lambda's body definition stays local" 'local
        ((lambda ()
           (define (shadowing-begin-probe) 'local)
           (shadowing-begin-probe)))))
