;; The library system's Scheme (src/core/scheme/library_system.scm): parsing
;; `define-library` and import sets, the names an import set gives, and the
;; feature requirements `cond-expand` tests.

(import (scheme base)
        (scheme-js library-system))

;; /**
;;  * Whether a feature requirement is met, with the features `r7rs` and
;;  * `scheme-js` and only the library `(present lib)` available.
;;  * @param {*} requirement - The requirement.
;;  * @returns {boolean}
;;  */
(define (met? requirement)
  (requirement-met? requirement '(r7rs scheme-js)
                    (lambda (name) (equal? name '(present lib)))))

(test-group "library system - feature requirements"
  (test "a feature present" #t (met? 'r7rs))
  (test "a feature absent" #f (met? 'chicken))
  (test "and of present features" #t (met? '(and r7rs scheme-js)))
  (test "and with one absent" #f (met? '(and r7rs chicken)))
  (test "and of none" #t (met? '(and)))
  (test "or with one present" #t (met? '(or chicken r7rs)))
  (test "or of none" #f (met? '(or)))
  (test "not of an absent feature" #t (met? '(not chicken)))
  (test "nested" #t (met? '(or (and chicken r7rs) (not (not scheme-js)))))
  (test "a library available" #t (met? '(library (present lib))))
  (test "a library not available" #f (met? '(library (absent lib))))
  (test "an unknown requirement is not met" #f (met? '(frobnicate r7rs)))
  (test-error "not takes one requirement" "not" (met? '(not r7rs scheme-js)))
  (test-error "library takes one name" "library" (met? '(library))))

(test-group "library system - import sets"
  (define (steps spec) (import-set-steps (parse-import-set spec)))
  (define (library spec) (import-set-library-name (parse-import-set spec)))
  (test "a library name alone" '(scheme base) (library '(scheme base)))
  (test "has no filters" '() (steps '(scheme base)))
  (test "a name with a number in it" '(srfi 1) (library '(srfi 1)))
  (test "only" '((only car cdr)) (steps '(only (scheme base) car cdr)))
  (test "except" '((except car)) (steps '(except (scheme base) car)))
  (test "prefix" '((prefix . b:)) (steps '(prefix (scheme base) b:)))
  (test "rename" '((rename (car . first) (cdr . rest))) (steps '(rename (scheme base) (car first) (cdr rest))))
  (test "filters nest, innermost first" '((prefix . p:) (only p:car))
        (steps '(only (prefix (scheme base) p:) p:car)))
  (test "and the library is the innermost set's" '(scheme base)
        (library '(only (prefix (scheme base) p:) p:car)))
  ;; A library may be named by a filter's keyword: it is a filter only when
  ;; what follows the keyword is an import set.
  (test "a library whose name begins with a filter's keyword" '(only lib) (library '(only lib)))
  (test "is not filtered" '() (steps '(only lib))))

(test-group "library system - the names an import set gives"
  (define (named name spec) (imported-name name (import-set-steps (parse-import-set spec))))
  (test "without filters, every name as it is" 'car (named 'car '(scheme base)))
  (test "only keeps a name listed" 'car (named 'car '(only (scheme base) car)))
  (test "and leaves out one that is not" #f (named 'cdr '(only (scheme base) car)))
  (test "except leaves out a name listed" #f (named 'car '(except (scheme base) car)))
  (test "and keeps one that is not" 'cdr (named 'cdr '(except (scheme base) car)))
  (test "prefix" 'b:car (named 'car '(prefix (scheme base) b:)))
  (test "rename" 'first (named 'car '(rename (scheme base) (car first))))
  (test "rename leaves other names" 'cdr (named 'cdr '(rename (scheme base) (car first))))
  (test "only around prefix names prefixed names" 'p:car (named 'car '(only (prefix (scheme base) p:) p:car)))
  (test "and leaves out the rest" #f (named 'cdr '(only (prefix (scheme base) p:) p:car)))
  (test "prefix around rename prefixes the new name" 'p:first
        (named 'car '(prefix (rename (scheme base) (car first)) p:))))

(test-group "library system - define-library"
  (define (parsed form) (parse-define-library form met?))
  (define simple
    (parsed '(define-library (my lib)
               (export a (rename b c))
               (import (scheme base) (only (srfi 1) fold))
               (begin (define a 1) (define b 2))
               (include "one.scm" "two.scm")
               (include-ci "three.scm")
               (include-library-declarations "decls.scm"))))
  (test "the name" '(my lib) (library-definition-name simple))
  (test "exports, each an internal name and the name exported" '((a . a) (b . c))
        (library-definition-exports simple))
  (test "imports, as import sets" '((scheme base) (srfi 1))
        (map import-set-library-name (library-definition-imports simple)))
  (test "with their filters" '(() ((only fold)))
        (map import-set-steps (library-definition-imports simple)))
  (test "the body's forms, in order" '((define a 1) (define b 2)) (library-definition-body simple))
  (test "included files" '("one.scm" "two.scm") (library-definition-includes simple))
  (test "included files folding case" '("three.scm") (library-definition-includes-ci simple))
  (test "files of library declarations" '("decls.scm") (library-definition-declaration-files simple))
  (test "declarations of a kind gather in order" '((define a 1) (define b 2) (define c 3))
        (library-definition-body
          (parsed '(define-library (l) (begin (define a 1)) (export a) (begin (define b 2) (define c 3))))))
  (test "cond-expand takes the first clause whose requirement is met" '((define x 'r7rs))
        (library-definition-body
          (parsed '(define-library (l)
                     (cond-expand (chicken (begin (define x 'chicken)))
                                  (r7rs (begin (define x 'r7rs)))
                                  (else (begin (define x 'else))))))))
  (test "or its else clause" '((define x 'else))
        (library-definition-body
          (parsed '(define-library (l)
                     (cond-expand (chicken (begin (define x 'chicken)))
                                  (else (begin (define x 'else))))))))
  (test "or nothing" '()
        (library-definition-body
          (parsed '(define-library (l) (cond-expand (chicken (begin (define x 1))))))))
  (test "a clause's declarations may be of any kind, cond-expand included" '((x . x) (y . y))
        (library-definition-exports
          (parsed '(define-library (l)
                     (cond-expand (r7rs (export x) (cond-expand ((library (present lib)) (export y)))))))))
  (test "an empty declaration is nothing" '() (library-definition-body (parsed '(define-library (l) ()))))
  (test-error "a library needs a name" "define-library" (parsed '(define-library)))
  (test-error "a form that is not define-library" "define-library" (parsed '(define-something (l))))
  (test-error "an unknown declaration" "unknown" (parsed '(define-library (l) (frobnicate x))))
  (test-error "a declaration that is not a list" "define-library" (parsed '(define-library (l) export)))
  (test-error "an export that is neither a name nor a rename" "export"
              (parsed '(define-library (l) (export (rename a))))))
