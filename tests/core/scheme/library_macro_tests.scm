;; Library macro tests
;;
;; R7RS 4.3: a macro is referentially transparent -- an identifier its
;; template introduces free refers to the binding visible where the macro was
;; defined, not where it is used. For a macro a library exports, that is the
;; library's own environment: its definitions, exported or not, and the names
;; it imports. A program using the macro need not import, and cannot shadow,
;; any of them. Libraries that export a test framework's syntax, as SRFI 64
;; implementations do, depend on it: their `test-assert` expands into calls
;; of procedures the library keeps to itself.
;;
;; The libraries are written inline, and their names and the names they bind
;; are unusual, since test files share an environment.

;; A library that only another library imports, so that the program here has
;; none of its names.
(define-library (library-macro-tests source)
  (export lmt-source-value)
  (import (scheme base))
  (begin
    (define (lmt-source-value) 'from-the-source)))

(define-library (library-macro-tests exporter)
  (export lmt-call-helper lmt-set-field lmt-get-field lmt-bump-counter
          lmt-counter-value lmt-call-import lmt-introduce-binding
          lmt-call-later-helper lmt-later-helper-result lmt-nested
          lmt-define-caller lmt-shadow-proof)
  (import (scheme base) (library-macro-tests source))
  (begin
    ;; Unexported procedures and a record type, none exported.
    (define (lmt-helper) 'helper-result)
    (define-record-type lmt-box
      (make-lmt-box a)
      lmt-box?
      (a lmt-box-a)
      (b lmt-box-b set-lmt-box-b!))
    (define lmt-counter 0)

    (define-syntax lmt-call-helper
      (syntax-rules ()
        ((_) (lmt-helper))))

    ;; The constructor, a modifier and an accessor, all internal.
    (define-syntax lmt-set-field
      (syntax-rules ()
        ((_ value) (let ((box (make-lmt-box 'a)))
                     (set-lmt-box-b! box value)
                     box))))
    (define-syntax lmt-get-field
      (syntax-rules ()
        ((_ box) (if (lmt-box? box) (lmt-box-b box) 'not-a-box))))

    ;; An assignment to an unexported variable.
    (define-syntax lmt-bump-counter
      (syntax-rules ()
        ((_) (begin (set! lmt-counter (+ lmt-counter 1)) lmt-counter))))
    (define (lmt-counter-value) lmt-counter)

    ;; A name the library imports, from a library the program does not.
    (define-syntax lmt-call-import
      (syntax-rules ()
        ((_) (lmt-source-value))))

    ;; A binding the template introduces, beside the program's own variable
    ;; of the same name.
    (define-syntax lmt-introduce-binding
      (syntax-rules ()
        ((_ e) (let ((lmt-tmp 'introduced)) (list lmt-tmp e)))))

    ;; Used by a procedure of the library's own before the helper it calls
    ;; is defined.
    (define-syntax lmt-call-later-helper
      (syntax-rules ()
        ((_) (lmt-later-helper))))
    (define (lmt-later-helper-result) (lmt-call-later-helper))
    (define (lmt-later-helper) 'later-helper-result)

    ;; One exported macro expanding into another, each reaching internals.
    (define-syntax lmt-nested
      (syntax-rules ()
        ((_ v) (lmt-get-field (lmt-set-field v)))))

    ;; A macro whose expansion defines a macro: the inner macro's template
    ;; came from this library, so it reaches the library's helper too.
    (define-syntax lmt-define-caller
      (syntax-rules ()
        ((_ name) (define-syntax name
                    (syntax-rules ()
                      ((_) (lmt-helper)))))))

    ;; A use site's own binding of the helper's name does not capture it.
    (define-syntax lmt-shadow-proof
      (syntax-rules ()
        ((_) (lmt-helper))))))

(import (library-macro-tests exporter))

(test-group "library macros reach the library's own bindings"

  (test "an unexported procedure"
    'helper-result
    (lmt-call-helper))

  (test "an unexported record type's constructor, modifier and accessor"
    'stored
    (lmt-get-field (lmt-set-field 'stored)))

  (test "an unexported record type's predicate"
    'not-a-box
    (lmt-get-field 42))

  (test "an assignment to an unexported variable"
    '(1 2 2)
    (let* ((first (lmt-bump-counter))
           (second (lmt-bump-counter)))
      (list first second (lmt-counter-value))))

  (test "a name the library imports and the program does not"
    'from-the-source
    (lmt-call-import))

  (test "a procedure of the library's own, through a macro used before the helper is defined"
    'later-helper-result
    (lmt-later-helper-result))

  (test "one exported macro expanding into another"
    'nested
    (lmt-nested 'nested))

  (test "a macro defined by a library macro's expansion"
    'helper-result
    (let ()
      (lmt-define-caller lmt-defined-caller)
      (lmt-defined-caller))))

(test-group "library macros are referentially transparent"

  (test "a local binding of the helper's name at the use site"
    'helper-result
    (let ((lmt-helper (lambda () 'the-use-sites)))
      (lmt-shadow-proof)))

  (test "a binding the template introduces captures nothing"
    '(introduced mine)
    (let ((lmt-tmp 'mine))
      (lmt-introduce-binding lmt-tmp))))

;; The program's own top-level definition of the helper's name.
(define (lmt-helper) 'the-programs)

(test-group "a program's definition of a library's internal name"

  (test "is the program's"
    'the-programs
    (lmt-helper))

  (test "is not the macro's"
    'helper-result
    (lmt-shadow-proof)))

;; A library's macros are its own: two libraries may each define a macro of
;; the same name, as (rapid match) and (rapid rbtree) each define
;; `compile-pattern` for their own use, and each library's macros expand into
;; its own. A program's imported macro is the one it imported, whatever other
;; libraries loaded later define under its name, until the program defines
;; the name itself.
(define-library (library-macro-tests first-namesake)
  (export lmt-use-first lmt-namesake)
  (import (scheme base))
  (begin
    (define-syntax lmt-shared-name
      (syntax-rules ()
        ((_) 'the-firsts)))
    (define-syntax lmt-use-first
      (syntax-rules ()
        ((_) (lmt-shared-name))))
    (define-syntax lmt-namesake
      (syntax-rules ()
        ((_) 'the-first-namesake)))))

(import (library-macro-tests first-namesake))

(define-library (library-macro-tests second-namesake)
  (export lmt-use-second)
  (import (scheme base))
  (begin
    (define-syntax lmt-shared-name
      (syntax-rules ()
        ((_) 'the-seconds)))
    (define-syntax lmt-use-second
      (syntax-rules ()
        ((_) (lmt-shared-name))))
    (define-syntax lmt-namesake
      (syntax-rules ()
        ((_) 'the-second-namesake)))))

(import (library-macro-tests second-namesake))

(test-group "libraries' macros of the same name"

  (test "the library loaded first expands into its own"
    'the-firsts
    (lmt-use-first))

  (test "the library loaded second expands into its own"
    'the-seconds
    (lmt-use-second))

  (test "a program's imported macro is the one it imported"
    'the-first-namesake
    (lmt-namesake)))

(define-syntax lmt-namesake
  (syntax-rules ()
    ((_) 'the-programs-namesake)))

(test-group "a program's own definition of an imported macro's name"

  (test "is the program's"
    'the-programs-namesake
    (lmt-namesake)))
