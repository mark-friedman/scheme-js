;; Keyword rename tests
;;
;; R7RS 5.2 and 5.6.1: an import set's `rename` and `prefix`, and an export
;; spec's `rename`, apply to every identifier a library exports, syntactic
;; keywords included. A library that defines its own `quasiquote` imports the
;; standard one under another name to build it on, as (rapid quasiquote)
;; does; (rapid match) imports `...` as `ellipsis`, so that a pattern of its
;; own can name the user's ellipsis as a literal.
;;
;; The libraries are written inline, and their names and the names they bind
;; are unusual, since test files share an environment.

(define-library (keyword-rename-tests renamer)
  (export krt-quasi krt-choose krt-if-alias krt-dots-literal)
  (import (rename (scheme base)
                  (quasiquote krt-qq)
                  (if krt-if)
                  (... krt-dots)))
  (begin
    ;; A renamed special form, used in a template.
    (define-syntax krt-quasi
      (syntax-rules ()
        ((_ x) (krt-qq (a (unquote x) c)))))

    ;; A renamed special form, used in the library's own code.
    (define (krt-choose t) (krt-if t 'yes 'no))

    (define-syntax krt-if-alias
      (syntax-rules ()
        ((_ t a b) (krt-if t a b))))

    ;; A pattern literal naming the ellipsis by its new name matches the
    ;; ellipsis as the macro's user writes it.
    (define-syntax krt-dots-literal
      (syntax-rules (krt-dots)
        ((_ a krt-dots) 'an-ellipsis)
        ((_ a b) 'something-else)))))

(define-library (keyword-rename-tests exporter)
  (export (rename krt-internal-qq krt-exported-qq)
          (rename krt-internal-swap krt-exported-swap))
  (import (rename (scheme base) (quasiquote krt-internal-qq)))
  (begin
    (define-syntax krt-internal-swap
      (syntax-rules ()
        ((_ a b) (list b a))))))

(import (keyword-rename-tests renamer)
        (keyword-rename-tests exporter))

(test-group "keywords renamed on import"

  (test "a renamed quasiquote, in a library macro's template"
    '(a 2 c)
    (krt-quasi (+ 1 1)))

  (test "a renamed if, in the library's own code"
    'no
    (krt-choose #f))

  (test "a renamed if, in a library macro's template"
    1
    (krt-if-alias #t 1 2))

  (test "a literal naming the renamed ellipsis matches the ellipsis"
    'an-ellipsis
    (krt-dots-literal x ...))

  (test "a literal naming the renamed ellipsis matches nothing else"
    'something-else
    (krt-dots-literal x y)))

(import (rename (only (scheme base) when) (when krt-when)))
(import (prefix (only (scheme base) unless) krt-p:))
(import (rename (only (scheme base) else =>) (else krt-else) (=> krt-arrow)))

(test-group "keywords renamed on import into a program"

  (test "a renamed macro"
    'ran
    (krt-when #t 'ran))

  (test "a prefixed macro"
    'ran
    (krt-p:unless #f 'ran))

  ;; (scheme base) exports the auxiliary syntax its forms take, so that it
  ;; can be renamed (R7RS Appendix A).
  (test "a renamed else, in cond"
    'otherwise
    (cond (#f 'first) (krt-else 'otherwise)))

  (test "a renamed =>, in cond"
    2
    (cond ((+ 1 1) krt-arrow (lambda (x) x)) (krt-else 'otherwise))))

(test-group "keywords renamed on export"

  (test "a special form exported under another name"
    '(1 2)
    (krt-exported-qq (1 (unquote (+ 1 1)))))

  (test "a macro exported under another name"
    '(2 1)
    (krt-exported-swap 1 2)))

;; Macros are defined by name, but a name a keyword was imported under is
;; that keyword, as it was when imported, whatever its own name is later
;; defined as: a library defining its own `quasiquote` imports the standard
;; one as another name to build it on.
(define-library (keyword-rename-tests first)
  (export krt-m)
  (import (scheme base))
  (begin
    (define-syntax krt-m
      (syntax-rules ()
        ((_) 'the-first-krt-m)))))

(define-library (keyword-rename-tests user)
  (export krt-call-original)
  (import (scheme base)
          (rename (keyword-rename-tests first) (krt-m krt-original-m)))
  (begin
    (define-syntax krt-call-original
      (syntax-rules ()
        ((_) (krt-original-m))))))

(define-library (keyword-rename-tests redefiner)
  (export krt-m)
  (import (scheme base))
  (begin
    (define-syntax krt-m
      (syntax-rules ()
        ((_) 'the-second-krt-m)))))

(import (keyword-rename-tests user) (keyword-rename-tests redefiner))

(test-group "a renamed keyword is the one imported"

  (test "though another library defines its name again"
    'the-first-krt-m
    (krt-call-original))

  (test "while its name is the other library's"
    'the-second-krt-m
    (krt-m)))

;; A library's macro is exported as a macro though JavaScript has a global of
;; the same name: browsers now define `when`, which (scheme base) exports.
(js-eval "globalThis['krt-js-global'] = function () { return 'the JavaScript global'; }")

(define-library (keyword-rename-tests shadowed)
  (export krt-js-global)
  (import (scheme base))
  (begin
    (define-syntax krt-js-global
      (syntax-rules ()
        ((_ x) 'the-macro)))))

(import (rename (keyword-rename-tests shadowed) (krt-js-global krt-renamed-shadowed)))

(test-group "a macro named as a JavaScript global"

  (test "is exported as the macro"
    'the-macro
    (krt-renamed-shadowed 1)))
