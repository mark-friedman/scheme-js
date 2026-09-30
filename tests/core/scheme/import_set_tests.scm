;; Import set tests
;;
;; R7RS 5.6.1: an import set is a library name, or another import set
;; filtered by `only`, `except`, `prefix` or `rename`, nested in any order,
;; each filter applying to the names the set inside it provides. `rename`
;; takes (from to) pairs. And every library Appendix A defines can be
;; imported. The names bound here are unusual, since test files share an
;; environment.

(import (rename (scheme base) (car import-set-first) (cdr import-set-rest)))
(import (only (prefix (scheme base) import-set:) import-set:cadr))
(import (prefix (only (scheme base) caddr) import-set-p:))
(import (rename (prefix (scheme base) import-set-q:) (import-set-q:length import-set-count)))
(import (scheme inexact))
(import (prefix (scheme inexact) import-set-inexact:))

(test-group "import sets"

  (test "rename binds each (from to) pair's second name"
    '(1 (2))
    (list (import-set-first '(1 2)) (import-set-rest '(1 2))))

  (test "only filters the names a prefix inside it made"
    2
    (import-set:cadr '(1 2)))

  (test "prefix applies to what an only inside it kept"
    3
    (import-set-p:caddr '(1 2 3)))

  (test "rename renames a name a prefix inside it made"
    2
    (import-set-count '(1 2))))

(test-group "(scheme inexact)"

  ;; Only that each name is bound, which a missing export would not be: in
  ;; the environment the test runner shares, `log` has been rebound by an
  ;; earlier test before `(scheme primitives)` is built from it.
  (test "exports its twelve names"
    12
    (length
         (list import-set-inexact:acos import-set-inexact:asin
               import-set-inexact:atan import-set-inexact:cos
               import-set-inexact:exp import-set-inexact:finite?
               import-set-inexact:infinite? import-set-inexact:log
               import-set-inexact:nan? import-set-inexact:sin
               import-set-inexact:sqrt import-set-inexact:tan)))

  (test "finite?, imported from it"
    '(#t #f)
    (list (finite? 1.0) (finite? (/ 1.0 0.0)))))
