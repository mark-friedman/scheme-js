;; SRFI 48, intermediate format strings, from its reference implementation.
;;
;; The reference implementation (test/srfi-48.scm in the SRFI's repository,
;; pinned in benchmarks/corpus/manifest.json) is a file of definitions, not
;; a library; each test driver beside it defines what it needs for one
;; implementation and includes it. This is that driver as an R7RS library:
;; the corpus's SRFI 64 imports (srfi 48), and nothing on Snow-Fort provides
;; it. The include is resolved in the downloaded repository.

(define-library (srfi 48)
  (export format)
  (import (scheme base) (scheme char) (scheme write))
  (begin
    ;; The R5RS names the reference implementation uses, and SRFI 38's,
    ;; as R7RS has them.
    (define exact->inexact inexact)
    (define inexact->exact exact)
    (define write-with-shared-structure write-shared))
  (include "test/srfi-48.scm"))
