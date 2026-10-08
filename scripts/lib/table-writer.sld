;; (scheme-js table-writer) library
;;
;; Writing the prebuilt tables -- the shipped libraries' and the compiler's own
;; -- as JavaScript modules: what `installLibraryTable` in
;; src/compiler/prebuilt.js reads. A build tool, used by the build steps
;; (scripts/lib/prebuild.scm) and scripts/pin_seed.scm, and not shipped. Its
;; procedures are in table_writer.scm.

(define-library (scheme-js table-writer)
  (import (scheme base)
          (only (scheme complex) real-part imag-part)
          (only (scheme inexact) nan? infinite?)
          (only (srfi 152) string-split string-join)
          (only (scheme-js interop) js-undefined? js-ref))
  (export render-tables constants-expression constant-expression json-datum
          json-string json-strings
          procedure-definition-name macro-definition-name restore-sequence restore-writable?)
  (include "table_writer.scm"))
