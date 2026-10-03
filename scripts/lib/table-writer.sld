;; (scheme-js table-writer) library
;;
;; Writing the prebuilt tables -- the shipped libraries' and the compiler's own
;; -- as JavaScript modules: what `installLibraryTable` in
;; src/compiler/prebuilt.js reads. A build tool, used by
;; scripts/generate_compiled_libraries.js and scripts/generate_compiled_compiler.js,
;; and not shipped. Its procedures are in table_writer.scm.

(define-library (scheme-js table-writer)
  (import (scheme base)
          (only (srfi 152) string-split string-join))
  (export render-tables constants-expression constant-expression
          json-string json-strings)
  (include "table_writer.scm"))
