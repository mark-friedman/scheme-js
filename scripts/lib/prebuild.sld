;; (scheme-js prebuild) library
;;
;; What the two build steps that write the prebuilt tables share
;; (scripts/generate_compiled_libraries.scm, the shipped libraries';
;; scripts/generate_compiled_compiler.scm, the compiler's own): reading the
;; libraries' files, noting each top-level form a library runs with its core
;; form, making a library's table from the code the compiler generates for it,
;; and saying what was made. A build tool, run from the CLI, and not shipped.
;; Its procedures are in prebuild.scm.

(define-library (scheme-js prebuild)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (srfi 1)
          (only (srfi 152) string-join string-split)
          (scheme-js interop)
          (only (scheme-js library-system) parse-define-library requirement-met?
                library-definition-includes library-definition-includes-ci
                library-definition-declaration-files)
          (only (scheme primitives) %read-forms %environment-own-value)
          (scheme-js compiler)
          (scheme-js compiler build)
          (scheme-js table-writer))
  (export file-text source-reader source-locator relative-path library-file-names library-resolver library-files library-declaration library-definition
          library-key
          declared-library-name say
          note! take-noted!
          library-table library-table? library-table-key library-table-fingerprint library-table-files
          library-table-entries
          library-table-restore library-table-restored library-table-forms
          library-table-declined library-table-unserializable
          make-library-table library-table-for install-table-code! library-table-list
          write-tables! report-restoring report-unserializable)
  (include "prebuild.scm"))
