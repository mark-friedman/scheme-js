;; (scheme-js ahead) library
;;
;; Compiling a whole program ahead of time, with every library it uses, so
;; that it runs with no interpreter, expander, reader or library system: each
;; top-level form compiled, and written as a table that src/compiler/ahead.js
;; runs. A build tool, run from the CLI, and not shipped. Its procedures are
;; in ahead.scm.

(define-library (scheme-js ahead)
  (import (scheme base)
          (scheme write)
          (srfi 1)
          (only (srfi 152) string-join string-split)
          (only (scheme-js library-system) parse-define-library library-definition-imports
                library-definition-exports library-definition-declaration-files
                import-set-library-name import-set-steps imported-name parse-import-set
                program-parts)
          (only (scheme primitives) %environment-define!)
          (scheme-js compiler)
          (scheme-js compiler build)
          (scheme-js prebuild)
          (scheme-js table-writer))
  (export top-level-items build-program program-build? program-build-refusals render-program
          build-program-file)
  (include "ahead.scm"))
