;; (scheme-js library-system) library
;;
;; The library system: parsing `define-library` and import sets, the names an
;; import set gives, the feature requirements `cond-expand` tests, the
;; registries of loaded libraries, loading and importing libraries, and
;; substituting values throughout them. Its procedures are Scheme, in
;; library_system.scm and substitute.scm.
;;
;; It is written with (scheme core) and (scheme control) alone, since it is
;; loaded before any other library, by the little that is JavaScript of the
;; library system (src/core/interpreter/library_seed.js): what it uses has to
;; be loadable without it. What it needs of the host besides -- the reader,
;; environments, the analyzer's tables -- are primitives
;; (src/core/primitives/library.js).
;;
;; The file is `library-system.sld` because every library resolver finds a
;; library's file by the last part of its name.

(define-library (scheme-js library-system)
  (import (scheme core)
          (scheme control))
  (export
    ;; define-library
    parse-define-library library-definition?
    library-definition-name library-definition-exports library-definition-imports
    library-definition-body library-definition-includes library-definition-includes-ci
    library-definition-declaration-files
    ;; Import sets
    parse-import-set import-set? import-set-library-name import-set-steps imported-name
    ;; Feature requirements
    requirement-met? standard-features registry-requirement-met?
    ;; Names and registries
    library-key make-library-registry library-registry? add-feature! registry-features
    registry-resolver set-registry-resolver! registry-load-hook set-registry-load-hook!
    registry-restorer set-registry-restorer!
    registered-exports registered-environment register-exports! registered-keys
    clear-registry!
    ;; Loading and importing
    make-loader registry-loader loader-registry load-library define-library!
    import-sets! import-into! syntactic-keyword?
    ;; Substituting values, and procedures compiled over closures
    substitute-library-values! make-debugged-programs record-compiled-over!
    compiled-over? interpret-compiled-over! switch-back-to-closure!
    ;; The files a load would read
    files-wanted definition-files-wanted define-library-parts)
  (include "library_system.scm"
           "substitute.scm"))
