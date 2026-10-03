;; (scheme-js library-system) library
;;
;; The library system: parsing `define-library` and import sets, the names an
;; import set gives, and the feature requirements `cond-expand` tests. Its
;; procedures are Scheme, in library_system.scm.
;;
;; It is written with (scheme core) and (scheme control) alone, since it is
;; loaded before any other library, by the little that is JavaScript of the
;; library system: what it uses has to be loadable without it.
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
    requirement-met?)
  (include "library_system.scm"))
