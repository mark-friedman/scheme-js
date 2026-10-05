;; (scheme-js procedural-macros) library
;;
;; Macros whose transformers are procedures, which R7RS-small does not have:
;; `er-macro-transformer`, explicit renaming (Clinger, 1991), hygienic where
;; its procedure renames; and `define-macro`, the legacy form, which renames
;; nothing. A program or library that imports nothing has both; one that
;; imports has them if it imports this library (docs/hygiene.md).

(define-library (scheme-js procedural-macros)
  (export er-macro-transformer define-macro))
