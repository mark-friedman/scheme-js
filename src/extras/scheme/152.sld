;; (srfi 152) library
;;
;; String library (reduced): the index-based string procedures that fit
;; R7RS-small's own. Its procedures are Scheme, in string_lib.scm. Those
;; R7RS-small already defines -- which here accept the optional start and end
;; indices SRFI 152 describes -- are re-exported rather than defined again.
;;
;; `string-copy!` is not exported: strings are immutable JavaScript strings in
;; this implementation, which is a known deviation from R7RS-small, and
;; `string-set!` and `string-fill!` signal an error saying so.
;;
;; The file is `152.sld` because every library resolver finds a library's file
;; by the last part of its name.

(define-library (srfi 152)
  (import (scheme base)
          (scheme char))
  (export
    ;; Predicates
    string? string-null? string-every string-any
    ;; Constructors
    make-string string string-tabulate string-unfold string-unfold-right
    ;; Conversion
    string->vector string->list vector->string list->string reverse-list->string
    ;; Selection
    string-length string-ref substring string-copy
    string-take string-take-right string-drop string-drop-right
    string-pad string-pad-right string-trim string-trim-right string-trim-both
    ;; Replacement
    string-replace
    ;; Comparison
    string=? string<? string>? string<=? string>=?
    string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
    ;; Prefixes and suffixes
    string-prefix-length string-suffix-length string-prefix? string-suffix?
    ;; Searching
    string-index string-index-right string-skip string-skip-right
    string-contains string-contains-right
    ;; Concatenation
    string-append string-concatenate string-concatenate-reverse string-join
    ;; Fold and map
    string-fold string-fold-right string-map string-for-each
    string-count string-filter string-remove
    ;; Replication and splitting
    string-replicate string-split
    ;; Input and output
    read-string write-string
    ;; Mutation
    string-set! string-fill!)
  (include "string_lib.scm"))
