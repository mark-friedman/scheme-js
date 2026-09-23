;; (srfi 125) library
;;
;; Intermediate hash tables, over SRFI 128 comparators. The table logic is
;; Scheme; the constant-time store underneath it is a JavaScript `Map`, reached
;; through the `%hash-store-*` primitives in src/extras/primitives/hash_table.js.
;;
;; The file is `125.sld` because every library resolver finds a library's
;; file by the last part of its name.

(define-library (srfi 125)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 128))
  (export
    ;; Constructors
    make-hash-table hash-table hash-table-unfold alist->hash-table
    ;; Predicates
    hash-table? hash-table-contains? hash-table-exists? hash-table-empty?
    hash-table=? hash-table-mutable?
    ;; Accessors
    hash-table-ref hash-table-ref/default
    ;; Mutators
    hash-table-set! hash-table-delete! hash-table-intern! hash-table-update!
    hash-table-update!/default hash-table-pop! hash-table-clear!
    ;; The whole hash table
    hash-table-size hash-table-keys hash-table-values hash-table-entries
    hash-table-find hash-table-count
    ;; Mapping and folding
    hash-table-map hash-table-for-each hash-table-walk hash-table-map!
    hash-table-map->list hash-table-fold hash-table-prune!
    ;; Copying and conversion
    hash-table-copy hash-table-empty-copy hash-table->alist
    ;; Hash tables as sets
    hash-table-union! hash-table-merge! hash-table-intersection!
    hash-table-difference! hash-table-xor!
    ;; Hash functions and reflectivity (deprecated)
    hash string-hash string-ci-hash hash-by-identity
    hash-table-equivalence-function hash-table-hash-function)
  (include "hash_table.scm"))
