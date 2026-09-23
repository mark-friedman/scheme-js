;; (srfi 128) library
;;
;; Comparators: a type test, an equality predicate, an ordering predicate and a
;; hash function, bundled as one object. SRFI 125 hash tables are built from
;; them. The implementation is Scheme except for the hash functions that need
;; primitive access to their argument (`string-hash`, `string-ci-hash`,
;; `number-hash`), which are JavaScript primitives re-exported here.
;;
;; The file is `128.sld` because every library resolver finds a library's
;; file by the last part of its name.

(define-library (srfi 128)
  (import (scheme base)
          (scheme char)
          (scheme complex))
  (export
    ;; Predicates
    comparator? comparator-ordered? comparator-hashable?
    ;; Constructors
    make-comparator make-pair-comparator make-list-comparator
    make-vector-comparator make-eq-comparator make-eqv-comparator
    make-equal-comparator
    ;; Standard hash functions
    boolean-hash char-hash char-ci-hash string-hash string-ci-hash
    symbol-hash number-hash
    ;; Bounds and salt
    hash-bound hash-salt
    ;; Default comparators
    make-default-comparator default-hash comparator-register-default!
    ;; Accessors and invokers
    comparator-type-test-predicate comparator-equality-predicate
    comparator-ordering-predicate comparator-hash-function
    comparator-test-type comparator-check-type comparator-hash
    ;; Comparison predicates
    =? <? >? <=? >=?
    ;; Syntax
    comparator-if<=>)
  (include "comparator.scm"))
