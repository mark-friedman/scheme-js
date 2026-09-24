;; (srfi 1) library
;;
;; The list library. Its procedures are Scheme, in list_lib.scm. The ones
;; R7RS-small already defines compatibly -- `map`, `member`, `assoc` and the
;; rest, whose R7RS versions accept the extra arguments SRFI 1 describes -- are
;; re-exported from (scheme base) and (scheme cxr) rather than defined again.
;;
;; The file is `1.sld` because every library resolver finds a library's file
;; by the last part of its name.

(define-library (srfi 1)
  (import (scheme base)
          (scheme cxr))
  (export
    ;; Constructors
    cons list xcons cons* make-list list-tabulate list-copy circular-list iota
    ;; Predicates
    pair? null? proper-list? circular-list? dotted-list? not-pair? null-list? list=
    ;; Selectors
    car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    list-ref first second third fourth fifth sixth seventh eighth ninth tenth
    car+cdr take drop take-right drop-right take! drop-right!
    split-at split-at! last last-pair
    ;; Miscellaneous
    length length+ append concatenate reverse append! concatenate! reverse!
    append-reverse append-reverse! zip unzip1 unzip2 unzip3 unzip4 unzip5 count
    ;; Fold, unfold and map
    fold unfold pair-fold reduce fold-right unfold-right pair-fold-right
    reduce-right append-map append-map! pair-for-each filter-map map-in-order
    map for-each map!
    ;; Filtering and partitioning
    filter partition remove filter! partition! remove!
    ;; Searching
    member memq memv find find-tail any every list-index
    take-while drop-while take-while! span break span! break!
    ;; Deleting
    delete delete-duplicates delete! delete-duplicates!
    ;; Association lists
    assoc assq assv alist-cons alist-copy alist-delete alist-delete!
    ;; Lists as sets
    lset<= lset= lset-adjoin lset-union lset-union! lset-intersection
    lset-intersection! lset-difference lset-difference! lset-xor lset-xor!
    lset-diff+intersection lset-diff+intersection!
    ;; Primitive side effects
    set-car! set-cdr!)
  (include "list_lib.scm"))
