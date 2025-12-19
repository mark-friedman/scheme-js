;; R7RS (scheme cxr) library
;; 
;; Provides compound car/cdr accessors up to 4 levels deep.
;; Per R7RS Appendix A.

(define-library (scheme cxr)
  (import (scheme core))

  (export
    ;; Depth 2
    caar cadr cdar cddr
    
    ;; Depth 3
    caaar caadr cadar caddr
    cdaar cdadr cddar cdddr
    
    ;; Depth 4
    caaaar caaadr caadar caaddr
    cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr
    cddaar cddadr cdddar cddddr
  )
)
