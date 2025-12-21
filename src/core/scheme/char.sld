;; R7RS (scheme char) library
;; 
;; Character procedures that are not in (scheme base).
;; Per R7RS Appendix A.

(define-library (scheme char)
  (import (scheme primitives))
  
  (export
    ;; Case-insensitive comparison
    char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
    
    ;; Character class predicates
    char-alphabetic? char-numeric? char-whitespace?
    char-upper-case? char-lower-case?
    
    ;; Case conversion
    char-upcase char-downcase char-foldcase
    
    ;; Digit value
    digit-value
  )
  
  (begin
    ;; No definitions needed - primitives are injected by the runtime
  ))
