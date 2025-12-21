;; R7RS (scheme write) library
;;
;; Provides output procedures for formatted writing.
;; Per R7RS §6.13.3.

(define-library (scheme write)
  (import (scheme primitives))
  
  (export
    display
    write
    ;; write-shared and write-simple are optional extensions
    ;; that we don't implement yet
  )
  
  (begin
    ;; Bindings come from primitives
  ))
