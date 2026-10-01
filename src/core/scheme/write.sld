;; R7RS (scheme write) library
;;
;; Provides output procedures for formatted writing.
;; Per R7RS §6.13.3.

(define-library (scheme write)
  ;; Scheme, over the runtime's writers, in ports.scm.
  (import (only (scheme core) display write write-shared write-simple))
  
  (export
    display
    write
    write-shared
    write-simple
  ))
