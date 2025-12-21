;; R7RS (scheme file) library
;;
;; Provides file I/O operations (Node.js only).
;; Per R7RS §6.13.

(define-library (scheme file)
  (import (scheme primitives))
  
  (export
    open-input-file
    open-output-file
    call-with-input-file
    call-with-output-file
    file-exists?
    delete-file
  )
  
  (begin
    ;; All procedures are implemented as primitives
    ;; Note: These procedures only work in Node.js
    ;; Browser calls will raise errors
  ))
