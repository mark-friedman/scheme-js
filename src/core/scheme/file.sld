;; R7RS (scheme file) library
;;
;; Provides file I/O operations (Node.js only).
;; Per R7RS §6.13.

(define-library (scheme file)
  (import (scheme primitives))
  ;; The two that call a procedure with a port are Scheme, beside
  ;; `call-with-port`, in ports.scm.
  (import (only (scheme core) call-with-input-file call-with-output-file))
  
  (export
    open-input-file
    open-output-file
    call-with-input-file
    call-with-output-file
    file-exists?
    delete-file
  )
  
  (begin
    ;; The rest are primitives, which work in Node.js only: in a browser
    ;; they raise errors.
  ))
