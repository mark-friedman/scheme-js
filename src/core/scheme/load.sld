;; (scheme load) library - R7RS 6.14
;;
;; `load`: reading a file's forms and evaluating them, in Scheme, over
;; `read` and `eval`. Files are Node's: in a browser, opening one is an error.

(define-library (scheme load)
  (import (scheme base) (scheme read) (scheme eval) (scheme repl) (scheme file))
  (export load)
  (include "load.scm"))
