;; build_ahead.scm -- compiles a program ahead of time, with every library it
;; uses, so that it runs with no interpreter (scripts/lib/ahead.scm).
;;
;;     node repl.js -I scripts/lib scripts/build_ahead.scm PROGRAM OUTPUT
;;
;; Writes OUTPUT, a JavaScript module whose default export `runProgram` in
;; src/compiler/ahead.js runs; or, if the program cannot run so, says why on
;; the error port and exits with status 1. The libraries are read from the
;; program's directory, then from those the bundle ships.

(import (scheme base)
        (scheme write)
        (scheme file)
        (scheme process-context)
        (srfi 1)
        (only (scheme primitives) %read-forms)
        (scheme-js prebuild)
        (scheme-js ahead))

(define arguments (command-line))

(if (< (length arguments) 3)
    (begin (display "usage: build_ahead.scm PROGRAM OUTPUT" (current-error-port))
           (newline (current-error-port))
           (exit 1)))

(define program-file (list-ref arguments (- (length arguments) 2)))
(define output-file (list-ref arguments (- (length arguments) 1)))

;; /**
;;  * The directory a file is in, or "." for a file named alone.
;;  * @param {string} file - The file.
;;  * @returns {string}
;;  */
(define (directory-of file)
  (let loop ((i (- (string-length file) 1)))
    (cond ((< i 0) ".")
          ((char=? (string-ref file i) #\/) (substring file 0 i))
          (else (loop (- i 1))))))

(define build
  (build-program (%read-forms (file-text program-file) program-file #f)
                 (source-reader (list (directory-of program-file) "src/core/scheme" "src/extras/scheme"))))

(if (pair? (program-build-refusals build))
    (begin
      (for-each (lambda (reason)
                  (display reason (current-error-port))
                  (newline (current-error-port)))
                (program-build-refusals build))
      (exit 1)))

(call-with-output-file output-file
  (lambda (port) (write-string (render-program build program-file) port)))
