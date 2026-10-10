;;; ports.scm -- Port procedures written in Scheme.
;;;
;;; The ports themselves, and the procedures that read and write a port they
;;; are given (`%read-char` and the rest), are JavaScript primitives; what can
;;; be said in terms of those is said here: the current ports, the procedures
;;; that read and write them by default, and those that call a procedure with
;;; a port.

;; /**
;;  * Calls a procedure with a port, and closes the port if the procedure
;;  * returns (R7RS 6.13.1).
;;  *
;;  * A port the procedure escapes from is left open: R7RS allows closing it only
;;  * when it can be proved that the port will never be used again, and an escape
;;  * may be re-entered. So no `dynamic-wind`, and the port is closed only on the
;;  * way back from an ordinary return.
;;  *
;;  * @param {port} port - The port.
;;  * @param {procedure} proc - Called with the port.
;;  * @returns {*} Every value `proc` returns.
;;  */
(define (call-with-port port proc)
  (if (not (port? port))
      (error "call-with-port: expected port" port))
  (if (not (procedure? proc))
      (error "call-with-port: expected procedure" proc))
  (call-with-values
    (lambda () (proc port))
    (lambda results
      (close-port port)
      (apply values results))))

;; /**
;;  * Opens a file for input, calls a procedure with the port, and closes the
;;  * port if the procedure returns, as `call-with-port` does (R7RS 6.13.1).
;;  *
;;  * The procedure is checked before the file is opened, so that a mistake
;;  * leaves no port open.
;;  *
;;  * @param {string} filename - The file.
;;  * @param {procedure} proc - Called with the port.
;;  * @returns {*} Every value `proc` returns.
;;  */
(define (call-with-input-file filename proc)
  (if (not (procedure? proc))
      (error "call-with-input-file: expected procedure" proc))
  (call-with-port (open-input-file filename) proc))

;; /**
;;  * Opens a file for output, calls a procedure with the port, and closes the
;;  * port if the procedure returns, as `call-with-port` does (R7RS 6.13.1).
;;  * @param {string} filename - The file.
;;  * @param {procedure} proc - Called with the port.
;;  * @returns {*} Every value `proc` returns.
;;  */
(define (call-with-output-file filename proc)
  (if (not (procedure? proc))
      (error "call-with-output-file: expected procedure" proc))
  (call-with-port (open-output-file filename) proc))

;; ---------------------------------------------------------------------------
;; The current ports (R7RS 6.13.1)
;; ---------------------------------------------------------------------------
;;
;; Parameter objects, so that `parameterize` binds one for the dynamic extent
;; of its body. Each begins as the console port the runtime keeps
;; (`%console-output-port` and the others, in
;; src/core/primitives/io/port_primitives.js), and takes only a port of its kind.
;;
;; Each is a top-level procedure over a global cell (`parameter-dispatch` in
;; parameter.scm) rather than what `make-parameter` returns: a closure
;; `make-parameter` makes as the library loads is interpreted, and every write
;; to the current port asks for it.

;; /**
;;  * The converters: the port, if it is of the parameter's kind, and otherwise
;;  * an error naming the parameter.
;;  * @param {*} port - What the parameter is to be given.
;;  * @returns {port} The port.
;;  */
(define (as-current-input-port port)
  (if (input-port? port) port (error "current-input-port: expected input port" port)))
(define (as-current-output-port port)
  (if (output-port? port) port (error "current-output-port: expected output port" port)))
(define (as-current-error-port port)
  (if (output-port? port) port (error "current-error-port: expected output port" port)))

(define current-input-port-cell (parameter-cell as-current-input-port (%console-input-port)))
(define current-output-port-cell (parameter-cell as-current-output-port (%console-output-port)))
(define current-error-port-cell (parameter-cell as-current-error-port (%console-error-port)))

(define (current-input-port . args) (parameter-dispatch current-input-port-cell args))
(define (current-output-port . args) (parameter-dispatch current-output-port-cell args))
(define (current-error-port . args) (parameter-dispatch current-error-port-cell args))

;; /**
;;  * The current ports' values now: what `(current-input-port)` and the others
;;  * return, without the rest list a parameter object's call makes. Every
;;  * read or write of a current port asks, so it is kept to one call.
;;  * @returns {port}
;;  */
(define (the-current-input-port) (cdr (param-dynamic-lookup current-input-port-cell)))
(define (the-current-output-port) (cdr (param-dynamic-lookup current-output-port-cell)))

;; /**
;;  * A writer's port, checked here, once, to be an open textual output port,
;;  * for a writer that writes it piece by piece.
;;  * @param {string} who - The writer, for the error.
;;  * @param {*} port - The port its optional argument gives
;;  *   (`optional-port`).
;;  * @returns {port} The port.
;;  */
(define (textual-output-port who port)
  (cond ((not (and (output-port? port) (textual-port? port)))
         (error (string-append who ": expected textual output port") port))
        ((not (output-port-open? port))
         (error (string-append who ": port is closed") port))
        (else port)))

;; /**
;;  * The port an optional port argument gives -- the default if it was left
;;  * out -- or the error for too many arguments. A macro, so that the rest
;;  * parameter is only taken apart where the procedure is, which compiled code
;;  * does from the arguments, without making the list ("A rest parameter" in
;;  * src/compiler/emit.scm); handed to a procedure, it was made on every call.
;;  * @syntax (optional-port who rest default)
;;  */
(define-syntax optional-port
  (syntax-rules ()
    ((_ who rest default)
     (cond ((null? rest) default)
           ((null? (cdr rest)) (car rest))
           (else (error (string-append who ": too many arguments") rest))))))

;; ---------------------------------------------------------------------------
;; Reading and writing the current port by default (R7RS 6.13.2, 6.13.3)
;; ---------------------------------------------------------------------------
;;
;; Each takes an optional port, the current one if it is left out, and has the
;; runtime's procedure of the same name with `%` before it read or write it,
;; which checks the port.

(define (read-char . port)
  (%read-char (optional-port "read-char" port (the-current-input-port))))
(define (peek-char . port)
  (%peek-char (optional-port "peek-char" port (the-current-input-port))))
(define (char-ready? . port)
  (%char-ready? (optional-port "char-ready?" port (the-current-input-port))))
(define (read-line . port)
  (%read-line (optional-port "read-line" port (the-current-input-port))))
(define (read-string k . port)
  (%read-string k (optional-port "read-string" port (the-current-input-port))))
(define (read-u8 . port)
  (%read-u8 (optional-port "read-u8" port (the-current-input-port))))
(define (peek-u8 . port)
  (%peek-u8 (optional-port "peek-u8" port (the-current-input-port))))
(define (u8-ready? . port)
  (%u8-ready? (optional-port "u8-ready?" port (the-current-input-port))))
(define (read-bytevector k . port)
  (%read-bytevector k (optional-port "read-bytevector" port (the-current-input-port))))

;; /**
;;  * Reads bytes from a binary input port into part of a bytevector
;;  * (R7RS 6.13.2): as many as are there, up to the part's length.
;;  * @param {bytevector} target - Where the bytes go.
;;  * @param {port} [port] - The port; the current input port if left out.
;;  * @param {integer} [start=0] - The first position of the part.
;;  * @param {integer} [end] - The position after its last; the bytevector's
;;  *   length if left out.
;;  * @returns {integer|eof-object} How many bytes were read, or the end-of-file
;;  *   object if the port had none.
;;  */
(define (read-bytevector! target . options)
  (if (not (bytevector? target))
      (error "read-bytevector!: expected bytevector" target))
  (let* ((port (if (pair? options) (car options) (the-current-input-port)))
         (bounds (if (pair? options) (cdr options) '()))
         (start (if (pair? bounds) (car bounds) 0))
         (end (if (and (pair? bounds) (pair? (cdr bounds))) (cadr bounds) (bytevector-length target))))
    (if (and (pair? bounds) (pair? (cdr bounds)) (pair? (cddr bounds)))
        (error "read-bytevector!: too many arguments" options))
    (if (not (and (exact-integer? start) (exact-integer? end)
                  (<= 0 start end (bytevector-length target))))
        (error "read-bytevector!: range out of bounds" start end))
    (let ((bytes (if (= start end) (bytevector) (%read-bytevector (- end start) port))))
      (cond ((eof-object? bytes) bytes)
            (else (bytevector-copy! target start bytes)
                  (bytevector-length bytes))))))
(define (read . port)
  (%read (optional-port "read" port (the-current-input-port))))

(define (write-char char . port)
  (%write-char char (optional-port "write-char" port (the-current-output-port))))
(define (write-u8 byte . port)
  (%write-u8 byte (optional-port "write-u8" port (the-current-output-port))))
(define (newline . port)
  (%newline (optional-port "newline" port (the-current-output-port))))

;; `display` and the `write`s write a datum as the printer does (printer.scm),
;; which differs between them only in whether strings and characters are
;; written as themselves and which objects take datum labels.
(define (display obj . port)
  (print-datum obj (textual-output-port "display" (optional-port "display" port (the-current-output-port)))
               #t 'cycles))
(define (write obj . port)
  (print-datum obj (textual-output-port "write" (optional-port "write" port (the-current-output-port)))
               #f 'cycles))
(define (write-simple obj . port)
  (print-datum obj (textual-output-port "write-simple" (optional-port "write-simple" port (the-current-output-port)))
               #f 'none))
(define (write-shared obj . port)
  (print-datum obj (textual-output-port "write-shared" (optional-port "write-shared" port (the-current-output-port)))
               #f 'shared))
(define (flush-output-port . port)
  (%flush-output-port (optional-port "flush-output-port" port (the-current-output-port))))

;; /**
;;  * Calls a writer of part of a sequence -- `%write-string`, `%write-bytevector`
;;  * -- with a port and up to two bounds. R7RS has the port first, and the
;;  * bounds only after it; here a bound may also come first, with the port left
;;  * out, as this implementation has always accepted.
;;  * @param {string} who - The procedure, for the error.
;;  * @param {procedure} writer - The runtime's writer.
;;  * @param {*} sequence - What to write part of.
;;  * @param {list} rest - The optional arguments: a port, then the bounds.
;;  * @returns {unspecified}
;;  */
(define (write-part who writer sequence rest)
  (let* ((port-first (and (pair? rest) (output-port? (car rest))))
         (port (if port-first (car rest) (the-current-output-port)))
         (bounds (if port-first (cdr rest) rest)))
    (if (and (pair? bounds) (pair? (cdr bounds)) (pair? (cddr bounds)))
        (error (string-append who ": too many arguments") rest))
    (apply writer sequence port bounds)))

(define (write-string string . rest) (write-part "write-string" %write-string string rest))
(define (write-bytevector bytevector . rest) (write-part "write-bytevector" %write-bytevector bytevector rest))

;; ---------------------------------------------------------------------------
;; A file as the current port (R7RS 6.13.1)
;; ---------------------------------------------------------------------------

;; /**
;;  * Opens a file for input, makes it the current input port while a thunk
;;  * runs, and closes it if the thunk returns. An escape from the thunk puts
;;  * the old current port back, as `parameterize` does, and leaves the file's
;;  * port open, as `call-with-port` does.
;;  * @param {string} filename - The file.
;;  * @param {procedure} thunk - Called with no arguments.
;;  * @returns {*} Every value `thunk` returns.
;;  */
(define (with-input-from-file filename thunk)
  (if (not (procedure? thunk))
      (error "with-input-from-file: expected procedure" thunk))
  (let ((port (open-input-file filename)))
    (call-with-values
      (lambda () (parameterize ((current-input-port port)) (thunk)))
      (lambda results
        (close-port port)
        (apply values results)))))

;; /**
;;  * Opens a file for output, makes it the current output port while a thunk
;;  * runs, and closes it if the thunk returns, as `with-input-from-file` does
;;  * for input.
;;  * @param {string} filename - The file.
;;  * @param {procedure} thunk - Called with no arguments.
;;  * @returns {*} Every value `thunk` returns.
;;  */
(define (with-output-to-file filename thunk)
  (if (not (procedure? thunk))
      (error "with-output-to-file: expected procedure" thunk))
  (let ((port (open-output-file filename)))
    (call-with-values
      (lambda () (parameterize ((current-output-port port)) (thunk)))
      (lambda results
        (close-port port)
        (apply values results)))))
