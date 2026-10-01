;;; ports.scm -- Port procedures written in Scheme.
;;;
;;; The ports themselves, and the procedures that read and write them, are
;;; JavaScript primitives; what can be said in terms of those is said here.

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
