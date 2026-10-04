;;; load.scm -- (scheme load)

;; /**
;;  * Reads the forms of a file and evaluates each, in order (R7RS 6.14).
;;  * @param {string} filename - The file.
;;  * @param {environment} [environment] - Where they are evaluated; the
;;  *   interaction environment if left out.
;;  * @returns {unspecified}
;;  */
(define (load filename . environment)
  (if (not (string? filename)) (error "load: expected string" filename))
  (let ((env (if (pair? environment) (car environment) (interaction-environment))))
    (call-with-input-file filename
      (lambda (port)
        (let loop ((form (read port)))
          (if (not (eof-object? form))
              (begin (eval form env)
                     (loop (read port)))))))))
