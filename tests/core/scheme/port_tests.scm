;; Port procedure tests
;;
;; `call-with-port` (R7RS 6.13.1): calls a procedure with a port, closes the
;; port if the procedure returns, and returns what it returned.

(test-group "call-with-port"

  (test "returns what the procedure returns"
    "x"
    (call-with-port (open-output-string)
      (lambda (p) (write 'x p) (get-output-string p))))

  (test "closes the port when the procedure returns"
    #f
    (let ((p (open-input-string "abc")))
      (call-with-port p read-char)
      (input-port-open? p)))

  (test "passes the port to the procedure"
    #t
    (let ((p (open-input-string "abc")))
      (call-with-port p (lambda (q) (eq? p q)))))

  (test "returns every value the procedure returns"
    '(1 2)
    (call-with-values
      (lambda () (call-with-port (open-input-string "") (lambda (p) (values 1 2))))
      list))

  ;; R7RS: a port the procedure escapes from is not closed automatically,
  ;; since the escape may be re-entered.
  (test "leaves the port open when the procedure escapes"
    #t
    (let ((p (open-input-string "abc")))
      (call/cc (lambda (k) (call-with-port p (lambda (q) (k 'escaped)))))
      (input-port-open? p)))

  (test-error "rejects what is not a port"
    "call-with-port: expected port"
    (call-with-port 5 (lambda (p) p)))

  (test-error "rejects what is not a procedure"
    "call-with-port: expected procedure"
    (call-with-port (open-input-string "") 5)))
