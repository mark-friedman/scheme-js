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

;; `read-char` and `peek-char` (R7RS 6.13.2) return characters, and a
;; character outside the Basic Multilingual Plane is one character, as it is
;; to `string-length`, although a JavaScript string holds it as two code units.
;; `read-string` counts characters the same way.

(test-group "read-char and peek-char"

  (test "read-char returns a character"
    #t
    (char? (read-char (open-input-string "abc"))))

  (test "read-char returns the next character"
    #\a
    (read-char (open-input-string "abc")))

  (test "peek-char returns a character"
    #\a
    (peek-char (open-input-string "abc")))

  (test "peek-char does not consume the character"
    '(#\a #\a #\b)
    (let* ((p (open-input-string "ab"))
           (peeked (peek-char p))
           (first (read-char p))
           (second (read-char p)))
      (list peeked first second)))

  (test "the end of the input is the end-of-file object"
    '(#t #t)
    (let ((p (open-input-string "")))
      (list (eof-object? (peek-char p)) (eof-object? (read-char p)))))

  (test "a character outside the BMP is read as one character"
    '(#\x1F600 #\b)
    (let* ((p (open-input-string "\x1F600;b"))
           (first (read-char p))
           (second (read-char p)))
      (list first second)))

  (test "peek-char sees a character outside the BMP whole"
    #\x1F600
    (peek-char (open-input-string "\x1F600;")))

  (test "read-string counts characters, not code units"
    '("\x1F600;b" "c")
    (let* ((p (open-input-string "\x1F600;bc"))
           (first (read-string 2 p))
           (rest (read-string 5 p)))
      (list first rest))))

;; This implementation's parameter objects take a new value when called with
;; one, and the current ports do the same, which is how the CLI makes the
;; process's standard input, output and error its current ports.

;; /**
;;  * Calls a thunk with a port as one of the current ports, putting the old
;;  * one back however the thunk exits.
;;  *
;;  * @param {procedure} current - `current-input-port`, `current-output-port`
;;  *   or `current-error-port`.
;;  * @param {port} port - The port.
;;  * @param {procedure} thunk - Called with no arguments.
;;  * @returns {*} What the thunk returns.
;;  */
(define (with-current current port thunk)
  (let ((old (current)))
    (dynamic-wind
      (lambda () (current port))
      thunk
      (lambda () (current old)))))

(test-group "current-input-port given a port"

  (test "the port given becomes the current input port"
    #t
    (let ((p (open-input-string "")))
      (with-current current-input-port p (lambda () (eq? p (current-input-port))))))

  (test "reading with no port reads the port given"
    '(#\x "yz")
    (with-current current-input-port (open-input-string "xyz")
      (lambda ()
        (let* ((c (read-char))
               (rest (read-line)))
          (list c rest)))))

  (test "the old port can be put back"
    #t
    (let ((old (current-input-port)))
      (with-current current-input-port (open-input-string "") (lambda () #f))
      (eq? old (current-input-port))))

  (test-error "rejects what is not a port"
    "current-input-port: expected input port"
    (current-input-port 5))

  (test-error "rejects an output port"
    "current-input-port: expected input port"
    (current-input-port (open-output-string))))

(test-group "current-output-port given a port"

  (test "the port given becomes the current output port"
    #t
    (let ((p (open-output-string)))
      (with-current current-output-port p (lambda () (eq? p (current-output-port))))))

  (test "writing with no port writes the port given"
    "x 1\n\"y\""
    (let ((p (open-output-string)))
      (with-current current-output-port p
        (lambda ()
          (write-char #\x)
          (display " ")
          (display 1)
          (newline)
          (write "y")))
      (get-output-string p)))

  (test "the old port can be put back"
    #t
    (let ((old (current-output-port)))
      (with-current current-output-port (open-output-string) (lambda () #f))
      (eq? old (current-output-port))))

  (test-error "rejects what is not a port"
    "current-output-port: expected output port"
    (current-output-port 5))

  (test-error "rejects an input port"
    "current-output-port: expected output port"
    (current-output-port (open-input-string ""))))

(test-group "current-error-port given a port"

  (test "the port given becomes the current error port"
    "oops"
    (let ((p (open-output-string)))
      (with-current current-error-port p
        (lambda () (display "oops" (current-error-port))))
      (get-output-string p)))

  (test "the old port can be put back"
    #t
    (let ((old (current-error-port)))
      (with-current current-error-port (open-output-string) (lambda () #f))
      (eq? old (current-error-port))))

  (test-error "rejects an input port"
    "current-error-port: expected output port"
    (current-error-port (open-input-string ""))))
