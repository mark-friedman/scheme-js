;; R7RS Compliance Tests - Section 6.13: Input and Output
;; Adapted from Chibi Scheme's R7RS test suite by Alex Shinn
;; NOTE: Uses (bytevector ...) function instead of #u8() literals

(test-group "6.13 Input and output"

  (test #t (port? (current-input-port)))
  (test #t (input-port? (current-input-port)))
  (test #t (output-port? (current-output-port)))
  (test #t (output-port? (current-error-port)))
  (test #t (input-port? (open-input-string "abc")))
  (test #t (output-port? (open-output-string)))

  (test #t (textual-port? (open-input-string "abc")))
  (test #t (textual-port? (open-output-string)))
  (test #t (binary-port? (open-input-bytevector (bytevector 0 1 2))))
  (test #t (binary-port? (open-output-bytevector)))

  (test #t (input-port-open? (open-input-string "abc")))
  (test #t (output-port-open? (open-output-string)))

  (test #f
      (let ((in (open-input-string "abc")))
        (close-input-port in)
        (input-port-open? in)))

  (test #f
      (let ((out (open-output-string)))
        (close-output-port out)
        (output-port-open? out)))

  (test #f
      (let ((out (open-output-string)))
        (close-port out)
        (output-port-open? out)))

  (test #t (eof-object? (eof-object)))
  (test #t (eof-object? (read (open-input-string ""))))
  (test #t (char-ready? (open-input-string "42")))
  (test 42 (read (open-input-string " 42 ")))

  (test #t (eof-object? (read-char (open-input-string ""))))
  (test #\a (read-char (open-input-string "abc")))

  (test #t (eof-object? (read-line (open-input-string ""))))
  (test "abc" (read-line (open-input-string "abc")))
  (test "abc" (read-line (open-input-string "abc\ndef\n")))

  (test #t (eof-object? (read-string 3 (open-input-string ""))))
  (test "abc" (read-string 3 (open-input-string "abcd")))
  (test "abc" (read-string 3 (open-input-string "abc\ndef\n")))

  (test "abc"
      (let ((out (open-output-string)))
        (write 'abc out)
        (get-output-string out)))

  (test "abc def"
      (let ((out (open-output-string)))
        (display "abc def" out)
        (get-output-string out)))

  (test "abc"
      (let ((out (open-output-string)))
        (display #\a out)
        (display "b" out)
        (display #\c out)
        (get-output-string out)))

  (test #t
        (let* ((out (open-output-string))
               (r (begin (newline out) (get-output-string out))))
          (or (equal? r "\n") (equal? r "\r\n"))))

  (test "abc def"
      (let ((out (open-output-string)))
        (write-string "abc def" out)
        (get-output-string out)))

  (test "def"
      (let ((out (open-output-string)))
        (write-string "abc def" out 4)
        (get-output-string out)))

  (test "c d"
      (let ((out (open-output-string)))
        (write-string "abc def" out 2 5)
        (get-output-string out)))

  (test ""
    (let ((out (open-output-string)))
      (flush-output-port out)
      (get-output-string out)))

  (test #t (eof-object? (read-u8 (open-input-bytevector (bytevector)))))
  (test 1 (read-u8 (open-input-bytevector (bytevector 1 2 3))))

  (test #t (eof-object? (read-bytevector 3 (open-input-bytevector (bytevector)))))
  (test #t (u8-ready? (open-input-bytevector (bytevector 1))))
  (test (bytevector 1) (read-bytevector 3 (open-input-bytevector (bytevector 1))))
  (test (bytevector 1 2) (read-bytevector 3 (open-input-bytevector (bytevector 1 2))))
  (test (bytevector 1 2 3) (read-bytevector 3 (open-input-bytevector (bytevector 1 2 3))))
  (test (bytevector 1 2 3) (read-bytevector 3 (open-input-bytevector (bytevector 1 2 3 4))))

  (test (bytevector 1 2 3)
    (let ((out (open-output-bytevector)))
      (write-u8 1 out)
      (write-u8 2 out)
      (write-u8 3 out)
      (get-output-bytevector out)))

  (test (bytevector 1 2 3 4 5)
    (let ((out (open-output-bytevector)))
      (write-bytevector (bytevector 1 2 3 4 5) out)
      (get-output-bytevector out)))

  (test (bytevector 3 4 5)
    (let ((out (open-output-bytevector)))
      (write-bytevector (bytevector 1 2 3 4 5) out 2)
      (get-output-bytevector out)))

  (test (bytevector 3 4)
    (let ((out (open-output-bytevector)))
      (write-bytevector (bytevector 1 2 3 4 5) out 2 4)
      (get-output-bytevector out)))

  (test (bytevector)
    (let ((out (open-output-bytevector)))
      (flush-output-port out)
      (get-output-bytevector out)))

  (test "((1 2 3) (1 2 3))"
      (let ((out (open-output-string))
            (x (list 1 2 3)))
        (write (list x x) out)
        (get-output-string out)))

  (test "((1 2 3) (1 2 3))"
      (let ((out (open-output-string))
            (x (list 1 2 3)))
        (write-simple (list x x) out)
        (get-output-string out)))
)
