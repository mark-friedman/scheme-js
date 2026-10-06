;; R7RS Compliance Tests - Section 6.13 Input and output
;; Adapted from Chibi Scheme's R7RS test suite by Alex Shinn: the section as
;; chibi_original/r7rs-tests.scm has it, every test as Chibi wrote it, run with
;; Chibi's test forms and compared as Chibi compares (../test-equal.scm).

(test-group "6.13 Input and output"

(test #t (port? (current-input-port)))
(test #t (input-port? (current-input-port)))
(test #t (output-port? (current-output-port)))
(test #t (output-port? (current-error-port)))
(test #t (input-port? (open-input-string "abc")))
(test #t (output-port? (open-output-string)))

(test #t (textual-port? (open-input-string "abc")))
(test #t (textual-port? (open-output-string)))
(test #t (binary-port? (open-input-bytevector #u8(0 1 2))))
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

(test 'error
    (let ((in (open-input-string "abc")))
      (close-input-port in)
      (guard (exn (else 'error)) (read-char in))))

(test 'error
    (let ((out (open-output-string)))
      (close-output-port out)
      (guard (exn (else 'error)) (write-char #\c out))))

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

(let ((in (open-input-string (string #\x10F700 #\x10F701 #\x10F702))))
  (let* ((c0 (peek-char in))
         (c1 (read-char in))
         (c2 (read-char in))
         (c3 (read-char in)))
    (test #\x10F700 c0)
    (test #\x10F700 c1)
    (test #\x10F701 c2)
    (test #\x10F702 c3)))

(test (string #\x10F700)
    (let ((out (open-output-string)))
      (write-char #\x10F700 out)
      (get-output-string out)))

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

(test #t (eof-object? (read-u8 (open-input-bytevector #u8()))))
(test 1 (read-u8 (open-input-bytevector #u8(1 2 3))))

(test #t (eof-object? (read-bytevector 3 (open-input-bytevector #u8()))))
(test #t (u8-ready? (open-input-bytevector #u8(1))))
(test #u8(1) (read-bytevector 3 (open-input-bytevector #u8(1))))
(test #u8(1 2) (read-bytevector 3 (open-input-bytevector #u8(1 2))))
(test #u8(1 2 3) (read-bytevector 3 (open-input-bytevector #u8(1 2 3))))
(test #u8(1 2 3) (read-bytevector 3 (open-input-bytevector #u8(1 2 3 4))))

(test #t
    (let ((bv (bytevector 1 2 3 4 5)))
      (eof-object? (read-bytevector! bv (open-input-bytevector #u8())))))

(test #u8(6 7 8 9 10)
  (let ((bv (bytevector 1 2 3 4 5)))
    (read-bytevector! bv (open-input-bytevector #u8(6 7 8 9 10)) 0 5)
    bv))

(test #u8(6 7 8 4 5)
  (let ((bv (bytevector 1 2 3 4 5)))
    (read-bytevector! bv (open-input-bytevector #u8(6 7 8 9 10)) 0 3)
    bv))

(test #u8(1 2 3 6 5)
  (let ((bv (bytevector 1 2 3 4 5)))
    (read-bytevector! bv (open-input-bytevector #u8(6 7 8 9 10)) 3 4)
    bv))

(test #u8(1 2 3)
  (let ((out (open-output-bytevector)))
    (write-u8 1 out)
    (write-u8 2 out)
    (write-u8 3 out)
    (get-output-bytevector out)))

(test #u8(1 2 3 4 5)
  (let ((out (open-output-bytevector)))
    (write-bytevector #u8(1 2 3 4 5) out)
    (get-output-bytevector out)))

(test #u8(3 4 5)
  (let ((out (open-output-bytevector)))
    (write-bytevector #u8(1 2 3 4 5) out 2)
    (get-output-bytevector out)))

(test #u8(3 4)
  (let ((out (open-output-bytevector)))
    (write-bytevector #u8(1 2 3 4 5) out 2 4)
    (get-output-bytevector out)))

(test #u8()
  (let ((out (open-output-bytevector)))
    (flush-output-port out)
    (get-output-bytevector out)))

(test #t
    (and (member
          (let ((out (open-output-string))
                (x (list 1)))
            (set-cdr! x x)
            (write x out)
            (get-output-string out))
          ;; labels not guaranteed to be 0 indexed, spacing may differ
          '("#0=(1 . #0#)" "#1=(1 . #1#)"))
         #t))

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

(test #t
    (and (member (let ((out (open-output-string))
                       (x (list 1 2 3)))
                   (write-shared (list x x) out)
                   (get-output-string out))
                 '("(#0=(1 2 3) #0#)" "(#1=(1 2 3) #1#)"))
         #t))

)
