;; Mutable string tests
;;
;; R7RS 6.7: every string a procedure newly allocates may be changed with
;; `string-set!`, `string-fill!` and `string-copy!`, and the change is seen
;; through every reference to it. Literals and the strings `symbol->string`
;; returns are immutable. Two newly allocated strings are distinct objects, so
;; `eq?` tells them apart while `equal?` and `string=?` compare characters.

(import (prefix (srfi 125) string-mutation:))

(test-group "string-set! is seen through every reference"

  (test "the string itself"
    "yxx"
    (let ((s (make-string 3 #\x)))
      (string-set! s 0 #\y)
      s))

  (test "another name for it, and a list holding it"
    '("yxx" "yxx")
    (let* ((s (make-string 3 #\x))
           (t s)
           (l (list s)))
      (string-set! s 0 #\y)
      (list t (car l))))

  (test "a string reversed in place, reading and writing in turn"
    "olleh"
    (let ((s (string-copy "hello")))
      (let loop ((i 0) (j (- (string-length s) 1)))
        (when (< i j)
          (let ((c (string-ref s i)))
            (string-set! s i (string-ref s j))
            (string-set! s j c))
          (loop (+ i 1) (- j 1))))
      s)))

(test-group "every newly allocated string may be changed"

  (define (changed s)
    (string-set! s 0 #\*)
    s)

  (test "make-string" "*b" (changed (make-string 2 #\b)))
  (test "string" "*b" (changed (string #\a #\b)))
  (test "string-copy" "*b" (changed (string-copy "ab")))
  (test "string-append" "*b" (changed (string-append "a" "b")))
  (test "substring" "*b" (changed (substring "xab" 1 3)))
  (test "list->string" "*b" (changed (list->string (list #\a #\b))))
  (test "vector->string" "*b" (changed (vector->string (vector #\a #\b))))
  (test "number->string" "*2" (changed (number->string 12)))
  (test "string-upcase" "*B" (changed (string-upcase "ab")))
  (test "string-downcase" "*b" (changed (string-downcase "AB")))
  (test "string-foldcase" "*b" (changed (string-foldcase "AB")))
  (test "string-map" "*b" (changed (string-map char-downcase "AB")))
  (test "utf8->string" "*b" (changed (utf8->string (bytevector 97 98))))
  (test "get-output-string" "*b"
    (changed (let ((p (open-output-string))) (write-string "ab" p) (get-output-string p))))
  (test "read-string" "*b" (changed (read-string 2 (open-input-string "abc"))))
  (test "read-line" "*b" (changed (read-line (open-input-string "ab\ncd")))))

(test-group "what may not be changed"

  (define (refused thunk)
    (guard (e (#t 'refused)) (thunk) 'changed))

  (test "a literal" 'refused (refused (lambda () (string-set! "abc" 0 #\x))))
  (test "a literal, filled" 'refused (refused (lambda () (string-fill! "abc" #\x))))
  (test "a literal, copied into" 'refused (refused (lambda () (string-copy! "abc" 0 "xyz"))))
  (test "what symbol->string returns" 'refused
    (refused (lambda () (string-set! (symbol->string 'abc) 0 #\x))))
  (test "a position past the end" 'refused
    (refused (lambda () (string-set! (make-string 2 #\a) 2 #\x))))
  (test "a value that is not a character" 'refused
    (refused (lambda () (string-set! (make-string 2 #\a) 0 "x")))))

(test-group "string-fill! and string-copy!"

  (test "fill a range" "a--d"
    (let ((s (string-copy "abcd"))) (string-fill! s #\- 1 3) s))

  (test "copy from a mutated string" "xbcd"
    (let ((from (string-copy "abcd"))
          (to (make-string 4 #\-)))
      (string-set! from 0 #\x)
      (string-copy! to 0 from)
      to))

  (test "copy within a string, forwards and back" '("aabcd" "bcdee")
    (let ((a (string-copy "abcde"))
          (b (string-copy "abcde")))
      (string-copy! a 1 a 0 4)
      (string-copy! b 0 b 1 5)
      (list a b))))

(test-group "a changed string is a string like any other"

  (define s (string-copy "hello"))
  (string-set! s 0 #\j)

  (test "string?" #t (string? s))
  (test "string-length" 5 (string-length s))
  (test "string-ref" #\j (string-ref s 0))
  (test "string=? against a literal" #t (string=? s "jello"))
  (test "string<?" #t (string<? s "kello"))
  (test "string-ci=?" #t (string-ci=? s "JELLO"))
  (test "equal? against a literal" #t (equal? s "jello"))
  (test "equal? inside a structure" #t (equal? (list s) (list "jello")))
  (test "string-append" "jello!" (string-append s "!"))
  (test "substring" "ell" (substring s 1 4))
  (test "string->list" '(#\j #\e #\l #\l #\o) (string->list s))
  (test "string->symbol" 'jello (string->symbol s))
  (test "string->number" 42 (let ((n (string-copy "12"))) (string-set! n 0 #\4) (string->number n)))
  (test "write" "\"jello\"" (let ((p (open-output-string))) (write s p) (get-output-string p)))
  (test "display" "jello" (let ((p (open-output-string))) (display s p) (get-output-string p)))
  (test "read from it" 'jello (read (open-input-string s)))
  (test "string-for-each" '(#\o #\l #\l #\e #\j)
    (let ((out '())) (string-for-each (lambda (c) (set! out (cons c out))) s) out)))

(test-group "identity"

  (test "two newly made strings are distinct" #f (eq? (string-copy "a") (string-copy "a")))
  (test "one is itself" #t (let ((s (string-copy "a"))) (eq? s s)))
  (test "eqv? agrees" #f (eqv? (string-copy "a") (string-copy "a")))
  (test "and equal? compares characters" #t (equal? (string-copy "a") (string-copy "a"))))

(test-group "characters beyond the Basic Multilingual Plane"

  ;; A position is a UTF-16 code unit, so such a character takes two.
  (test "stored, it takes two positions" 3
    (let ((s (make-string 2 #\a))) (string-set! s 0 #\x1F600) (string-length s)))
  (test "and reads back as itself" #\x1F600
    (let ((s (make-string 2 #\a))) (string-set! s 0 #\x1F600) (string-ref s 0))))

(test-group "a changed string as a table key"

  (define (key)
    (let ((k (string-copy "kex")))
      (string-set! k 2 #\y)
      k))

  (test "a string=? table finds it by its characters" 'v
    (let ((t (string-mutation:make-hash-table string=?)))
      (string-mutation:hash-table-set! t (key) 'v)
      (string-mutation:hash-table-ref/default t "key" #f)))

  (test "and a string-ci=? table" 'v
    (let ((t (string-mutation:make-hash-table string-ci=?)))
      (string-mutation:hash-table-set! t (key) 'v)
      (string-mutation:hash-table-ref/default t "KEY" #f)))

  (test "and an equal? table" 'v
    (let ((t (string-mutation:make-hash-table equal?)))
      (string-mutation:hash-table-set! t (list (key)) 'v)
      (string-mutation:hash-table-ref/default t (list "key") #f))))
