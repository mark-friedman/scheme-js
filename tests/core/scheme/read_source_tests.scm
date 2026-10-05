;; The reader's Scheme (src/core/scheme/reader.scm): text into data, with the
;; span each list and vector was read from, dot notation, the directives, datum
;; labels, and the errors for text that ends inside a datum.

(import (scheme-js reader))

;; /**
;;  * The data of a text, read as a program's is, dot notation on.
;;  */
(define (read-text text)
  (read-source text "t.scm" #f #t))

;; /**
;;  * The span a datum carries, as (line column end-line end-column), or #f.
;;  */
(define (span-of datum)
  (let ((span (js-ref datum "source")))
    (and (not (js-undefined? span))
         (map (lambda (key) (exact (js-ref span key))) '("line" "column" "endLine" "endColumn")))))

;; /**
;;  * The message of the error reading a text raises, or #f.
;;  */
(define (read-failure text)
  (guard (e ((error-object? e) (error-object-message e)))
    (read-text text)
    #f))

(test-group "reader - data"
  (test "lists, dotted lists and vectors" '((a b . c) #(1 2) ()) (read-text "(a b . c) #(1 2) ()"))
  (test "the quote forms" '('x `(a ,b ,@c)) (read-text "'x `(a ,b ,@c)"))
  (test "strings with their escapes" '("a\nb\x41;\"") (read-text "\"a\\nb\\x41;\\\"\""))
  (test "a line continuation stands for nothing" '("ab") (read-text "\"a\\\n   b\""))
  (test "characters, by name, by code and themselves" (list #\space #\A #\a #\() (read-text "#\\space #\\x41 #\\a #\\("))
  (test "booleans, both ways" '(#t #f #t #f) (read-text "#t #f #true #false"))
  (test "numbers, as string->number reads them" '(1/2 16 3/2 100.0) (read-text "1/2 #x10 #e1.5 1e2"))
  (test "a |symbol|, its name as written" (list (string->symbol "a b")) (read-text "|a b|"))
  (test "a bytevector" (list (bytevector 1 2 255)) (read-text "#u8(1 2 255)"))
  (test "comments of every kind are skipped" '(kept done) (read-text "; line\n#;(skipped) kept #| a #| nested |# |# done"))
  (test "a form feed, which breaks a file into pages, is whitespace" '(a b)
        (read-text (string #\a (integer->char 12) #\b)))
  (test "a string that is read cannot be changed" #t
        (guard (e (#t #t)) (string-set! (car (read-text "\"abc\"")) 0 #\x) #f)))

(test-group "reader - spans"
  (test "a list's, from its parenthesis to the one closing it" '(1 1 2 11)
        (span-of (car (read-text "(define (f x)\n  (+ x 1))"))))
  (test "a nested list's" '(1 9 1 14) (span-of (cadr (car (read-text "(define (f x) y)")))))
  (test "a vector's" '(1 3 1 9) (span-of (car (read-text "  #(1 2)"))))
  (test "a quote form's, its quote mark's" '(1 1 1 2) (span-of (car (read-text "'(a)")))))

(test-group "reader - dot notation and directives"
  (test "a property access" '((js-ref (js-ref a "b") "c")) (read-text "a.b.c"))
  (test "after a datum, with nothing between" '((js-ref (f x) "y")) (read-text "(f x).y"))
  (test "off, a dot is part of a name" (list (string->symbol "a.b")) (read-source "a.b" "t.scm" #f #f))
  (test "turned off by a directive" (list (string->symbol "a.b")) (read-text "#!no-dot-notation a.b"))
  (test "case folded by another" '(abc DEF) (read-text "#!fold-case ABC #!no-fold-case DEF"))
  (test "a script header is skipped" '(x) (read-text "#!/usr/bin/env scheme\nx"))
  (test "an object literal" '((js-obj 'a 1 "b" 2)) (read-text "#{(a 1) (\"b\" 2)}")))

(test-group "reader - datum labels"
  (test "a label and a reference to it" '((a a)) (map (lambda (d) (list (car d) (cadr d))) (read-text "(#0=a #0#)")))
  (test "a circular list" #t (let ((d (car (read-text "#0=(a . #0#)")))) (eq? d (cdr d))))
  (test "a reference to no label" #t (string? (read-failure "#5#"))))

(test-group "reader - errors"
  (test "a list not closed" #t (string? (read-failure "(a b")))
  (test "a parenthesis not opened" #t (string? (read-failure "a)")))
  (test "a string not ended" #t (string? (read-failure "\"abc")))
  (test "a bracket, which R7RS reserves" #t (string? (read-failure "[a]")))
  (test "a lone dot" #t (string? (read-failure "."))))
