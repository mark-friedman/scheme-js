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

;; /**
;;  * What a read error for a text says of it, as (incomplete? offset line
;;  * column), or #f if the text reads.
;;  */
(define (read-failure-place text)
  (guard (e ((error-object? e)
             (map (lambda (key)
                    (let ((v (js-ref e key)))
                      (cond ((js-null? v) #f) ((number? v) (exact v)) (else v))))
                  '("incomplete" "offset" "line" "column"))))
    (read-text text)
    #f))

(test-group "reader - block comments, among the other tokens"
  (test "nested, and over lines" '(a b) (read-text "a #| one\ntwo\r\n#| three |# |# b"))
  (test "only one" '() (read-text "#| nothing |#"))
  (test "#| in a string is the string's" '("#|") (read-text "\"#|\""))
  (test "and after an escaped quote" '("\"#|") (read-text "\"\\\"#|\""))
  (test "a |symbol| ending in #" (list (string->symbol "a#") 'b) (read-text "|a#| b"))
  (test "#\\# then a |symbol|" (list #\# 'a) (read-text "#\\#|a|"))
  (test "#| in a line comment is the comment's" '(a b) (read-text "a ; #| not a comment\nb"))
  (test "a block comment ends an identifier" '((a b)) (read-text "(a#|c|#b)"))
  (test "|# outside a comment begins a |symbol|" (list 'a (string->symbol "#") 'b) (read-text "a|#| b")))

(test-group "reader - text that ends inside a token"
  (test "a string, where it begins" '(#t 3 1 4) (read-failure-place "(a \"bc"))
  (test "a string whose last quote is escaped" '(#t 0 1 1) (read-failure-place "\"a\\\""))
  (test "a string ending in a backslash" '(#t 0 1 1) (read-failure-place "\"a\\"))
  (test "a |symbol|" '(#t 3 1 4) (read-failure-place "(a |bc"))
  (test "a #\\ with no character" '(#t 3 1 4) (read-failure-place "(a #\\"))
  (test "a block comment" '(#t 2 1 3) (read-failure-place "a #| b"))
  (test "a string after CR LF, on the line it begins" '(#t 6 2 2) (read-failure-place "(a)\r\n \"b"))
  (test "a block comment holding an ended one" '(#t 7 1 8) (read-failure-place "(a \"b\" #| c #| d |#"))
  (test "a list, which more text could end" #t (car (read-failure-place "(a b")))
  (test "but not a parenthesis too many" #f (car (read-failure-place "a)")))
  (test "ended, a string with an escaped backslash" '("a\\" b) (read-text "\"a\\\\\" b"))
  (test "and a |symbol| with an escaped |" (list (string->symbol "a|") 'b) (read-text "|a\\|| b")))

(test-group "reader - spans past comments, line endings and wide characters"
  (test "a list after a block comment over CR LF" '(2 9 2 12)
        (span-of (cadr (read-text "#| a\r\nb |# xy (c)"))))
  (test "columns count a character outside the BMP as two" '(1 4 1 9)
        (span-of (cadr (read-text "\x1F600; (a b)"))))
  (test "a quote form's after a line comment" '(2 1 2 2) (span-of (car (read-text "; c\n'(x)")))))

(test-group "reader - from a port"
  (define port (open-input-string "(a b) foo \"s\" #\\x41 rest a.b"))
  (test "a datum at a time" '((a b) foo "s" #\A) (list (read port) (read port) (read port) (read port)))
  (test "leaving what follows it in the port" #\space (read-char port))
  (test "dot notation off, as R7RS reads" (list 'rest (string->symbol "a.b")) (list (read port) (read port)))
  (test "and then the end" #t (eof-object? (read port)))
  (test "#!fold-case holds for the port's next reads" '(abc def)
        (let ((p (open-input-string "#!fold-case ABC DEF"))) (list (read p) (read p))))
  (test "a datum label inside one read" #t
        (let ((d (read (open-input-string "#0=(a . #0#)")))) (eq? d (cdr d)))))

(test-group "reader - what a REPL asks"
  (test "complete, a datum" #t (complete-text? "(a b)"))
  (test "or an error to report" #t (complete-text? "a)"))
  (test "not, a list not closed" #f (complete-text? "(a (b"))
  (test "nor blank text" #f (complete-text? "  \n"))
  (test "the parentheses delimiting lists and vectors, by position"
        '((0 . #t) (12 . #t) (14 . #f) (19 . #t) (21 . #f) (33 . #f))
        (delimiter-parens "(a \"(\" #\\( #(1) #u8(2) |(| ; (\n b)"))
  (test "those before a token the text ends inside" '((0 . #t)) (delimiter-parens "(a \"unfinished"))
  (test "a parenthesis too many is one" '((0 . #f)) (delimiter-parens ")"))
  (test "the match of an opening one" 8 (matching-delimiter "(a (b) c)" 0))
  (test "of a closing one" 3 (matching-delimiter "(a (b) c)" 5))
  (test "none, where there is no parenthesis" #f (matching-delimiter "(a (b) c)" 1))
  (test "nor for one no other closes" #f (matching-delimiter "(a (b" 0)))
