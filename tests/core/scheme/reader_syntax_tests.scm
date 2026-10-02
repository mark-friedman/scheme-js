;; Reader Syntax Tests
;; Tests for R7RS reader syntax features added to fix chibi compliance

(test-group "reader-syntax"
  (test-group "boolean-literals"
    (test #t (read (open-input-string "#true")))
    (test #f (read (open-input-string "#false")))
    (test #t (read (open-input-string "#t")))
    (test #f (read (open-input-string "#f"))))

  (test-group "block-comments"
    ;; Block comments #|...|# should be ignored
    (test 5 (read (open-input-string "#| comment |# 5")))
    (test 42 (read (open-input-string "42 #| ignored |#")))
    (test 10 (read (open-input-string "#| outer #| inner |# still outer |# 10")))
    ;; Between the data a port holds, each read skipping the comments before
    ;; its datum, and the last read finding only a comment before the end
    (let ((port (open-input-string "#| a |# 1 #| b #| c |# |# 2 #| d |#")))
      (test '(1 2 #t) (list (read port) (read port) (eof-object? (read port)))))
    ;; Inside a list, where what a comment holds -- a parenthesis, a double
    ;; quote, a semicolon -- is not read
    (test '(a b) (read (open-input-string "(a #| ( \" ; |# b)")))
    (test 'b (read (open-input-string "#; #| c |# a b")))
    ;; In this file's own source, nested and over several lines
    (test '(1 2) '(1 #| a #| nested |# comment |# 2))
    (test '(1 2) '(1 #| over
                      two lines |# 2))
    ;; A line comment hides a #| in it, which starts no block comment
    (test '(1 2) '(1 ; #| in a line comment
                   2))
    ;; R7RS 2.2: an unterminated block comment is an error, not the rest of
    ;; the input commented out
    (test 'read-error
          (guard (e ((read-error? e) 'read-error) (#t 'other))
            (read (open-input-string "#| never closed"))))
    (test 'read-error
          (guard (e ((read-error? e) 'read-error) (#t 'other))
            (read (open-input-string "(a #| never #| closed |# b)")))))

  (test-group "hash-bar-inside-tokens"
    ;; R7RS 2.2: #| starts a comment only where a token can start, so inside
    ;; a string, a |symbol| or a character it is ordinary text, and so is |#.
    ;; First in this file's own source:
    (test 2 (string-length "#|"))
    (test 5 (string-length "#|#||"))
    (test '(#\# #\|) (string->list "#|"))
    (test '(#\| #\#) (string->list "|#"))
    (test 4 (length '(a "#|x" "y|#" b)))
    (test "a#" (symbol->string '|a#|))
    (test "#" (symbol->string '|#|))
    (test '("#" "a#" "b") (map symbol->string '(|#| |a#| b)))
    (test "a#|b" (symbol->string '|a#\|b|))
    (test "a|#b" (symbol->string '|a\|#b|))
    (test 124 (char->integer #\|))
    (test '(#\| #\#) (list #\| #\#))
    ;; A vertical line is a delimiter (R7RS 7.1.1), so #\# may be followed
    ;; directly by a |symbol|
    (test '(#\# a) '(#\#|a|))
    ;; Then read from a port
    (test "#|" (read (open-input-string "\"#|\"")))
    (test "#|#||" (read (open-input-string "\"#|#||\"")))
    (test '(a "#|x" "y|#" b) (read (open-input-string "(a \"#|x\" \"y|#\" b)")))
    (test '("#" "a#" "b")
          (map symbol->string (read (open-input-string "(|#| |a#| b)"))))
    (test "a#|b" (symbol->string (read (open-input-string "|a#\\|b|"))))
    (test #\| (read (open-input-string "#\\|")))
    (test '(#\# a) (read (open-input-string "(#\\#|a|)")))
    ;; A character that would otherwise open or close something -- a |symbol|,
    ;; a list, a string -- or start a comment, read one datum at a time
    (let ((port (open-input-string "#\\| #\\# |a| #\\( #\\\" #\\; x")))
      (test '(#\| #\# a #\( #\" #\; x)
            (list (read port) (read port) (read port) (read port)
                  (read port) (read port) (read port))))
    (test '(#\( #\) #\| #\") (read (open-input-string "(#\\( #\\) #\\| #\\\")"))))

  (test-group "vertical-bar-symbols"
    (test 'Hello (read (open-input-string "|Hello|")))
    (test #t (symbol? (read (open-input-string "|hello world|"))))
    (test #t (symbol? (read (open-input-string "||"))))  ; empty symbol
    )

  (test-group "symbol-writing"
    ;; When written, special symbols should be wrapped in |...|
    (let ((port (open-output-string)))
      (write '|| port)
      (test "||" (get-output-string port)))
    ;; The dot symbol needs to be read from string since '. is invalid syntax
    (let ((port (open-output-string)))
      (write (read (open-input-string "|.|")) port)
      (test "|.|" (get-output-string port)))
    ;; +i as a symbol (not the complex number)
    (let ((port (open-output-string)))
      (write '|+i| port)  ; use |+i| to ensure it's a symbol, not complex
      (test "|+i|" (get-output-string port))))

)
