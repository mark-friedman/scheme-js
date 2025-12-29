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
    (test 10 (read (open-input-string "#| outer #| inner |# still outer |# 10"))))

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
