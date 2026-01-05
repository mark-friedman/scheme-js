(test-group "HTML Script Tests"
  (test-assert "Basic Math" (= (* 6 7) 42))

  (define result (* 6 7))
  (test-assert "Defined variable" (= result 42))

  (display "HTML Script Tests Completed\n")
)
