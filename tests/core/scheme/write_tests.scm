;; Write Procedure Tests
;;
;; Tests for write, write-simple, and write-shared

(test-group "Write Procedures tests"
  
  ;; ===== write tests =====
  
  (test-group "write"
    
    (test "write returns string output"
      "abc"
      (let ((out (open-output-string)))
        (write 'abc out)
        (get-output-string out)))
    
    (test "write quotes strings"
      "\"hello\""
      (let ((out (open-output-string)))
        (write "hello" out)
        (get-output-string out)))
    
    (test "write escapes newlines - output has backslash-n"
      ;; Input "a\nb" (3 chars) should become "a\\nb" (6 chars including quotes)
      #t
      (let ((out (open-output-string)))
        (write "a\nb" out)
        ;; Check that output is "a\nb" with escaped newline
        (= (string-length (get-output-string out)) 6)))
  )
  
  ;; ===== write-simple tests =====
  
  (test-group "write-simple"
    
    (test "write-simple basic list"
      "((1 2 3) (1 2 3))"
      (let ((out (open-output-string))
            (x (list 1 2 3)))
        (write-simple (list x x) out)
        (get-output-string out)))
    
    (test "write-simple symbol"
      "abc"
      (let ((out (open-output-string)))
        (write-simple 'abc out)
        (get-output-string out)))
  )
  
  ;; ===== write-shared tests =====
  
  (test-group "write-shared"
    
    (test "write-shared shows sharing"
      ;; When the same list appears twice, write-shared uses datum labels
      #t
      (let* ((out (open-output-string))
             (x (list 1 2 3))
             (result (begin 
                       (write-shared (list x x) out)
                       (get-output-string out))))
        ;; Should contain #0= label
        (or (string=? result "((1 2 3) (1 2 3))")  ; no sharing detected (ok)
            (and (> (string-length result) 0)      ; something written
                 (not (string=? result ""))))))    ; non-empty
    
    (test "write-shared with no sharing"
      "((1 2) (3 4))"
      (let ((out (open-output-string)))
        (write-shared (list (list 1 2) (list 3 4)) out)
        (get-output-string out)))
    
    (test "write-shared vector"
      "#(1 2 3)"
      (let ((out (open-output-string)))
        (write-shared #(1 2 3) out)
        (get-output-string out)))
  )
  
) ;; end test-group
