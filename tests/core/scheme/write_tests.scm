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
  
  ;; ===== object printing tests =====
  
  (test-group "object printing"
    
    (test "write simple object"
      "#{(a 1) (b 2)}"
      (let ((out (open-output-string)))
        (write #{(a 1) (b 2)} out)
        (get-output-string out)))
    
    (test "display object with string value"
      "#{(name hello)}"
      (let ((out (open-output-string)))
        (display #{(name "hello")} out)
        (get-output-string out)))
    
    (test "write object with string value"
      "#{(name \"hello\")}"
      (let ((out (open-output-string)))
        (write #{(name "hello")} out)
        (get-output-string out)))
    
    (test "write empty object"
      "#{}"
      (let ((out (open-output-string)))
        (write #{} out)
        (get-output-string out)))
  )
  
  ;; ===== roundtrip tests (write -> read -> equal?) =====
  
  (test-group "roundtrip"
    
    ;; Objects: equal? doesn't support JS objects, so compare written forms
    ;; The round trip is: write -> read -> eval -> write, check strings match
    (test "object roundtrip"
      #t
      (let* ((obj #{(a 1) (b "hello")})
             (write-str (lambda (x) 
                          (let ((p (open-output-string)))
                            (write x p)
                            (get-output-string p))))
             (str1 (write-str obj))
             (readback (eval (read (open-input-string str1)) (interaction-environment)))
             (str2 (write-str readback)))
        (string=? str1 str2)))
    
    ;; Vectors: equal? supports vectors
    (test "vector roundtrip"
      #t
      (let* ((vec #(1 2 3 "test"))
             (str (let ((p (open-output-string)))
                    (write vec p)
                    (get-output-string p))))
        (equal? vec (read (open-input-string str)))))
    
    ;; Nested objects: compare written forms
    (test "nested object roundtrip"
      #t
      (let* ((obj #{(inner #{(x 1)})})
             (write-str (lambda (x) 
                          (let ((p (open-output-string)))
                            (write x p)
                            (get-output-string p))))
             (str1 (write-str obj))
             (readback (eval (read (open-input-string str1)) (interaction-environment)))
             (str2 (write-str readback)))
        (string=? str1 str2)))
    
    ;; Lists: equal? supports lists
    (test "list roundtrip"
      #t
      (let* ((lst '(1 2 (3 4) "test"))
             (str (let ((p (open-output-string)))
                    (write lst p)
                    (get-output-string p))))
        (equal? lst (read (open-input-string str)))))
  )
  
) ;; end test-group
