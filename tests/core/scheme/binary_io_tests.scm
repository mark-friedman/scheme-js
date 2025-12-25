;; Binary I/O Tests
;;
;; Tests for bytevector ports and binary I/O operations

(test-group "Binary I/O tests"
  
  ;; ===== Bytevector Input Port tests =====
  
  (test-group "bytevector input port"
    
    (test "open-input-bytevector returns port"
      #t
      (input-port? (open-input-bytevector (bytevector 1 2 3))))
    
    (test "bytevector input port is binary"
      #t
      (binary-port? (open-input-bytevector (bytevector 1 2 3))))
    
    (test "read-u8 from bytevector port"
      1
      (let ((p (open-input-bytevector (bytevector 1 2 3))))
        (read-u8 p)))
    
    (test "peek-u8 doesn't consume"
      2
      (let ((p (open-input-bytevector (bytevector 1 2 3))))
        (read-u8 p)  ; consume 1
        (peek-u8 p)  ; peek at 2
        (read-u8 p))) ; should return 2
    
    (test "u8-ready? on bytevector port"
      #t
      (let ((p (open-input-bytevector (bytevector 1 2 3))))
        (u8-ready? p)))
    
    (test "read-u8 returns eof at end"
      #t
      (let ((p (open-input-bytevector (bytevector))))
        (eof-object? (read-u8 p))))
    
    (test "read-bytevector returns bytevector"
      #t
      (let ((p (open-input-bytevector (bytevector 1 2 3 4 5))))
        (bytevector? (read-bytevector 3 p))))
    
    (test "read-bytevector reads correct count"
      3
      (let ((p (open-input-bytevector (bytevector 1 2 3 4 5))))
        (bytevector-length (read-bytevector 3 p))))
  )
  
  ;; ===== Bytevector Output Port tests =====
  
  (test-group "bytevector output port"
    
    (test "open-output-bytevector returns port"
      #t
      (output-port? (open-output-bytevector)))
    
    (test "bytevector output port is binary"
      #t
      (binary-port? (open-output-bytevector)))
    
    (test "write-u8 and get-output-bytevector"
      1
      (let ((p (open-output-bytevector)))
        (write-u8 42 p)
        (bytevector-length (get-output-bytevector p))))
    
    (test "write-u8 writes correct byte"
      42
      (let ((p (open-output-bytevector)))
        (write-u8 42 p)
        (bytevector-u8-ref (get-output-bytevector p) 0)))
    
    (test "write-bytevector writes all bytes"
      3
      (let ((p (open-output-bytevector)))
        (write-bytevector (bytevector 1 2 3) p)
        (bytevector-length (get-output-bytevector p))))
    
    (test "multiple writes accumulate"
      5
      (let ((p (open-output-bytevector)))
        (write-u8 1 p)
        (write-u8 2 p)
        (write-bytevector (bytevector 3 4 5) p)
        (bytevector-length (get-output-bytevector p))))
  )
  
  ;; ===== Type checks =====
  
  (test-group "binary I/O type errors"
    
    (test "read-u8 requires binary port"
      'error
      (guard (e (#t 'error))
        (read-u8 (open-input-string "abc"))))
    
    (test "write-u8 range check"
      'error
      (guard (e (#t 'error))
        (let ((p (open-output-bytevector)))
          (write-u8 256 p))))
  )
  
) ;; end test-group
