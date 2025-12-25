;;; Bytevector Tests
;;; R7RS §6.9 - Bytevector operations

(test-group "Bytevectors"

  (test-group "Type Predicate"
    (test "bytevector? on bytevector" #t
          (bytevector? (bytevector)))
    (test "bytevector? on vector" #f
          (bytevector? #()))
    (test "bytevector? on list" #f
          (bytevector? '()))
    (test "bytevector? on string" #f
          (bytevector? "hello")))

  (test-group "Constructors"
    (test "make-bytevector length" 5
          (bytevector-length (make-bytevector 5)))
    (test "make-bytevector with fill" 42
          (bytevector-u8-ref (make-bytevector 3 42) 0))
    (test "bytevector length" 3
          (bytevector-length (bytevector 1 2 3)))
    (test "bytevector elements" 2
          (bytevector-u8-ref (bytevector 1 2 3) 1)))

  (test-group "Accessors"
    (test "bytevector-u8-set!" 99
          (let ((bv (make-bytevector 3 0)))
            (bytevector-u8-set! bv 1 99)
            (bytevector-u8-ref bv 1))))

  (test-group "Copy Operations"
    (test "bytevector-copy length" 3
          (bytevector-length (bytevector-copy (bytevector 1 2 3))))
    (test "bytevector-copy element" 2
          (bytevector-u8-ref (bytevector-copy (bytevector 1 2 3)) 1))
    (test "bytevector-copy with start" 2
          (bytevector-length (bytevector-copy (bytevector 1 2 3) 1)))
    (test "bytevector-append length" 5
          (bytevector-length (bytevector-append (bytevector 1 2) (bytevector 3 4 5)))))

  (test-group "String Conversion"
    (test "utf8->string basic" "ABC"
          (utf8->string (bytevector 65 66 67)))
    (test "string->utf8 length" 3
          (bytevector-length (string->utf8 "ABC"))))
)
