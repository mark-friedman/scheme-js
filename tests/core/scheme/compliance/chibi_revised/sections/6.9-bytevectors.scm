;; R7RS Compliance Tests - Section 6.9: Bytevectors
;; Adapted from Chibi Scheme's R7RS test suite by Alex Shinn
;; NOTE: Uses (bytevector ...) function instead of #u8() literals

(test-group "6.9 Bytevectors"

  ;; Test bytevector? predicate
  (test #t (bytevector? (bytevector)))
  (test #t (bytevector? (bytevector 0 1 2)))
  (test #f (bytevector? #()))
  (test #f (bytevector? #(0 1 2)))
  (test #f (bytevector? '()))
  (test #t (bytevector? (make-bytevector 0)))

  (test 0 (bytevector-length (make-bytevector 0)))
  (test 1024 (bytevector-length (make-bytevector 1024)))
  (test 1024 (bytevector-length (make-bytevector 1024 255)))

  (test 3 (bytevector-length (bytevector 0 1 2)))

  (test 0 (bytevector-u8-ref (bytevector 0 1 2) 0))
  (test 1 (bytevector-u8-ref (bytevector 0 1 2) 1))
  (test 2 (bytevector-u8-ref (bytevector 0 1 2) 2))

  (test (bytevector 0 255 2)
      (let ((bv (bytevector 0 1 2))) (bytevector-u8-set! bv 1 255) bv))

  (test (bytevector) (bytevector-copy (bytevector)))
  (test (bytevector 0 1 2) (bytevector-copy (bytevector 0 1 2)))
  (test (bytevector 1 2) (bytevector-copy (bytevector 0 1 2) 1))
  (test (bytevector 1) (bytevector-copy (bytevector 0 1 2) 1 2))

  (test (bytevector 1 6 7 4 5)
      (let ((bv (bytevector 1 2 3 4 5)))
        (bytevector-copy! bv 1 (bytevector 6 7 8 9 10) 0 2)
        bv))
  (test (bytevector 6 7 8 9 10)
      (let ((bv (bytevector 1 2 3 4 5)))
        (bytevector-copy! bv 0 (bytevector 6 7 8 9 10))
        bv))
  (test (bytevector 8 9 10 4 5)
      (let ((bv (bytevector 1 2 3 4 5)))
        (bytevector-copy! bv 0 (bytevector 6 7 8 9 10) 2)
        bv))
  (test (bytevector 1 2 6 7 8)
      (let ((bv (bytevector 1 2 3 4 5)))
        (bytevector-copy! bv 2 (bytevector 6 7 8 9 10) 0 3)
        bv))
  (test (bytevector 1 2 8 4 5)
      (let ((bv (bytevector 1 2 3 4 5)))
        (bytevector-copy! bv 2 (bytevector 6 7 8 9 10) 2 3)
        bv))

  ;; same source and dest
  (test (bytevector 1 1 2 4 5)
      (let ((bv (bytevector 1 2 3 4 5)))
        (bytevector-copy! bv 1 bv 0 2)
        bv))
  (test (bytevector 1 2 3 1 2)
      (let ((bv (bytevector 1 2 3 4 5)))
        (bytevector-copy! bv 3 bv 0 2)
        bv))

  (test (bytevector) (bytevector-append (bytevector)))
  (test (bytevector) (bytevector-append (bytevector) (bytevector)))
  (test (bytevector 0 1 2) (bytevector-append (bytevector) (bytevector 0 1 2)))
  (test (bytevector 0 1 2) (bytevector-append (bytevector 0 1 2) (bytevector)))
  (test (bytevector 0 1 2 3 4) (bytevector-append (bytevector 0 1 2) (bytevector 3 4)))
  (test (bytevector 0 1 2 3 4 5) (bytevector-append (bytevector 0 1 2) (bytevector 3 4) (bytevector 5)))

  (test "ABC" (utf8->string (bytevector #x41 #x42 #x43)))
  (test "ABC" (utf8->string (bytevector 0 #x41 #x42 #x43) 1))
  (test "ABC" (utf8->string (bytevector 0 #x41 #x42 #x43 0) 1 4))
  (test (bytevector #x41 #x42 #x43) (string->utf8 "ABC"))
  (test (bytevector #x42 #x43) (string->utf8 "ABC" 1))
  (test (bytevector #x42) (string->utf8 "ABC" 1 2))
)
