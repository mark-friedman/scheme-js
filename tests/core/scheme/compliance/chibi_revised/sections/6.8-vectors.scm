;; R7RS Compliance Tests - Section 6.8: Vectors
;; Adapted from Chibi Scheme's R7RS test suite by Alex Shinn

(test-group "6.8 Vectors"

  (test #t (vector? #()))
  (test #t (vector? #(1 2 3)))
  (test #t (vector? '#(1 2 3)))

  (test 0 (vector-length (make-vector 0)))
  (test 1000 (vector-length (make-vector 1000)))

  (test #(0 (2 2 2 2) "Anna") '#(0 (2 2 2 2) "Anna"))

  (test #(a b c) (vector 'a 'b 'c))

  (test 8 (vector-ref '#(1 1 2 3 5 8 13 21) 5))
  (test 13 (vector-ref '#(1 1 2 3 5 8 13 21)
              (let ((i (round (* 2 (acos -1)))))
                (if (inexact? i)
                    (exact i)
                    i))))

  (test #(0 ("Sue" "Sue") "Anna") (let ((vec (vector 0 '(2 2 2 2) "Anna")))
    (vector-set! vec 1 '("Sue" "Sue"))
    vec))

  (test '(dah dah didah) (vector->list '#(dah dah didah)))
  (test '(dah didah) (vector->list '#(dah dah didah) 1))
  (test '(dah) (vector->list '#(dah dah didah) 1 2))
  (test #(dididit dah) (list->vector '(dididit dah)))

  (test #() (string->vector ""))
  (test #(#\A #\B #\C) (string->vector "ABC"))
  (test #(#\B #\C) (string->vector "ABC" 1))
  (test #(#\B) (string->vector "ABC" 1 2))

  (test "" (vector->string #()))
  (test "123" (vector->string #(#\1 #\2 #\3)))
  (test "23" (vector->string #(#\1 #\2 #\3) 1))
  (test "2" (vector->string #(#\1 #\2 #\3) 1 2))

  (test #() (vector-copy #()))
  (test #(a b c) (vector-copy #(a b c)))
  (test #(b c) (vector-copy #(a b c) 1))
  (test #(b) (vector-copy #(a b c) 1 2))

  (test #() (vector-append #()))
  (test #() (vector-append #() #()))
  (test #(a b c) (vector-append #() #(a b c)))
  (test #(a b c) (vector-append #(a b c) #()))
  (test #(a b c d e) (vector-append #(a b c) #(d e)))
  (test #(a b c d e f) (vector-append #(a b c) #(d e) #(f)))

  (test #(1 2 smash smash 5)
      (let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'smash 2 4) vec))
  (test #(x x x x x)
      (let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'x) vec))
  (test #(1 2 x x x)
      (let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'x 2) vec))
  (test #(1 2 x 4 5)
      (let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'x 2 3) vec))

  (test #(1 a b 4 5)
      (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 1 #(a b c d e) 0 2) vec))
  (test #(a b c d e)
      (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 0 #(a b c d e)) vec))
  (test #(c d e 4 5)
      (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 0 #(a b c d e) 2) vec))
  (test #(1 2 a b c)
      (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 2 #(a b c d e) 0 3) vec))
  (test #(1 2 c 4 5)
      (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 2 #(a b c d e) 2 3) vec))

  ;; same source and dest
  (test #(1 1 2 4 5)
      (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 1 vec 0 2) vec))
  (test #(1 2 3 1 2)
      (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 3 vec 0 2) vec))
)
