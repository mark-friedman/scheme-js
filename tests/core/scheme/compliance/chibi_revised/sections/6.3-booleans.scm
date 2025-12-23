;; R7RS Compliance Tests - Section 6.3: Booleans
;; Adapted from Chibi Scheme's R7RS test suite by Alex Shinn

(test-group "6.3 Booleans"

  (test #t #t)
  (test #f #f)
  (test #f '#f)

  (test #f (not #t))
  (test #f (not 3))
  (test #f (not (list 3)))
  (test #t (not #f))
  (test #f (not '()))
  (test #f (not (list)))
  (test #f (not 'nil))

  (test #t (boolean? #f))
  (test #f (boolean? 0))
  (test #f (boolean? '()))

  (test #t (boolean=? #t #t))
  (test #t (boolean=? #f #f))
  (test #f (boolean=? #t #f))
  (test #t (boolean=? #f #f #f))
  (test #f (boolean=? #t #t #f))
)
