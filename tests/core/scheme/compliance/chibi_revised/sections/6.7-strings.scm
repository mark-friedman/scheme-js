;; R7RS Compliance Tests - Section 6.7: Strings
;; Adapted from Chibi Scheme's R7RS test suite by Alex Shinn

(test-group "6.7 Strings"

  (test #t (string? ""))
  (test #t (string? " "))
  (test #f (string? 'a))
  ;; SKIP: JS uses single-char strings for characters - no type disjointness
  (test-skip "(string? #\\a) disjointness" "JS characters are single-char strings")

  (test 3 (string-length (make-string 3)))
  (test "---" (make-string 3 #\-))

  (test "" (string))
  (test "---" (string #\- #\- #\-))
  (test "kitten" (string #\k #\i #\t #\t #\e #\n))

  (test 0 (string-length ""))
  (test 1 (string-length "a"))
  (test 3 (string-length "abc"))

  (test #\a (string-ref "abc" 0))
  (test #\b (string-ref "abc" 1))
  (test #\c (string-ref "abc" 2))

  ;; SKIP: JavaScript strings are immutable - string-set! not supported
  (test-skip "string-set!" "JavaScript strings are immutable")

  (test #t (string=? "" ""))
  (test #t (string=? "abc" "abc" "abc"))
  (test #f (string=? "" "abc"))
  (test #f (string=? "abc" "aBc"))

  (test #f (string<? "" ""))
  (test #f (string<? "abc" "abc"))
  (test #t (string<? "abc" "abcd" "acd"))
  (test #f (string<? "abcd" "abc"))
  (test #t (string<? "abc" "bbc"))

  (test #f (string>? "" ""))
  (test #f (string>? "abc" "abc"))
  (test #f (string>? "abc" "abcd"))
  (test #t (string>? "acd" "abcd" "abc"))
  (test #f (string>? "abc" "bbc"))

  (test #t (string<=? "" ""))
  (test #t (string<=? "abc" "abc"))
  (test #t (string<=? "abc" "abcd" "abcd"))
  (test #f (string<=? "abcd" "abc"))
  (test #t (string<=? "abc" "bbc"))

  (test #t (string>=? "" ""))
  (test #t (string>=? "abc" "abc"))
  (test #f (string>=? "abc" "abcd"))
  (test #t (string>=? "abcd" "abcd" "abc"))
  (test #f (string>=? "abc" "bbc"))

  (test #t (string-ci=? "" ""))
  (test #t (string-ci=? "abc" "abc"))
  (test #f (string-ci=? "" "abc"))
  (test #t (string-ci=? "abc" "aBc"))
  (test #f (string-ci=? "abc" "aBcD"))

  (test #f (string-ci<? "abc" "aBc"))
  (test #t (string-ci<? "abc" "aBcD"))
  (test #f (string-ci<? "ABCd" "aBc"))

  (test #f (string-ci>? "abc" "aBc"))
  (test #f (string-ci>? "abc" "aBcD"))
  (test #t (string-ci>? "ABCd" "aBc"))

  (test #t (string-ci<=? "abc" "aBc"))
  (test #t (string-ci<=? "abc" "aBcD"))
  (test #f (string-ci<=? "ABCd" "aBc"))

  (test #t (string-ci>=? "abc" "aBc"))
  (test #f (string-ci>=? "abc" "aBcD"))
  (test #t (string-ci>=? "ABCd" "aBc"))

  (test "ABC" (string-upcase "abc"))
  (test "ABC" (string-upcase "ABC"))
  (test "abc" (string-downcase "abc"))
  (test "abc" (string-downcase "ABC"))
  (test "abc" (string-foldcase "abc"))
  (test "abc" (string-foldcase "ABC"))

  (test "" (substring "" 0 0))
  (test "" (substring "a" 0 0))
  (test "" (substring "abc" 1 1))
  (test "ab" (substring "abc" 0 2))
  (test "bc" (substring "abc" 1 3))

  (test "" (string-append ""))
  (test "" (string-append "" ""))
  (test "abc" (string-append "" "abc"))
  (test "abc" (string-append "abc" ""))
  (test "abcde" (string-append "abc" "de"))
  (test "abcdef" (string-append "abc" "de" "f"))

  (test '() (string->list ""))
  (test '(#\a) (string->list "a"))
  (test '(#\a #\b #\c) (string->list "abc"))
  (test '(#\a #\b #\c) (string->list "abc" 0))
  (test '(#\b #\c) (string->list "abc" 1))
  (test '(#\b #\c) (string->list "abc" 1 3))

  (test "" (list->string '()))
  (test "abc" (list->string '(#\a #\b #\c)))

  (test "" (string-copy ""))
  (test "" (string-copy "" 0))
  (test "" (string-copy "" 0 0))
  (test "abc" (string-copy "abc"))
  (test "abc" (string-copy "abc" 0))
  (test "bc" (string-copy "abc" 1))
  (test "b" (string-copy "abc" 1 2))
  (test "bc" (string-copy "abc" 1 3))

  ;; SKIP: JavaScript strings are immutable - string-fill! not supported
  (test-skip "string-fill! (full)" "JavaScript strings are immutable")
  (test-skip "string-fill! (start)" "JavaScript strings are immutable")
  (test-skip "string-fill! (start end)" "JavaScript strings are immutable")

  ;; SKIP: JavaScript strings are immutable - string-copy! not supported
  (test-skip "string-copy! (1)" "JavaScript strings are immutable")
  (test-skip "string-copy! (2)" "JavaScript strings are immutable")
  (test-skip "string-copy! (3)" "JavaScript strings are immutable")
  (test-skip "string-copy! (4)" "JavaScript strings are immutable")
  (test-skip "string-copy! (5)" "JavaScript strings are immutable")

  ;; same source and dest - also requires mutable strings
  (test-skip "string-copy! (same source/dest 1)" "JavaScript strings are immutable")
  (test-skip "string-copy! (same source/dest 2)" "JavaScript strings are immutable")
)
