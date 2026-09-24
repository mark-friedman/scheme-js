;; SRFI 152: String library (reduced)
;;
;; The procedures SRFI 152 shares with R7RS-small are re-exported from
;; (scheme base) and (scheme char) and are not tested again here, except where
;; the SRFI takes optional start and end indices R7RS does not.

(import (scheme base)
        (scheme char)
        (srfi 152))

(test-group "SRFI 152 - predicates"
  (test "string-null? of the empty string" #t (string-null? ""))
  (test "string-null? of a string" #f (string-null? "a"))
  (test "string-every" #t (string-every char-alphabetic? "abc"))
  (test "string-every returns the last value" #\c (string-every (lambda (c) (and (char-alphabetic? c) c)) "abc"))
  (test "string-every of an empty range" #t (string-every char-alphabetic? "a1b" 1 1))
  (test "string-every over a range" #t (string-every char-numeric? "a12b" 1 3))
  (test "string-every failing" #f (string-every char-alphabetic? "a1"))
  (test "string-any" #t (string-any char-numeric? "ab1"))
  (test "string-any returns the first value" #\1 (string-any (lambda (c) (and (char-numeric? c) c)) "ab12"))
  (test "string-any failing" #f (string-any char-numeric? "abc"))
  (test "string-any over a range" #f (string-any char-numeric? "1ab2" 1 3)))

(test-group "SRFI 152 - constructors"
  (test "string-tabulate" "0123" (string-tabulate (lambda (i) (integer->char (+ i 48))) 4))
  (test "string-tabulate of zero" "" (string-tabulate (lambda (i) #\a) 0))
  (test "string-unfold" "abc"
        (string-unfold null? car cdr '(#\a #\b #\c)))
  (test "string-unfold with a base and a final" "<abc>"
        (string-unfold null? car cdr '(#\a #\b #\c) "<" (lambda (x) ">")))
  (test "string-unfold with a mapper returning strings" "aabbcc"
        (string-unfold null? (lambda (l) (string (car l) (car l))) cdr '(#\a #\b #\c)))
  (test "string-unfold-right" "cba"
        (string-unfold-right null? car cdr '(#\a #\b #\c)))
  (test "string-unfold-right with a base and a final" "<cba>"
        (string-unfold-right null? car cdr '(#\a #\b #\c) ">" (lambda (x) "<"))))

(test-group "SRFI 152 - conversion"
  (test "reverse-list->string" "cba" (reverse-list->string '(#\a #\b #\c)))
  (test "string->list with a range" '(#\b #\c) (string->list "abcd" 1 3))
  (test "string->vector with a range" #(#\b) (string->vector "abc" 1 2)))

(test-group "SRFI 152 - selection"
  (test "string-take" "ab" (string-take "abcde" 2))
  (test "string-drop" "cde" (string-drop "abcde" 2))
  (test "string-take-right" "de" (string-take-right "abcde" 2))
  (test "string-drop-right" "abc" (string-drop-right "abcde" 2))
  (test "string-pad" "  325" (string-pad "325" 5))
  (test "string-pad truncates from the left" "71325" (string-pad "8871325" 5))
  (test "string-pad with a character" "00325" (string-pad "325" 5 #\0))
  (test "string-pad-right" "325  " (string-pad-right "325" 5))
  (test "string-pad-right truncates from the right" "88713" (string-pad-right "8871325" 5))
  (test "string-trim" "abc  " (string-trim "  abc  "))
  (test "string-trim-right" "  abc" (string-trim-right "  abc  "))
  (test "string-trim-both" "abc" (string-trim-both "  abc  "))
  (test "string-trim with a predicate" "abc" (string-trim "xxabc" (lambda (c) (char=? c #\x))))
  (test "string-trim-both of blanks" "" (string-trim-both "   "))
  (test-error "string-take past the end" "string-take" (string-take "ab" 3)))

(test-group "SRFI 152 - replacement"
  (test "string-replace" "The Scheme Language"
        (string-replace "The TCL Language" "Scheme" 4 7))
  (test "string-replace with a range of the second string" "aXYd"
        (string-replace "abcd" "WXYZ" 1 3 1 3)))

(test-group "SRFI 152 - prefixes and suffixes"
  (test "string-prefix-length" 2 (string-prefix-length "abcd" "abxy"))
  (test "string-suffix-length" 2 (string-suffix-length "xycd" "abcd"))
  (test "string-prefix?" #t (string-prefix? "ab" "abcd"))
  (test "string-prefix? failing" #f (string-prefix? "ac" "abcd"))
  (test "string-suffix?" #t (string-suffix? "cd" "abcd"))
  (test "string-suffix? failing" #f (string-suffix? "bd" "abcd"))
  (test "string-prefix? of the empty string" #t (string-prefix? "" "abc")))

(test-group "SRFI 152 - searching"
  (test "string-index" 2 (string-index "ab1c" char-numeric?))
  (test "string-index with no match" #f (string-index "abc" char-numeric?))
  (test "string-index from a start" 3 (string-index "1ab2" char-numeric? 1))
  (test "string-index-right" 3 (string-index-right "1ab2" char-numeric?))
  (test "string-index-right with no match" #f (string-index-right "abc" char-numeric?))
  (test "string-skip" 2 (string-skip "  ab" char-whitespace?))
  (test "string-skip-right" 1 (string-skip-right "ab  " char-whitespace?))
  (test "string-contains" 4 (string-contains "The Scheme" "Scheme"))
  (test "string-contains with no match" #f (string-contains "The Scheme" "Lisp"))
  (test "string-contains of the empty string" 0 (string-contains "abc" ""))
  (test "string-contains from a start" 4 (string-contains "abcabc" "bc" 2))
  (test "string-contains-right" 4 (string-contains-right "abcabc" "bc"))
  (test "string-contains-right with no match" #f (string-contains-right "abc" "x")))

(test-group "SRFI 152 - concatenation"
  (test "string-concatenate" "abcd" (string-concatenate '("a" "bc" "" "d")))
  (test "string-concatenate of nothing" "" (string-concatenate '()))
  (test "string-concatenate-reverse" "cba" (string-concatenate-reverse '("a" "b" "c")))
  (test "string-concatenate-reverse with a final string" "cbaxy"
        (string-concatenate-reverse '("a" "b" "c") "xyz" 2))
  (test "string-join" "foo bar baz" (string-join '("foo" "bar" "baz")))
  (test "string-join with a delimiter" "foo:bar" (string-join '("foo" "bar") ":"))
  (test "string-join of nothing" "" (string-join '()))
  (test "string-join prefix" ":foo:bar" (string-join '("foo" "bar") ":" 'prefix))
  (test "string-join suffix" "foo:bar:" (string-join '("foo" "bar") ":" 'suffix))
  (test "string-join strict-infix" "foo:bar" (string-join '("foo" "bar") ":" 'strict-infix))
  (test-error "string-join strict-infix of nothing" "string-join" (string-join '() ":" 'strict-infix)))

(test-group "SRFI 152 - fold and map"
  (test "string-fold" '(#\c #\b #\a) (string-fold cons '() "abc"))
  (test "string-fold over a range" '(#\c #\b) (string-fold cons '() "abcd" 1 3))
  (test "string-fold-right" '(#\a #\b #\c) (string-fold-right cons '() "abc"))
  (test "string-count" 2 (string-count "a1b2" char-numeric?))
  (test "string-count over a range" 1 (string-count "a1b2" char-numeric? 0 2))
  (test "string-filter" "12" (string-filter char-numeric? "a1b2"))
  (test "string-remove" "ab" (string-remove char-numeric? "a1b2"))
  (test "string-filter over a range" "1" (string-filter char-numeric? "a1b2" 0 2)))

(test-group "SRFI 152 - replication and splitting"
  (test "string-replicate" "cdefab" (string-replicate "abcdef" 2 8))
  (test "string-replicate going negative" "efabcd" (string-replicate "abcdef" -2 4))
  (test "string-replicate of a range" "cbc" (string-replicate "abc" 1 4 1 3))
  (test "string-split" '("a" "b" "c") (string-split "a b c" " "))
  (test "string-split keeps empty fields" '("a" "" "b") (string-split "a,,b" ","))
  (test "string-split of the empty string" '() (string-split "" ","))
  (test "string-split with a limit" '("a" "b,c") (string-split "a,b,c" "," 'infix 1))
  (test "string-split prefix" '("a" "b") (string-split ",a,b" "," 'prefix))
  (test "string-split suffix" '("a" "b") (string-split "a,b," "," 'suffix))
  (test "string-split with a longer delimiter" '("a" "b") (string-split "a::b" "::")))
