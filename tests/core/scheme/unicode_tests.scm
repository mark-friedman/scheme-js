;; unicode_tests.scm -- characters and strings by Unicode, as R7RS 6.6 and 6.7
;; define them.
;;
;; The character predicates are Unicode's properties: char-alphabetic? the
;; Alphabetic property, char-numeric? Numeric_Digit -- a decimal digit, whose
;; value digit-value gives -- char-whitespace? White_Space, and
;; char-upper-case? and char-lower-case? Uppercase and Lowercase. A
;; character's case is mapped one character to one, Unicode's simple
;; mappings; a string's fully, so string-foldcase takes "ß" to "ss", and
;; folds every sigma to σ, and the -ci comparisons compare strings folded so.
;; Chibi's R7RS tests check the same things on fewer characters
;; (compliance/chibi_revised/sections/6.6-characters.scm, 6.7-strings.scm).

(test-group "the character predicates, by Unicode property"
  (test "Alphabetic, in any script" '(#t #t #t #t #t #t)
        (map char-alphabetic? (list #\a #\Λ #\λ #\é #\ж #\あ)))
  (test "and not digits, spaces or punctuation" '(#f #f #f)
        (map char-alphabetic? (list #\1 #\space #\!)))
  (test "a decimal digit in any script is numeric" '(#t #t #t #t)
        (map char-numeric? (list #\0 #\๐ #\x664 #\xAE6)))
  (test "a number that is not a decimal digit is not" '(#f #f)
        (map char-numeric? (list #\xBD #\x2167)))
  (test "White_Space: no-break, next-line and ideographic spaces" '(#t #t #t #t)
        (map char-whitespace? (list #\space #\xA0 #\x85 #\x3000)))
  (test "but not the byte order mark" #f (char-whitespace? #\xFEFF))
  (test "Uppercase and Lowercase" '(#t #f #f #t)
        (list (char-upper-case? #\Λ) (char-lower-case? #\Λ) (char-upper-case? #\λ) (char-lower-case? #\λ)))
  (test "a lowercase letter with no case mapping is lowercase" #t (char-lower-case? #\xAA))
  (test "a titlecase letter is neither" '(#f #f) (list (char-upper-case? #\x1C5) (char-lower-case? #\x1C5))))

(test-group "digit-value"
  (test "of the decimal digits of several scripts" '(3 4 0 9)
        (map digit-value (list #\3 #\x664 #\xAE6 #\xE59)))
  (test "of a mathematical digit, beyond the basic plane" 2 (digit-value #\x1D7DA))
  (test "of what is not a decimal digit" '(#f #f #f) (map digit-value (list #\a #\xBD #\x2167))))

(test-group "a character's case, one character to one"
  (test "upcase" '(#\Λ #\x1C4 #\ß) (list (char-upcase #\λ) (char-upcase #\x1C6) (char-upcase #\ß)))
  (test "downcase" '(#\σ #\i) (list (char-downcase #\Σ) (char-downcase #\I)))
  (test "foldcase takes every sigma, the long s and the Kelvin sign to their folded forms"
        '(#\σ #\σ #\s #\k #\ß)
        (map char-foldcase (list #\Σ #\x3C2 #\x17F #\x212A #\ß)))
  (test "char-ci=? compares characters folded" '(#t #t) (list (char-ci=? #\x3C2 #\Σ) (char-ci=? #\x17F #\S))))

(test-group "a string's case, fully"
  (test "foldcase" '("mass" "s" "μέλοσ" "ffi")
        (map string-foldcase (list "Maß" "\x17F;" "ΜΈΛΟΣ" "\xFB03;")))
  (test "downcase keeps a final sigma" "χαος" (string-downcase "ΧΑΟΣ"))
  (test "upcase" "STRASSE" (string-upcase "Straße"))
  (test "the -ci comparisons compare strings folded" '(#t #t #f)
        (list (string-ci=? "Straße" "STRASSE") (string-ci=? "ΜΈΛΟΣ" "μέλοσ") (string-ci<? "Straße" "STRASSE"))))
