;; character_tests.scm -- characters, and what a value is, in either tier.
;;
;; Compiled code compares two or three characters by their code points, reads
;; a character's code point, compares a value with a character constant by
;; identity -- there is one character object a code point -- and asks whether
;; a value is a character, a symbol, a string, a vector, a boolean or a
;; procedure, inline, and calls the primitive for anything else ("Characters,
;; and what a value is" in src/compiler/inline.scm). These check that the
;; answers are the interpreter's, a character beyond U+FFFF among them, which
;; UTF-16 orders before some below it, and that what the inline path does not
;; take -- a value that is no character -- still gets the primitive's error.
;;
;; The file runs twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js), and each procedure is called twice
;; before it is tested, since the tier compiles a procedure on its second call.

;; /**
;;  * Each comparison of two characters.
;;  * @param {char} a - One.
;;  * @param {char} b - The other.
;;  * @returns {list} What char=?, char<?, char>?, char<=? and char>=? answer.
;;  */
(define (compare a b)
  (list (char=? a b) (char<? a b) (char>? a b) (char<=? a b) (char>=? a b)))

;; /**
;;  * Each comparison of three characters.
;;  * @param {char} a - The first.
;;  * @param {char} b - The second.
;;  * @param {char} c - The third.
;;  * @returns {list} What char=?, char<?, char>?, char<=? and char>=? answer.
;;  */
(define (compare-three a b c)
  (list (char=? a b c) (char<? a b c) (char>? a b c) (char<=? a b c) (char>=? a b c)))

;; /**
;;  * A character's code point.
;;  * @param {char} c - The character.
;;  * @returns {integer}
;;  */
(define (code c) (char->integer c))

;; /**
;;  * What a character is, to a reader, by `case` on character data.
;;  * @param {*} c - The value.
;;  * @returns {symbol|boolean}
;;  */
(define (delimiter c)
  (case c ((#\( #\)) 'paren) ((#\;) 'comment) (else #f)))

;; /**
;;  * What a value is.
;;  * @param {*} x - The value.
;;  * @returns {list} What char?, symbol?, string?, vector?, boolean? and
;;  *   procedure? answer.
;;  */
(define (kind x)
  (list (char? x) (symbol? x) (string? x) (vector? x) (boolean? x) (procedure? x)))

;; /**
;;  * Whether a thunk raises an error.
;;  * @param {procedure} thunk - The thunk.
;;  * @returns {boolean}
;;  */
(define (refuses? thunk)
  (guard (e ((error-object? e) #t)) (thunk) #f))

(compare #\a #\b)
(compare #\a #\b)
(compare-three #\a #\b #\c)
(compare-three #\a #\b #\c)
(code #\a)
(code #\a)
(delimiter #\a)
(delimiter #\a)
(kind 1)
(kind 1)

(test-group "Characters"
  (test "the tier compiled them" (list *tier-attached* *tier-attached*)
        (list (eq? #t (js-ref compare "$compiled")) (eq? #t (js-ref kind "$compiled"))))
  (test "two characters in order" '(#f #t #f #t #f) (compare #\a #\b))
  (test "and the other way" '(#f #f #t #f #t) (compare #\b #\a))
  (test "two equal characters" '(#t #f #f #t #t) (compare #\a #\a))
  (test "by code point: a character beyond U+FFFF after one below it, which UTF-16 puts first"
        '(#f #t #f #t #f) (compare (integer->char #xFFFF) (integer->char #x10000)))
  (test "and case-insensitively, which the primitive does" '(#t #f)
        (list (char-ci<? (integer->char #xFFFF) (integer->char #x10000))
              (char-ci>? (integer->char #xFFFF) (integer->char #x10000))))
  (test "three characters in order" '(#f #t #f #t #f) (compare-three #\a #\m #\z))
  (test "three, the middle one out of order" '(#f #f #f #f #f) (compare-three #\a #\A #\z))
  (test "three equal characters" '(#t #f #f #t #t) (compare-three #\x #\x #\x))
  (test "a code point, beyond U+FFFF too" '(97 65536) (list (code #\a) (code (integer->char #x10000))))
  (test "case on characters" '(paren paren comment #f) (map delimiter (list #\( #\) #\; #\a)))
  (test "case on what is no character" '(#f #f) (list (delimiter '|(|) (delimiter "(")))
  (test "a character compared with no character is the primitive's error" #t
        (refuses? (lambda () (compare #\a 'a))))
  (test "and so is one of three" #t (refuses? (lambda () (compare-three #\a 1 #\z))))
  (test "and the code point of no character" #t (refuses? (lambda () (code "a")))))

(test-group "What a value is"
  (test "a character" '(#t #f #f #f #f #f) (kind #\a))
  (test "a symbol" '(#f #t #f #f #f #f) (kind 'a))
  (test "a string" '(#f #f #t #f #f #f) (kind (string-copy "a")))
  (test "a string constant" '(#f #f #t #f #f #f) (kind "a"))
  (test "a vector" '(#f #f #f #t #f #f) (kind (vector 1 2)))
  (test "true and false" '((#f #f #f #f #t #f) (#f #f #f #f #t #f)) (map kind '(#t #f)))
  (test "a procedure: compiled, a primitive, a continuation, a JavaScript function"
        '(#t #t #t #t)
        (map (lambda (p) (list-ref (kind p) 5))
             (list kind car (call/cc (lambda (k) k)) (js-eval "(function () {})"))))
  (test "and none of them: the empty list, a pair, numbers, a bytevector"
        '((#f #f #f #f #f #f) (#f #f #f #f #f #f) (#f #f #f #f #f #f) (#f #f #f #f #f #f)
          (#f #f #f #f #f #f))
        (map kind (list '() '(1) 1 1.5 (bytevector 1)))))
