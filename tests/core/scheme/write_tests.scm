;; Write Procedure Tests
;;
;; Tests for write, write-simple, and write-shared

;; /**
;;  * What a write procedure writes for a value.
;;  * @param {procedure} write-proc - write, display, write-shared or write-simple.
;;  * @param {*} x - The value.
;;  * @returns {string}
;;  */
(define (written write-proc x)
  (let ((out (open-output-string)))
    (write-proc x out)
    (get-output-string out)))

;; /**
;;  * A circular list of the values given: its last pair's cdr is its first.
;;  * @param {...*} xs - At least one value.
;;  * @returns {pair}
;;  */
(define (circular . xs)
  (let ((x (list-copy xs)))
    (let loop ((last x))
      (if (null? (cdr last))
          (set-cdr! last x)
          (loop (cdr last))))
    x))

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

  ;; ===== circular structure (R7RS 6.13.3) =====
  ;;
  ;; write and display must terminate on circular structure, labelling at
  ;; least the objects that form a cycle, and write must use no labels
  ;; where there is no cycle.

  (test-group "write, circular"

    (test "the example in R7RS 2.4"
      "#0=(a b c . #0#)"
      (written write (circular 'a 'b 'c)))

    (test "a cycle reached through a list's tail"
      "(bar . #0=(baz . #0#))"
      (written write (cons 'bar (circular 'baz))))

    (test "a circular list as an element"
      "(bar #0=(baz . #0#))"
      (written write (list 'bar (circular 'baz))))

    (test "a pair that is its own car"
      "#0=(#0#)"
      (let ((x (list 1)))
        (set-car! x x)
        (written write x)))

    (test "a vector that holds itself"
      "#0=#(1 #0#)"
      (let ((v (vector 1 2)))
        (vector-set! v 1 v)
        (written write v)))

    (test "two cycles, numbered as written"
      "(#0=(a . #0#) #1=(b . #1#))"
      (written write (list (circular 'a) (circular 'b))))

    (test "a cycle written twice is referred to the second time"
      "(#0=(1 . #0#) #0#)"
      (let ((x (circular 1)))
        (written write (list x x))))

    (test "shared structure that is not circular has no labels"
      "((1 2) (1 2))"
      (let ((x (list 1 2)))
        (written write (list x x))))

    (test "what write writes reads back as the same cycle"
      #t
      (let ((y (read (open-input-string (written write (circular 'a 'b 'c))))))
        (and (eq? y (cdddr y)) (eq? 'c (caddr y)))))

    (test "display labels a cycle too, writing its strings as display does"
      "#0=(a b . #0#)"
      (written display (circular "a" #\b)))
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
      "(#0=(1 2 3) #0#)"
      (let ((x (list 1 2 3)))
        (written write-shared (list x x))))

    (test "write-shared, a shared tail"
      ;; A pair shared in a list's tail breaks the list there
      "((1 . #0=(2 3)) #0#)"
      (let ((tail (list 2 3)))
        (written write-shared (list (cons 1 tail) tail))))

    (test "write-shared, a cycle reached through a list's tail"
      "(bar . #0=(baz . #0#))"
      (written write-shared (cons 'bar (circular 'baz))))

    (test "write-shared, a list that is its own tail"
      "#0=(a b c . #0#)"
      (written write-shared (circular 'a 'b 'c)))
    
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

;; ===== The Sign of Zero =====
;;
;; R7RS 6.2.4 distinguishes -0.0 from 0.0, and `write` writes a number so that
;; `read` gives it back. JavaScript's `String(-0)` is "0", so `write` lost the
;; sign, and a complex number's -0.0 imaginary part was written "+-0.0i",
;; which is not a number at all.

;; /**
;;  * Whether a number is negative zero.
;;  * @param {number} x - The number.
;;  * @returns {boolean}
;;  */
(define (negative-zero? x)
  (and (inexact? x) (zero? x) (= -inf.0 (/ 1 x))))

(test-group "the sign of zero"
  (test "write -0.0" "-0.0" (written write -0.0))
  (test "display -0.0" "-0.0" (written display -0.0))
  (test "write 0.0" "0.0" (written write 0.0))
  (test "write -0.0 in a list" "(-0.0 0.0)" (written write (list -0.0 0.0)))
  (test "write -0.0 in a vector" "#(-0.0)" (written write (vector -0.0)))
  (test "write a computed -0.0" "-0.0" (written write (round -0.5)))
  (test "number->string -0.0" "-0.0" (number->string -0.0))
  (test "-0.0 written reads back as -0.0"
    #t (negative-zero? (read (open-input-string (written write -0.0)))))
  (test "a negative zero imaginary part" "1.0-0.0i" (written write (make-rectangular 1.0 -0.0)))
  (test "a negative zero real part" "-0.0+2.0i" (written write (make-rectangular -0.0 2.0)))
  (test "both parts negative zero" "-0.0-0.0i" (written write (make-rectangular -0.0 -0.0)))
  (test "a positive zero imaginary part" "1.0+0.0i" (written write (make-rectangular 1.0 0.0)))
  (test "number->string of a negative zero imaginary part"
    "1.0-0.0i" (number->string (make-rectangular 1.0 -0.0)))
  (test "a negative zero imaginary part written reads back"
    #t (negative-zero?
        (imag-part (read (open-input-string (written write (make-rectangular 1.0 -0.0))))))))

;; R7RS 6.2.7: an inexact number is written with a decimal point wherever one
;; could be, so with an exponent too: JavaScript writes 5e-324 and 1e+21.
(test-group "an inexact number's text has a decimal point"
  (test "the smallest subnormal" "5.0e-324" (number->string 5e-324))
  (test "a large integer" "1.0e+21" (number->string 1e21))
  (test "a fraction with an exponent keeps its own" "1.5e-7" (number->string 1.5e-7))
  (test "as write writes it" "1.0e+21" (written write 1e21))
  (test "and in a complex number's parts" "1.0e+21+5.0e-324i" (number->string (make-rectangular 1e21 5e-324)))
  (test "which reads back" #t (= 1e21 (string->number (number->string 1e21)))))

;; A procedure is written by its name, where it has one; a continuation as
;; one. Each is written so, not as JavaScript names its function.
(test-group "procedures"
  (define (named x) x)
  (test "a primitive, by its name" "#<procedure car>" (written write car))
  (test "a procedure defined with a name" "#<procedure named>" (written write named))
  (test "a lambda, which has none" "#<procedure>" (written write (lambda (x) x)))
  (test "a continuation" "#<continuation>" (written write (call/cc (lambda (k) k))))
  (test "displayed the same" "#<procedure car>" (written display car)))

;; R7RS 6.6 names the control characters write writes, and R7RS 6.7 gives
;; strings escapes for them; any other control character is written by its
;; code. Either way what is written reads back as the same datum.
(test-group "control characters, written to be read back"
  (test "the named characters" '("#\\alarm" "#\\backspace" "#\\delete" "#\\escape" "#\\null" "#\\tab")
        (map (lambda (c) (written write c))
             (list #\alarm #\backspace #\delete #\escape #\null #\tab)))
  (test "another control character, by its code" "#\\x1" (written write (integer->char 1)))
  (test "a string's escapes" "\"\\a\\b\\t\\n\\r\\\"\\\\\""
        (written write (string #\alarm #\backspace #\tab #\newline #\return #\" #\\)))
  (test "another control character in a string, by its code" "\"a\\x1;b\""
        (written write (string #\a (integer->char 1) #\b)))
  (test "and each reads back" #t
        (let ((data (list (string #\alarm (integer->char 1) #\x7F #\newline #\\ #\")
                          #\null #\delete (integer->char 2) #\escape)))
          (equal? data (read (open-input-string (written write data)))))))

;; A symbol is written between bars where its name would read as something
;; else: a number, the start of one, or a datum it has a delimiter of.
(test-group "symbols written between bars"
  (test "names that read as numbers, or begin as one does"
        "(|1+| |+5| |.5a| |-i| |+I| |+inf.0x| |nan.0| |-NaN.0|)"
        (written write '(|1+| |+5| |.5a| |-i| |+I| |+inf.0x| |nan.0| |-NaN.0|)))
  (test "names that begin with # or hold a delimiter"
        "(|#foo| |a b| |a(b| |a;b| |a\"b|)"
        (written write '(|#foo| |a b| |a(b| |a;b| |a"b|)))
  (test "white space outside ASCII" "|a\xA0;b|" (written write (string->symbol "a\xA0;b")))
  (test "a bar or backslash in the name is escaped" "|a\\|b\\\\c|" (written write '|a\|b\\c|))
  (test "names that need none"
        "(abc ... + - ->x a.b? lambda inferno nano |.| ||)"
        (written write (list 'abc '... '+ '- '->x (string->symbol "a.b?") 'lambda 'inferno 'nano
                             (string->symbol ".") (string->symbol ""))))
  (test "display writes none" "a b" (written display '|a b|))
  (test "and each reads back" #t
        (let ((symbols '(|1+| |-i| |+inf.0x| |#foo| |a b| |a\|b| abc ...)))
          (equal? symbols (read (open-input-string (written write symbols)))))))

;; `write` and `display` first walk a datum as a tree, looking for nothing,
;; and look for cycles only in a datum too large to walk so: a walk into a
;; cycle never ends. Past that walk's budget, what is written is the same.
(test-group "labels, past the walk as a tree"
  (test "a long list with no cycle has no labels" #t
        (let ((long (make-list 5000 'a)))
          (string=? (written write long) (written write-simple long))))
  (test "nor a structure shared too often to walk as a tree" #t
        (let ((doubled (let loop ((n 14) (x '()))
                         (if (= n 0) x (loop (- n 1) (list x x))))))
          (string=? (written write doubled) (written write-simple doubled))))
  (test "a long cycle is labelled" '("#0=(1 1" ". #0#)")
        (let ((text (written write (apply circular (make-list 5000 1)))))
          (list (substring text 0 7) (substring text (- (string-length text) 6) (string-length text)))))
  (test "a cycle deep in a long list is labelled" ". #0=(z . #0#))"
        (let* ((tail (circular 'z))
               (text (written write (append (make-list 5000 'a) tail))))
          (substring text (- (string-length text) 15) (string-length text)))))
