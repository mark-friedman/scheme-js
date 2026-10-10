;; (scheme-js devtools): how DevTools shows Scheme values -- the header and
;; body its custom formatters draw a value with, as the markup the formatter's
;; JavaScript turns into DevTools' JsonML, and when a value is Scheme's to
;; draw: one only Scheme has always, a vector or bytevector, which are
;; JavaScript arrays too, only while paused in Scheme, unless the switch says
;; otherwise. What DevTools makes of them is tested by driving it
;; (tests/devtools/stepping_tests.js).

(import (scheme-js devtools))

(define-record-type point (make-point x y) point? (x point-x) (y point-y))

(define (sample-procedure x) x)

;; Where a pause in compiled Scheme is, and one in JavaScript.
(define in-scheme "scheme:///app.scm/main")
(define in-javascript "http://example.com/app.js")

(define (header value where) (devtools-header value where))
(define (text value where) (let ((h (header value where))) (and h (caddr h))))

(test-group "devtools - what is Scheme's to draw"
  (set-devtools-display! 'auto)
  (test "a list, paused in Scheme" "(1 2 3)" (text '(1 2 3) in-scheme))
  (test "and paused in JavaScript, since only Scheme has pairs" "(1 2 3)" (text '(1 2 3) in-javascript))
  (test "and not paused at all" "(1 2 3)" (text '(1 2 3) ""))
  (test "a vector, paused in Scheme" "#(1 2 3)" (text (vector 1 2 3) in-scheme))
  (test "but not paused in JavaScript, where it is an array" #f (header (vector 1 2 3) in-javascript))
  (test "nor not paused" #f (header (vector 1 2 3) ""))
  (test "a bytevector likewise" '("#u8(1 2)" #f) (list (text (bytevector 1 2) in-scheme) (header (bytevector 1 2) in-javascript)))
  (test "a JavaScript object is never Scheme's" #f (header (js-obj "a" 1) in-scheme))
  (test "nor a JavaScript function" #f (header (js-eval "(function g() {})") in-scheme))
  (set-devtools-display! 'scheme)
  (test "drawing everything as Scheme, a vector paused in JavaScript too" "#(1 2 3)" (text (vector 1 2 3) in-javascript))
  (set-devtools-display! 'javascript)
  (test "drawing everything as JavaScript, not even a list" #f (header '(1 2 3) in-scheme))
  (set-devtools-display! 'auto)
  (test "the switch says what it is" 'auto (devtools-display))
  (test "and takes nothing else" #t
        (guard (e ((error-object? e) #t)) (set-devtools-display! 'sideways) #f)))

(test-group "devtools - a value's header"
  (test "a symbol" "found?" (text 'found? in-scheme))
  (test "a character" "#\\a" (text #\a in-scheme))
  (test "a string" "\"abc\"" (text (string-copy "abc") in-scheme))
  (test "an inexact integer, which JavaScript would show as 1" "1.0" (text (inexact 1) in-scheme))
  (test "a procedure by its name" "#<procedure sample-procedure>" (text sample-procedure in-scheme))
  (test "a record by its type and fields" "#<point x: 1 y: 2>" (text (make-point 1 2) in-scheme))
  (test "an improper list" "(1 2 . 3)" (text '(1 2 . 3) in-scheme))
  (test "a long list, cut short" "(0 1 2 3 4 5 6 7 8 9 ...)"
        (text (let count ((i 19) (numbers '())) (if (< i 0) numbers (count (- i 1) (cons i numbers)))) in-scheme))
  (test "a deep one, too" "(1 (2 (3 (...))))" (text '(1 (2 (3 (4 (5))))) in-scheme))
  (test "a long string, cut short" #t
        (let ((t (text (make-string 200 #\x) in-scheme))) (< (string-length t) 100)))
  (test "a list holding itself ends" #t
        (let ((cycle (list 1 2))) (set-cdr! (cdr cycle) cycle) (string? (text cycle in-scheme)))))

(test-group "devtools - a value's body"
  (define (rows value) (cddr (devtools-body value in-scheme)))
  (define (last-row value) (car (reverse (rows value))))
  (test "a value Scheme draws has one" #t (devtools-has-body? '(1 2) in-scheme))
  (test "a list's are its elements, each drawn as DevTools draws it" '((li #f "0: " (object 1 #f)) (li #f "1: " (object b #f)))
        (list (car (rows '(1 b))) (cadr (rows '(1 b)))))
  (test "an improper list's tail is a row of its own" '(li #f ". " (object 3 #f))
        (caddr (rows '(1 2 . 3))))
  (test "a record's are its fields" '((li #f "x: " (object 1 #f)) (li #f "y: " (object 2 #f)))
        (list (car (rows (make-point 1 2))) (cadr (rows (make-point 1 2)))))
  (test "and every one's last is the value as JavaScript draws it" #t
        (let ((value '(1 2)))
          (equal? (last-row value) (list 'li #f "JavaScript: " (list 'object value #t))))))
