;; js_caller_tests.scm -- JavaScript calling Scheme procedures, in either tier.
;;
;; A Scheme procedure is a JavaScript function, so JavaScript can call it, as a
;; page does when it hands a procedure to `addEventListener` or `Array.map`.
;; Called that way the procedure is leaving Scheme, and has to behave as it
;; would for any JavaScript caller, whichever tier runs it: its arguments
;; converted into Scheme and its result out of it, and its tail calls and deep
;; recursion finished before it returns.
;;
;; Each JavaScript caller here is made with `js-eval` and describes what it got
;; back -- a type and a value, or the class of an object -- so that what
;; JavaScript saw is compared as a string rather than converted again on its
;; way back into Scheme. The file runs twice, interpreted and with the tier
;; attached (tests/run_tiered_scheme_tests_lib.js), and each procedure is
;; called twice before JavaScript is given it, since the tier compiles a
;; procedure on its second call.

;; /**
;;  * Calls a procedure from JavaScript and describes what JavaScript got back:
;;  * "number 5", "string ab", "a Values", or "threw RangeError".
;;  * @param {procedure} f - The procedure.
;;  * @param {...*} args - Its arguments, as JavaScript passes them.
;;  * @returns {string}
;;  */
(define js-sees
  (js-eval "(f, ...args) => {
              let value;
              try { value = f(...args); } catch (e) { return 'threw ' + e.constructor.name; }
              if (value === null || value === undefined) return String(value);
              if (typeof value === 'object' || typeof value === 'function') return 'a ' + value.constructor.name;
              return typeof value + ' ' + String(value);
            }"))

;; /**
;;  * Hands a procedure to JavaScript and takes back what JavaScript returns.
;;  * @param {procedure} f - The procedure.
;;  * @returns {*} What JavaScript returned: the procedure it was given.
;;  */
(define through-js (js-eval "(f) => f"))

;; /**
;;  * Whether a procedure is compiled.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {boolean}
;;  */
(define (compiled? procedure)
  (eq? #t (js-ref procedure "$compiled")))

;; /**
;;  * Whether every one of some procedures is compiled.
;;  * @param {...procedure} procedures - The procedures.
;;  * @returns {boolean}
;;  */
(define (all-compiled? . procedures)
  (not (memq #f (map compiled? procedures))))

;; ---------------------------------------------------------------------------
;; A program's own procedures
;; ---------------------------------------------------------------------------

(define (five) 5)
(define (two-letters) (string-copy "ab"))
(define (two-values) (values 1 2))
(define (even-parity n) (if (= n 0) "even" (odd-parity (- n 1))))
(define (odd-parity n) (if (= n 0) "odd" (even-parity (- n 1))))
(define (depth n) (if (= n 0) 0 (+ 1 (depth (- n 1)))))
(define (exactness x) (if (exact? x) "exact" "inexact"))

(five) (five)
(two-letters) (two-letters)
(two-values) (two-values)
(even-parity 2) (even-parity 2)
(depth 2) (depth 2)
(exactness 1) (exactness 1)

(test-group "JavaScript calling a program's own procedures"
  (test "the tier compiled them, and only in the run with it attached"
        *tier-attached*
        (all-compiled? five two-letters two-values even-parity odd-parity depth exactness))
  (test "an exact integer reaches JavaScript as a number" "number 5" (js-sees five))
  (test "a string made in Scheme reaches JavaScript as a JavaScript string"
        "string ab" (js-sees two-letters))
  (test "of several values, JavaScript receives the first" "number 1" (js-sees two-values))
  (test "tail calls 100,000 deep finish before JavaScript gets the value"
        "string even" (js-sees even-parity 100000))
  (test "a recursion 100,000 deep finishes before JavaScript gets the value"
        "number 100000" (js-sees depth 100000))
  (test "an integer JavaScript passes arrives as an exact integer"
        "string exact" (js-sees exactness 1))
  (test "a procedure handed to JavaScript and back is the same procedure"
        #t (eq? five (through-js five))))

;; ---------------------------------------------------------------------------
;; Callbacks a page makes
;; ---------------------------------------------------------------------------
;;
;; A procedure that makes procedures is compiled when it is bound, so the
;; procedures it makes are compiled from the start: the usual way a page's
;; callbacks are made.

(define (make-handlers)
  (list (lambda (name) (string-append "hello, " name))
        (lambda (n) (if (exact? n) "exact" "inexact"))
        (lambda (n)
          (letrec ((even (lambda (n) (if (= n 0) "even" (odd (- n 1)))))
                   (odd (lambda (n) (if (= n 0) "odd" (even (- n 1))))))
            (even n)))))

(define handlers (make-handlers))
(define greet (car handlers))
(define exactness-of (cadr handlers))
(define parity (caddr handlers))

(test-group "JavaScript calling the callbacks a page makes"
  (test "the tier compiled the procedure that made them, and so them"
        *tier-attached*
        (all-compiled? make-handlers greet exactness-of parity))
  (test "a string made in Scheme reaches JavaScript as a JavaScript string"
        "string hello, Ann" (js-sees greet "Ann"))
  (test "an integer JavaScript passes arrives as an exact integer"
        "string exact" (js-sees exactness-of 1))
  (test "tail calls 100,000 deep finish before JavaScript gets the value"
        "string even" (js-sees parity 100000))
  (test "a callback handed to JavaScript and back is the same procedure"
        #t (eq? greet (through-js greet))))

;; ---------------------------------------------------------------------------
;; A continuation JavaScript is given
;; ---------------------------------------------------------------------------
;;
;; A continuation is a Scheme procedure too, and JavaScript calling it passes
;; JavaScript values, which arrive converted as a closure's arguments do.

;; /**
;;  * Gives JavaScript the continuation of the call and returns what it is
;;  * invoked with.
;;  * @param {procedure} give - A JavaScript function, given the continuation.
;;  * @returns {*} The value the continuation is invoked with.
;;  */
(define (value-given-back give)
  (call-with-current-continuation (lambda (k) (give k) "not invoked")))

(define (exactness-given-back give) (if (exact? (value-given-back give)) "exact" "inexact"))

(value-given-back (js-eval "(k) => k(1)"))
(value-given-back (js-eval "(k) => k(1)"))
(exactness-given-back (js-eval "(k) => k(1)"))
(exactness-given-back (js-eval "(k) => k(1)"))

(test-group "JavaScript calling a continuation"
  (test "the tier compiled the procedures that capture it, and only in the run with it attached"
        *tier-attached* (all-compiled? value-given-back exactness-given-back))
  (test "an integer JavaScript passes arrives as an exact integer"
        "exact" (exactness-given-back (js-eval "(k) => k(1)")))
  (test "a string JavaScript passes arrives as a string"
        "ab" (value-given-back (js-eval "(k) => k('ab')"))))

;; ---------------------------------------------------------------------------
;; A procedure that leaves other than by returning
;; ---------------------------------------------------------------------------
;;
;; What a call from JavaScript does when the procedure raises, escapes through
;; a continuation captured outside the call, captures and re-enters one inside
;; it, or tail-calls a procedure the tier has not compiled: the same whichever
;; tier runs the procedure.

;; /**
;;  * Calls a procedure from JavaScript with no arguments and returns what it
;;  * returned, as JavaScript received it.
;;  */
(define call-from-js (js-eval "(f) => f()"))

(define (fails) (car '()))
(define (counts-by-reentry)
  (let ((n 0) (again #f))
    (call/cc (lambda (k) (set! again k)))
    (set! n (+ n 1))
    (if (< n 3) (again #f) n)))
(define (not-yet-compiled) "interpreted")
(define (tail-calls-interpreted) (not-yet-compiled))
(define (escapes k) (k "escaped"))

(define (fails-safely) (guard (e (#t "caught")) (call-from-js fails)))
(define (escape-from-js) (call/cc (lambda (k) (call-from-js (lambda () (escapes k))))))

(guard (e (#t #f)) (fails)) (guard (e (#t #f)) (fails))
(counts-by-reentry) (counts-by-reentry)
(tail-calls-interpreted) (tail-calls-interpreted)
(escapes (lambda (x) x)) (escapes (lambda (x) x))
(fails-safely) (fails-safely)
(escape-from-js) (escape-from-js)

(test-group "JavaScript calling a procedure that leaves other than by returning"
  ;; `fails-safely`, built on `guard`, the tier declines (`safety.scm`); what
  ;; it calls from JavaScript is compiled.
  (test "the tier compiled them, and only in the run with it attached"
        *tier-attached* (all-compiled? fails counts-by-reentry tail-calls-interpreted escapes))
  ;; Raised beneath a handler in the Scheme that called JavaScript, an error
  ;; goes to that handler, as it would with no JavaScript between.
  (test "an error raised in it goes to a guard around the call from JavaScript" "caught" (fails-safely))
  (test "a continuation captured outside the call escapes through it" "escaped" (escape-from-js))
  (test "a continuation captured inside it is re-entered before it returns" "number 3" (js-sees counts-by-reentry))
  (test "a tail call to a procedure the tier has not compiled finishes" "string interpreted"
        (js-sees tail-calls-interpreted)))

;; ---------------------------------------------------------------------------
;; A procedure called as a method
;; ---------------------------------------------------------------------------

;; /**
;;  * A procedure for JavaScript to call as a method of an object, answering
;;  * the object's name: `this` is the receiver, which the interpreter binds as
;;  * JavaScript calls a procedure as a method. The tier compiles a procedure
;;  * that makes one when it is bound, and compiled code binds no receiver, so
;;  * it leaves this one interpreted.
;;  * @returns {procedure}
;;  */
(define (make-greeter) (lambda () (js-ref this "name")))

(define greeted (js-obj "name" "greeted"))
(js-set! greeted "greet" (make-greeter))

(test-group "JavaScript calling a procedure as a method"
  (test "it reads its receiver as this" "greeted" (js-invoke greeted "greet"))
  (test "the tier leaves interpreted what reads this" #f (compiled? make-greeter)))
