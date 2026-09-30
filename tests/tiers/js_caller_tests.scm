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

;; /**
;;  * Why a JavaScript caller's view of a compiled procedure is expected to be
;;  * wrong in the run with the tier attached, and #f in the other.
;;  * @type {string|boolean}
;;  */
(define no-javascript-entry
  (and *tier-attached*
       "a compiled procedure has no JavaScript-facing entry, so a JavaScript caller gets compiled code's own calling convention"))

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
  (test-expect-fail no-javascript-entry
    (test "an exact integer reaches JavaScript as a number" "number 5" (js-sees five))
    (test "a string made in Scheme reaches JavaScript as a JavaScript string"
          "string ab" (js-sees two-letters))
    (test "of several values, JavaScript receives the first" "number 1" (js-sees two-values))
    (test "tail calls 100,000 deep finish before JavaScript gets the value"
          "string even" (js-sees even-parity 100000))
    (test "a recursion 100,000 deep finishes before JavaScript gets the value"
          "number 100000" (js-sees depth 100000))
    (test "an integer JavaScript passes arrives as an exact integer"
          "string exact" (js-sees exactness 1)))
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
  (test-expect-fail no-javascript-entry
    (test "a string made in Scheme reaches JavaScript as a JavaScript string"
          "string hello, Ann" (js-sees greet "Ann"))
    (test "an integer JavaScript passes arrives as an exact integer"
          "string exact" (js-sees exactness-of 1))
    (test "tail calls 100,000 deep finish before JavaScript gets the value"
          "string even" (js-sees parity 100000)))
  (test "a callback handed to JavaScript and back is the same procedure"
        #t (eq? greet (through-js greet))))
