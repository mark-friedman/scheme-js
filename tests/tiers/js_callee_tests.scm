;; js_callee_tests.scm -- Scheme calling JavaScript functions, in either tier.
;;
;; A JavaScript function is given JavaScript values, and what it returns comes
;; back converted into Scheme. Its arguments are converted out of Scheme
;; throughout, as `schemeToJsDeep` in src/core/interpreter/js_interop.js
;; converts -- an exact integer or a rational to a number, a character or a
;; Scheme string to a JavaScript string, a vector to a new array of converted
;; elements -- and a list is passed as its pairs. Its result arrives converted as
;; `js-invoke` converts it, one level deep (`jsToScheme`): an integral number is
;; an exact integer, and an array or object is JavaScript's own, its contents as
;; they are. Both conversions are fixed. They are the same however the program
;; calls the function and whichever tier runs the program, and no dynamic state
;; changes them: the interpreter calls a JavaScript function itself, compiled
;; code calls one through `callForeign` in src/core/interpreter/values.js where
;; the call is not in tail position and hands a tail call to it to the
;; interpreter, dot notation calls a method through `js-invoke`, and `js-new`
;; constructs. All of them must agree.
;;
;; The file runs twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js), and each procedure is called twice
;; before it is tested, since the tier compiles a procedure on its second call.

;; ---------------------------------------------------------------------------
;; A JavaScript function's result
;; ---------------------------------------------------------------------------

(define js-two (js-eval "() => 2"))
(define js-half (js-eval "() => 0.5"))
(define js-echo (js-eval "(x) => x"))
(define js-pair (js-eval "() => [1, 2]"))
(define holder (js-eval "({ two: () => 2 })"))

;; /**
;;  * Whether a procedure is compiled.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {boolean}
;;  */
(define (compiled? procedure)
  (eq? #t (js-ref procedure "$compiled")))

(define (two-directly) (js-two))
(define (half-directly) (js-half))
(define (two-through-js-invoke) (js-invoke holder "two"))
(define (echoed n) (js-echo n))
(define (pair-from-js) (js-pair))
;; Not in tail position: a tail call to a JavaScript function is returned to
;; the interpreter, which makes it.
(define (two-not-in-tail) (let ((n (js-two))) n))

(define (twice thunk) (thunk) (thunk))
(twice two-directly)
(twice half-directly)
(twice two-through-js-invoke)
(echoed 1) (echoed 1)
(twice pair-from-js)
(twice two-not-in-tail)

(test-group "A JavaScript function's result"
  (test "the tier compiled the callers, and only in the run with it attached"
        *tier-attached*
        (and (compiled? two-directly) (compiled? two-not-in-tail) (compiled? pair-from-js)))
  (test "an integral number arrives exact, called directly" #t (exact? (two-directly)))
  (test "and called from compiled code not in tail position" #t (exact? (two-not-in-tail)))
  (test "as it does through js-invoke" #t (exact? (two-through-js-invoke)))
  (test "a number that is not integral arrives inexact" #f (exact? (half-directly)))
  (test "an exact integer passed to JavaScript and returned is exact again" #t (exact? (echoed 3)))
  ;; An integral number is an exact integer wherever it is, inside an array as
  ;; much as returned (src/core/interpreter/number_representation.js).
  (test "an array arrives as it is, its integral elements exact" '(#t #t)
        (map exact? (vector->list (pair-from-js)))))

;; ---------------------------------------------------------------------------
;; A JavaScript function's arguments
;; ---------------------------------------------------------------------------

;; Each test of the arguments has JavaScript describe what it was given, so
;; that what JavaScript saw is compared as a string rather than converted again
;; on its way back into Scheme.

;; /**
;;  * An object whose `describe` method describes its arguments as JavaScript
;;  * sees them: "number 5", "string ab", "a Cons", an array as its elements in
;;  * brackets, and several arguments separated by " | ". Its `Recorder` is a
;;  * class whose instances keep, as `seen`, the same description of the
;;  * arguments they were constructed with.
;;  * @type {Object}
;;  */
(define describer
  (js-eval "(() => {
              const show = (v) => Array.isArray(v) ? '[' + v.map(show).join(', ') + ']'
                : v === null ? 'null'
                : typeof v === 'object' ? 'a ' + v.constructor.name
                : typeof v + ' ' + String(v);
              const describe = (...args) => args.map(show).join(' | ');
              return { describe, Recorder: class { constructor(...args) { this.seen = describe(...args); } } };
            })()"))

;; /**
;;  * The same method, as a JavaScript function a program calls directly.
;;  * @type {procedure}
;;  */
(define describe-arguments (js-ref describer "describe"))

;; /**
;;  * The class that records the arguments it was constructed with.
;;  * @type {procedure}
;;  */
(define Recorder (js-ref describer "Recorder"))

;; /**
;;  * Whether every one of some procedures is compiled.
;;  * @param {...procedure} procedures - The procedures.
;;  * @returns {boolean}
;;  */
(define (all-compiled? . procedures)
  (not (memv #f (map compiled? procedures))))

;; The ways a program calls a JavaScript function, for the arguments' tests.

;; /**
;;  * Calls a JavaScript function in tail position.
;;  * @param {procedure} f - The function.
;;  * @param {*} x - Its argument.
;;  * @returns {*} What it returned.
;;  */
(define (tail-call f x) (f x))

;; /**
;;  * Calls a JavaScript function where its result is still to be used.
;;  * @param {procedure} f - The function.
;;  * @param {*} x - Its argument.
;;  * @returns {*} What it returned.
;;  */
(define (non-tail-call f x) (car (list (f x))))

;; /**
;;  * Calls the describer's method through dot notation.
;;  * @param {*} x - Its argument.
;;  * @returns {string} The description.
;;  */
(define (method-call x) (describer.describe x))

;; /**
;;  * Constructs a recorder with `js-new`, and reads what it was given.
;;  * @param {*} x - Its argument.
;;  * @returns {string} The description.
;;  */
(define (construction x) (js-ref (js-new Recorder x) "seen"))

;; /**
;;  * What JavaScript was given, each way: the descriptions from a tail call, a
;;  * call in other positions, a method call and a construction, which should
;;  * all be the same.
;;  * @param {*} x - The argument.
;;  * @returns {list} The four descriptions.
;;  */
(define (seen-each-way x)
  (list (tail-call describe-arguments x)
        (non-tail-call describe-arguments x)
        (method-call x)
        (construction x)))

(seen-each-way 1) (seen-each-way 1)

(test-group "A JavaScript function's arguments"
  (test "the tier compiled the callers, and only in the run with it attached"
        *tier-attached*
        (all-compiled? tail-call non-tail-call method-call construction seen-each-way))
  (test "an exact integer reaches JavaScript as a number"
        '("number 5" "number 5" "number 5" "number 5") (seen-each-way 5))
  (test "an exact rational reaches JavaScript as a number"
        '("number 0.5" "number 0.5" "number 0.5" "number 0.5") (seen-each-way 1/2))
  (test "an inexact number reaches JavaScript as it is"
        '("number 2.5" "number 2.5" "number 2.5" "number 2.5") (seen-each-way 2.5))
  (test "a character reaches JavaScript as a string"
        '("string a" "string a" "string a" "string a") (seen-each-way #\a))
  (test "a string made in Scheme reaches JavaScript as a JavaScript string"
        '("string ab" "string ab" "string ab" "string ab") (seen-each-way (string-copy "ab")))
  (test "a vector reaches JavaScript as an array, converted throughout"
        '("[number 1, [number 2, string b]]"
          "[number 1, [number 2, string b]]"
          "[number 1, [number 2, string b]]"
          "[number 1, [number 2, string b]]")
        (seen-each-way (vector 1 (vector 2 #\b))))
  (test "a vector reaches JavaScript as a new array, not the vector itself"
        #f
        (let ((v (vector 1 2)))
          (eq? v (tail-call (js-eval "(a) => a") v))))
  (test "a list reaches JavaScript as its pairs"
        '("a Cons" "a Cons" "a Cons" "a Cons") (seen-each-way (list 1 2)))
  (test-error "an exact integer beyond 2^53, called in tail position, is refused"
              "outside safe integer range"
              (tail-call describe-arguments (expt 2 60)))
  (test-error "an exact integer beyond 2^53, called in other positions, is refused"
              "outside safe integer range"
              (non-tail-call describe-arguments (expt 2 60)))
  (test-error "an exact integer beyond 2^53, passed to a method, is refused"
              "outside safe integer range"
              (method-call (expt 2 60)))
  (test-error "an exact integer beyond 2^53, passed to a constructor, is refused"
              "outside safe integer range"
              (construction (expt 2 60))))
