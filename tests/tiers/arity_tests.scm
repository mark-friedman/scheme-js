;; arity_tests.scm -- a procedure called with the wrong number of arguments.
;;
;; It is an error (R7RS 4.1.4); this implementation signals it, in either tier,
;; for a call from Scheme: chibi's tests of (chibi term ansi) expect it, and a
;; procedure that drops an extra argument hides the mistake that made it. A
;; call from JavaScript is not checked. JavaScript calls a function with the
;; arguments it has to give -- an event handler with the event, `Array.map`'s
;; callback with an index and the array -- so a procedure called by
;; JavaScript takes the arguments it has parameters for, and a parameter given
;; none is left undefined, as before.
;;
;; The file runs twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js); each procedure is called twice
;; before it is tested, since the tier compiles a procedure on its second call.

;; /**
;;  * Whether a procedure is compiled.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {boolean}
;;  */
(define (compiled? procedure)
  (eq? #t (js-ref procedure "$compiled")))

;; /**
;;  * The message of the error a thunk raises, or #f if it raises none.
;;  * @param {procedure} thunk - The thunk.
;;  * @returns {string|boolean}
;;  */
(define (arity-message thunk)
  (guard (e ((error-object? e) (error-object-message e)))
    (thunk)
    #f))

(define (arity-three a b c) (list a b c))
(define (arity-rest a . more) (cons a more))
(define (arity-none) 'none)
(define (arity-caller f) (f 1 2 3 4))

(arity-three 1 2 3)
(arity-three 1 2 3)
(arity-rest 1)
(arity-rest 1 2)
(arity-none)
(arity-none)
(arity-caller list)
(arity-caller list)

(test-group "a procedure called with the wrong number of arguments"

  (test "too many"
    "arity-three: wrong number of arguments (expected 3, got 4)"
    (arity-message (lambda () (arity-three 1 2 3 4))))

  (test "too few"
    "arity-three: wrong number of arguments (expected 3, got 2)"
    (arity-message (lambda () (arity-three 1 2))))

  (test "one for a procedure taking none"
    "arity-none: wrong number of arguments (expected 0, got 1)"
    (arity-message (lambda () (arity-none 1))))

  (test "too few before a rest parameter"
    "arity-rest: wrong number of arguments (expected at least 1, got 0)"
    (arity-message (lambda () (arity-rest))))

  (test "a rest parameter takes any more"
    '(1 2 3 4)
    (arity-rest 1 2 3 4))

  (test "an anonymous procedure"
    #t
    (string? (arity-message (lambda () ((lambda (x) x) 1 2)))))

  (test "through apply"
    #t
    (string? (arity-message (lambda () (apply arity-three '(1 2 3 4))))))

  (test "through map"
    #t
    (string? (arity-message (lambda () (map arity-three '(1 2))))))

  (test "from a compiled caller"
    #t
    (string? (arity-message (lambda () (arity-caller arity-three)))))

  (test "an error guard catches"
    'caught
    (guard (e (#t 'caught)) (arity-three 1))))

;; /**
;;  * Calls a procedure from JavaScript with some arguments and returns what
;;  * it returned, or the class of what it threw.
;;  * @param {procedure} f - The procedure.
;;  * @param {...*} args - Its arguments, as JavaScript passes them.
;;  * @returns {*}
;;  */
(define js-calls
  (js-eval "(f, ...args) => {
              try { return f(...args); } catch (e) { return 'threw ' + e.constructor.name; }
            }"))

(define (arity-one x) (if (eq? x (js-eval "undefined")) 'undefined x))

(arity-one 1)
(arity-one 1)

(test-group "a procedure JavaScript calls"

  (test "takes the arguments it has parameters for"
    7
    (js-calls arity-one 7 8 9))

  (test "is left undefined for a parameter it is given none for"
    'undefined
    (js-calls arity-one))

  (test "with a rest parameter, takes them all"
    '(1 2 3)
    (js-calls arity-rest 1 2 3)))

(test-group "the procedures tested"

  (test "are compiled, with the tier attached"
    (if *tier-attached* '(#t #t #t #t #t) '(#f #f #f #f #f))
    (map compiled? (list arity-three arity-rest arity-none arity-caller arity-one))))
