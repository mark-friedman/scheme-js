;; local_names_tests.scm -- locals whose names JavaScript, or the compiler's
;; own code, already has a use for.
;;
;; Compiled code names a Scheme local much as it was written, so that a
;; debugger shows `items` rather than a mangled name: `found?` becomes
;; `found_p`, and two locals of one name in a procedure are told apart by a
;; suffix. A name that would mean something else in the generated JavaScript --
;; a reserved word, `arguments` or `undefined`, the runtime `R`, the
;; environment `E`, the constants `K`, a global's cell `C0` -- has to be named
;; otherwise, or the code would read the local where it meant its own. These
;; procedures use those names, and check that each tier gives the same
;; answers, through a frame saved and restored by a continuation too.
;;
;; The file runs twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js). A procedure that makes a procedure
;; is compiled when it is bound; any other is called twice before it is
;; tested, since the tier compiles a procedure on its second call.

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
(define (error-message thunk)
  (guard (e ((error-object? e) (error-object-message e)))
    (thunk)
    #f))

;; Locals named as JavaScript's reserved words and its `arguments` and
;; `undefined`, beside code whose JavaScript checks `arguments.length`, writes
;; an unspecified value as `undefined`, reads a constant, and an infinity,
;; which JavaScript writes with `Number`.
(define (javascript-words new arguments undefined eval)
  (list new arguments undefined eval
        (eq? (when (eq? new 'never) 'x) (if #f #f))
        '(a constant)
        +inf.0))

;; Locals named as the generated code's own: the runtime, the environment, the
;; constants, a global's cell and its accessor, and the globals the code uses.
(define (emitter-words R E K)
  (let ((C0 (car R)) (G0 (cdr R)) (P0 E) (W0 K) (Array 'a) (Error 'e) (Number 'n))
    (list C0 G0 P0 W0 Array Error Number (length (list R E K)) '(a constant) +inf.0)))

;; One name bound again and again in one procedure, and names a suffix might
;; have made.
(define (shadowed x)
  (let ((x (+ x 1)))
    (let ((x (* x 2)))
      (list x (let ((x-2 'a) (x_2 'b) (x_3 'c)) (list x-2 x_2 x_3 x))))))

;; A procedure made with free variables, and a parameter, of the emitter's
;; names: a nested procedure's free variables are its factory's parameters.
(define (make-adder R E K)
  (lambda (C0) (+ R E K C0)))

;; A rest parameter named `arguments`.
(define (rest-named first . arguments)
  (cons first arguments))

;; A continuation re-entered, so that the frame it saved is restored: its
;; locals by their names.
(define (reenter new arguments)
  (let* ((k #f)
         (count 0)
         (v (+ new (call/cc (lambda (c) (set! k c) arguments)))))
    (set! count (+ count 1))
    (if (< count 3) (k (+ v 1)) (list v count))))

(javascript-words 1 2 3 4)
(javascript-words 1 2 3 4)
(emitter-words '(1 . 2) 3 4)
(emitter-words '(1 . 2) 3 4)
(shadowed 1)
(shadowed 1)
(rest-named 1 2)
(rest-named 1 2)
(reenter 1 10)
(reenter 1 10)

(test-group "Locals named as JavaScript's words, or the compiler's own"
  (test "the procedures are compiled under the tier"
        (list *tier-attached* *tier-attached* *tier-attached* *tier-attached* *tier-attached*)
        (map compiled? (list javascript-words emitter-words shadowed rest-named reenter)))
  (test "JavaScript's reserved words, arguments and undefined"
        '(1 2 3 4 #t (a constant) +inf.0)
        (javascript-words 1 2 3 4))
  (test "the generated code's own names"
        '(1 2 3 4 a e n 3 (a constant) +inf.0)
        (emitter-words '(1 . 2) 3 4))
  (test "a call with the wrong number of arguments, a local being named R"
        "emitter-words: wrong number of arguments (expected 3, got 2)"
        (error-message (lambda () (emitter-words 1 2))))
  (test "one name bound three times, beside names a suffix might have made"
        '(4 (a b c 4))
        (shadowed 1))
  (test "free variables and a parameter of the emitter's names" 10 ((make-adder 1 2 3) 4))
  (test "a rest parameter named arguments" '(1 2 3) (rest-named 1 2 3))
  (test "a frame restored by a continuation re-entered" '(15 3) (reenter 1 10)))
