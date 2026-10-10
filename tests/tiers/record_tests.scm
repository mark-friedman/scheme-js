;; record_tests.scm -- a record's accessors and modifiers, in either tier.
;;
;; Compiled code reads and writes a record's field inline where the global it
;; calls holds an accessor or a modifier as the code is compiled, checking as
;; it runs that the callee is still one, of that field, and that the record is
;; of its type, and calling it otherwise ("Records" in src/compiler/emit.scm).
;; These check that the answers are the interpreter's: fields holding a
;; symbol, an exact integer, an inexact integer Scheme stored and one
;; JavaScript wrote; the accessor's error for what is not its record; and a
;; name rebound to another type's accessor of the same field, to another
;; field's, and to a procedure that is no accessor at all.
;;
;; The file runs twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js), and each procedure is called twice
;; before it is tested, since the tier compiles a procedure on its second call.

(define-record-type point (make-point x y) point? (x point-x set-point-x!) (y point-y set-point-y!))
(define-record-type label (make-label x) label? (x label-x))

;; /**
;;  * A point's two fields.
;;  * @param {point} p - The point.
;;  * @returns {list}
;;  */
(define (fields p) (list (point-x p) (point-y p)))

;; /**
;;  * A point with its fields set, and its fields after.
;;  * @param {point} p - The point.
;;  * @param {*} x - Its x.
;;  * @param {*} y - Its y.
;;  * @returns {list}
;;  */
(define (store! p x y)
  (set-point-x! p x)
  (set-point-y! p y)
  (fields p))

;; /**
;;  * A point's x, read where the reading is the procedure's value.
;;  * @param {point} p - The point.
;;  * @returns {*}
;;  */
(define (x-of p) (point-x p))

;; /**
;;  * Whether a thunk raises an error.
;;  * @param {procedure} thunk - The thunk.
;;  * @returns {boolean}
;;  */
(define (refuses? thunk)
  (guard (e ((error-object? e) #t)) (thunk) #f))

(define a-point (make-point 'a 1))
(fields a-point)
(fields a-point)
(store! (make-point 0 0) 1 2)
(store! (make-point 0 0) 1 2)
(x-of a-point)
(x-of a-point)

(test-group "Records"
  (test "the tier compiled them" (list *tier-attached* *tier-attached* *tier-attached*)
        (map (lambda (p) (eq? #t (js-ref p "$compiled"))) (list fields store! x-of)))
  (test "a field holding a symbol, and one an exact integer" '(a 1) (fields a-point))
  (test "read where the reading is the value" 'a (x-of a-point))
  (test "set, and read back" '(b 2) (store! (make-point 0 0) 'b 2))
  (test "an inexact integer Scheme stored reads back inexact" '(#t #t)
        (let ((stored (store! (make-point 0 0) 3. -0.)))
          (list (eqv? (car stored) 3.) (eqv? (cadr stored) -0.))))
  (test "an exact integer stored over an inexact one reads exact" '(4 5)
        (let ((p (make-point 3. 4.)))
          (store! p 4 5)))
  (test "an integer JavaScript wrote reads exact" '(7 #t)
        (let ((p (make-point 0 0)))
          (js-set! p "x" (js-eval "7"))
          (let ((x (car (fields p))))
            (list x (exact? x)))))
  (test "the accessor's error for what is not its record" #t
        (refuses? (lambda () (fields (make-label 'a)))))
  (test "and the modifier's" #t (refuses? (lambda () (store! (make-label 'a) 1 2))))
  (test "and for what is no record at all" #t (refuses? (lambda () (fields '(a 1))))))

;; The name rebound, after the procedures that call it were compiled.
(define saved-point-x point-x)

(test-group "Records - the name rebound"
  (set! point-x label-x)
  (test "to another type's accessor of the same field: that type's records" 'b (x-of (make-label 'b)))
  (test "whose error is its own for this type's" #t (refuses? (lambda () (x-of a-point))))
  (set! point-x point-y)
  (test "to another field's accessor" 1 (x-of a-point))
  (set! point-x (lambda (p) 'rebound))
  (test "to a procedure that is no accessor" 'rebound (x-of a-point))
  (set! point-x saved-point-x)
  (test "and back" 'a (x-of a-point)))
