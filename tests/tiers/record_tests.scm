;; record_tests.scm -- a record's constructor, predicate, accessors and
;; modifiers, in either tier.
;;
;; Compiled code makes a record, tests one, and reads and writes its fields
;; inline where the global it calls holds a constructor, a predicate, an
;; accessor or a modifier as the code is compiled, checking as it runs that the
;; callee is still one -- a constructor of those fields from those arguments,
;; an accessor or modifier of that field and the record of its type -- and
;; calling it otherwise ("Records" in src/compiler/emit.scm). These check that
;; the answers are the interpreter's: fields holding a symbol, an exact
;; integer, an inexact integer Scheme stored and one JavaScript wrote; records
;; made with their fields in order, out of order and some left out, which the
;; type's procedures, the printer and JavaScript read as they read one the
;; interpreter made; the constructor's error for the wrong number of
;; arguments, and the accessor's for what is not its record; a predicate of
;; its own records, another type's and what is no record; and a name rebound
;; to another type's procedure of the same kind, to one of the same type but
;; another field or order, and to a procedure that is none of them.
;;
;; The file runs twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js), and each procedure is called twice
;; before it is tested, since the tier compiles a procedure on its second call.

(define-record-type point (make-point x y) point? (x point-x set-point-x!) (y point-y set-point-y!))
(define-record-type label (make-label x) label? (x label-x))
;; A constructor taking its fields out of order, and one leaving one out.
(define-record-type swapped (make-swapped y x) swapped? (x swapped-x) (y swapped-y))
(define-record-type partial (make-partial y) partial? (x partial-x set-partial-x!) (y partial-y))
;; Another type with a point's fields, and one with them in the other order.
(define-record-type spot (make-spot x y) spot? (x spot-x) (y spot-y))
(define-record-type turned (make-turned y x) turned? (y turned-y) (x turned-x))

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

;; /**
;;  * A point made of its fields, in order.
;;  * @param {*} x - Its x.
;;  * @param {*} y - Its y.
;;  * @returns {point}
;;  */
(define (point-of x y) (make-point x y))

;; /**
;;  * A record whose constructor takes its fields out of order, and its fields.
;;  * @param {*} x - Its x.
;;  * @param {*} y - Its y.
;;  * @returns {list}
;;  */
(define (swapped-of x y)
  (let ((s (make-swapped y x)))
    (list (swapped? s) (swapped-x s) (swapped-y s))))

;; /**
;;  * A record whose constructor leaves a field out, the field set after.
;;  * @param {*} y - Its y.
;;  * @returns {list}
;;  */
(define (partial-of y)
  (let ((p (make-partial y)))
    (set-partial-x! p 'later)
    (list (partial? p) (partial-x p) (partial-y p))))

;; /**
;;  * A point made with one argument too few.
;;  * @returns {point}
;;  */
(define (too-few) (make-point 1))

;; /**
;;  * Which of the types a value is of, tested in order.
;;  * @param {*} v - The value.
;;  * @returns {symbol}
;;  */
(define (kind-of v)
  (cond ((point? v) 'point) ((label? v) 'label) ((swapped? v) 'swapped) (else 'none)))

;; /**
;;  * How a value is written.
;;  * @param {*} v - The value.
;;  * @returns {string}
;;  */
(define (written v)
  (let ((port (open-output-string)))
    (write v port)
    (get-output-string port)))

(define a-point (make-point 'a 1))
(fields a-point)
(fields a-point)
(store! (make-point 0 0) 1 2)
(store! (make-point 0 0) 1 2)
(x-of a-point)
(x-of a-point)
(point-of 1 2)
(point-of 1 2)
(swapped-of 1 2)
(swapped-of 1 2)
(partial-of 1)
(partial-of 1)
(refuses? too-few)
(refuses? too-few)
(kind-of a-point)
(kind-of a-point)

(test-group "Records"
  (test "the tier compiled them" (make-list 8 *tier-attached*)
        (map (lambda (p) (eq? #t (js-ref p "$compiled")))
             (list fields store! x-of point-of swapped-of partial-of too-few kind-of)))
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

(test-group "Records - made and tested"
  (test "made with its fields in order" '(#t a 1) (let ((p (point-of 'a 1))) (list (point? p) (point-x p) (point-y p))))
  (test "a new record each time" #f (let ((p (point-of 'a 1))) (eq? p (point-of 'a 1))))
  (test "made with its fields out of order" '(#t a b) (swapped-of 'a 'b))
  (test "made with a field left out, set after" '(#t later b) (partial-of 'b))
  (test "an inexact integer made into a field reads back inexact, an exact one exact" '(#t #t)
        (let ((p (point-of 3. 4)))
          (list (eqv? (point-x p) 3.) (eqv? (point-y p) 4))))
  (test "written as the interpreter's" (written (make-point 'a "b")) (written (point-of 'a "b")))
  (test "and equal? to it as one of the interpreter's is to another"
        (equal? (make-point 'a "b") (make-point 'a "b")) (equal? (make-point 'a "b") (point-of 'a "b")))
  (test "its fields as JavaScript reads them" '(a 1) (let ((p (point-of 'a 1))) (list (js-ref p "x") (js-ref p "y"))))
  (test "the constructor's error for too few arguments" #t (refuses? too-few))
  (test "a predicate of its own records and of another type's" '(point label swapped)
        (map kind-of (list (point-of 1 2) (make-label 1) (make-swapped 1 2))))
  (test "and of what is no record" '(none none none none none none)
        (map kind-of (list '(1 2) 1 #f 'point "point" kind-of))))

;; The names rebound, after the procedures that call them were compiled.
(define saved-make-point make-point)
(define saved-point? point?)

(test-group "Records - a constructor or predicate rebound"
  (set! make-point make-spot)
  (test "a constructor to another type's of the same fields: that type's record" '(#t #f 1 2)
        (let ((r (point-of 1 2))) (list (spot? r) (saved-point? r) (spot-x r) (spot-y r))))
  (set! make-point make-turned)
  (test "to one of the fields in the other order" '(#t 2 1)
        (let ((r (point-of 1 2))) (list (turned? r) (turned-x r) (turned-y r))))
  (set! make-point list)
  (test "to a procedure that is no constructor" '(1 2) (point-of 1 2))
  (set! make-point saved-make-point)
  (test "and back" '(#t 1) (let ((r (point-of 1 2))) (list (point? r) (point-x r))))
  (set! point? label?)
  (test "a predicate to another type's" '(point swapped) (list (kind-of (make-label 1)) (kind-of (make-swapped 1 2))))
  (set! point? (lambda (v) (eq? v 'a)))
  (test "to a procedure that is no predicate" 'point (kind-of 'a))
  (set! point? saved-point?)
  (test "and back" '(point none) (list (kind-of a-point) (kind-of 'a))))

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
