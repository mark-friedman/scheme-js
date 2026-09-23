;; /**
;;  * Point record type for testing.
;;  * @field x
;;  * @field y
;;  */
(define-record-type Point
  (make-point x y)
  point?
  (x point-x)
  (y point-y set-point-y!))

(test-group "Records"
  (test "Constructor" #t (point? (make-point 10 20)))
  
  (define p1 (make-point 10 20))
  
  (test "Predicate true" #t (point? p1))
  (test "Predicate false" #f (point? "not-a-point"))
  
  (test "Accessor x" 10 (point-x p1))
  (test "Accessor y" 20 (point-y p1))
  
  (set-point-y! p1 30)
  (test "Modifier" 30 (point-y p1))

  (test-group "Record Equality"
    (define p2 (make-point 1 2))
    (define p3 (make-point 1 2))
    (define p4 p2)

    (test "eq? same instance" #t (eq? p2 p4))
    (test "eq? different instances" #f (eq? p2 p3))
    
    (test "eqv? same instance" #t (eqv? p2 p4))
    (test "eqv? different instances" #f (eqv? p2 p3))
    
    (test "equal? same instance" #t (equal? p2 p4))
    (test "equal? different instances" #f (equal? p2 p3))

    ;; /**
    ;;  * Rectangle record type for testing nested equality.
    ;;  * @field p1 - Top-left point.
    ;;  * @field p2 - Bottom-right point.
    ;;  */
    (define-record-type Rect
      (make-rect p1 p2)
      rect?
      (p1 rect-p1)
      (p2 rect-p2))

    (define r1 (make-rect p2 p3))
    (define r2 (make-rect p2 p3))
    (define r3 r1)

    (test "Nested eq? same" #t (eq? r1 r3))
    (test "Nested eq? diff" #f (eq? r1 r2))
    (test "Nested equal? same" #t (equal? r1 r3))
    (test "Nested equal? diff" #f (equal? r1 r2))
  )
)

;; /**
;;  * One-field record for testing that a field keeps the exactness of the
;;  * value stored in it.
;;  * @field x
;;  */
(define-record-type Exactness-probe
  (make-probe x)
  probe?
  (x probe-x set-probe-x!))

(test-group "Record fields keep exactness"
  (test "integer-valued flonum stays inexact" #f (exact? (probe-x (make-probe 2.0))))
  (test "integer-valued flonum keeps its value" #t (eqv? 2.0 (probe-x (make-probe 2.0))))
  (test "negative zero survives" #t (eqv? -0.0 (probe-x (make-probe -0.0))))
  (test "exact integer stays exact" #t (exact? (probe-x (make-probe 2))))
  (test "non-integer flonum" #t (eqv? 2.5 (probe-x (make-probe 2.5))))
  (test "large flonum stays inexact" #f (exact? (probe-x (make-probe 1e300))))

  (define p (make-probe 1))
  (set-probe-x! p 3.0)
  (test "modifier stores a flonum" #f (exact? (probe-x p)))
  (set-probe-x! p 3)
  (test "modifier replaces a flonum with an exact integer" #t (exact? (probe-x p)))
  (test "replaced value" 3 (probe-x p))
  (set-probe-x! p 4.0)
  (test "modifier stores a flonum again" #t (eqv? 4.0 (probe-x p))))

;; /**
;;  * Record whose constructor names its fields in a different order from the
;;  * field specs, and leaves one field out.
;;  * @field a
;;  * @field b
;;  * @field c
;;  */
(define-record-type Reordered
  (make-reordered c a)
  reordered?
  (a reordered-a)
  (b reordered-b set-reordered-b!)
  (c reordered-c))

(test-group "Record constructor field tags"
  (define r (make-reordered 'first 'second))
  (test "first argument initialises the first tag" 'first (reordered-c r))
  (test "second argument initialises the second tag" 'second (reordered-a r))
  (test "unnamed field is not given an argument" #f
        (or (eq? (reordered-b r) 'first) (eq? (reordered-b r) 'second)))
  (set-reordered-b! r 'third)
  (test "unnamed field can be set" 'third (reordered-b r))

  (test-error "too few arguments" "wrong number of arguments" (make-reordered 'only))
  (test-error "too many arguments" "wrong number of arguments"
              (make-reordered 1 2 3))
  (test-error "in-order constructor checks arity" "wrong number of arguments"
              (make-point 1))
  (test-error "constructor tag must be a field" "not a field"
              (let ()
                (define-record-type Bad (make-bad z) bad? (x bad-x))
                make-bad))
  (test-error "constructor tag may not repeat" "more than once"
              (let ()
                (define-record-type Twice (make-twice x x) twice? (x twice-x))
                make-twice)))
