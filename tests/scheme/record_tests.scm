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
