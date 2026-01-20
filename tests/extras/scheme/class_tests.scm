;; (scheme-js class-tests) library
;;
;; Tests for define-class and method calling.

(import (scheme base)
        (scheme-js interop))

(test-group "define-class-tests"

;; 1. Basic Class (no constructor clause - uses defaults)
(define-class <Point>
  Point
  point?
  (fields (x point-x point-x-set!)
          (y point-y point-y-set!))
  (constructor (x y)
    (set! this.x x)
    (set! this.y y))
  (methods
    (magnitude () (sqrt (+ (* this.x this.x) (* this.y this.y))))
    (move! (dx dy)
      (point-x-set! this (+ this.x dx))
      (point-y-set! this (+ this.y dy)))))

(define p1 (Point 3 4))
(test "point?" #t (point? p1))
(test "point-x" 3 (point-x p1))
(test "point-y" 4 (point-y p1))
(test "p1.magnitude" 5 (p1.magnitude))

(p1.move! 1 2)
(test "point-x after move" 4 (point-x p1))
(test "point-y after move" 6 (point-y p1))

;; 2. Inheritance with custom constructor
(define-class <ColorPoint> <Point>
  ColorPoint
  color-point?
  (fields (color color-point-color))
  (constructor (x y color)
    (set! this.x x)
    (set! this.y y)
    (set! this.color color))
  (methods
    (describe () (list this.x this.y this.color))))

(define cp1 (ColorPoint 10 20 'red))
(test "color-point?" #t (color-point? cp1))
(test "color-point is point" #t (point? cp1))
(test "point-x inherited" 10 (point-x cp1))
(test "cp1.color" 'red cp1.color)
(test "cp1.describe" '(10 20 red) (cp1.describe))

;; 3. 'this' as expression
(define-class <SelfAware>
  SelfAware
  self-aware?
  (fields)
  (constructor ()
    #t) ;; empty constructor body
  (methods
    (get-self () this)))

(define sa (SelfAware))
(test "get-self" #t (eq? sa (sa.get-self)))

;; 4. Nested 'this'
(define-class <Nested>
  Nested
  nested?
  (fields (val nested-val))
  (constructor (val)
    (set! this.val val))
  (methods
    (get-closure ()
      (lambda () this.val))))

(define n (Nested 42))
(define cl (n.get-closure))
(test "nested closure 'this'" 42 (cl))

;; 5. bind from Scheme
(define (adder a b) (+ a b))
(define add10 (adder.bind #f 10))
(test "bind procedure from Scheme" 25 (add10 15))

(define-class <Greeter>
  Greeter
  greeter?
  (fields (name greeter-name))
  (constructor (name)
    (set! this.name name))
  (methods
    (greet (other) (string-append "Hi " other ", I'm " this.name))))

(define g1 (Greeter "Alice"))
;; Bind using #{...} syntax for context object
(define context #{(name "Bob")})
(define bound-greet (g1.greet.bind context))
(test "bind method from Scheme" "Hi Dave, I'm Bob" (bound-greet "Dave"))

;; 6. JS Object syntax verification - #{...} reader syntax
(test "#{...} literal" "Bob" (js-ref #{(name "Bob")} "name"))
(test "#{...} nested" 123 (js-ref (js-ref #{(a #{(b 123)})} "a") "b"))
(test "#{...} with multiple keys" 2 (js-ref #{(x 1) (y 2)} "y"))

;; 7. Key type tests
(test "#{...} symbol key" "val1" (js-ref #{(foo "val1")} "foo"))
(test "#{...} string key" "val2" (js-ref #{("bar" "val2")} "bar"))
(test "#{...} number key" "val3" (js-ref #{(42 "val3")} "42"))
(test "#{...} computed key" "val4" (js-ref #{((string-append "x" "y") "val4")} "xy"))

;; 8. Spread syntax
(define base #{(a 1) (b 2)})
(test "#{...} spread" 3 (js-ref #{(... base) (c 3)} "c"))
(test "#{...} spread override" 99 (js-ref #{(... base) (a 99)} "a"))

;; 9. Custom constructor with computed initialization
(define-class <Counter>
  Counter
  counter?
  (fields (count counter-count))
  (constructor (initial)
    (set! this.count (* initial 10)))  ;; Computed initialization
  (methods
    (get () this.count)))

(define c1 (Counter 5))
(test "custom constructor computed init" 50 (c1.get))

;; 10. Explicit super() call with custom args
(define-class <PointWithLabel> <Point>
  PointWithLabel
  point-with-label?
  (fields (label point-label))
  (constructor (x y label)
    (super x y)  ;; Explicit super call with only x, y
    (set! this.label label))
  (methods
    (describe () (string-append this.label ": " (number->string (point-x this)) "," (number->string (point-y this))))))

(define pwl (PointWithLabel 3 4 "Origin"))
(test "point-with-label x" 3 (point-x pwl))
(test "point-with-label y" 4 (point-y pwl))
(test "point-with-label label" "Origin" (point-label pwl))
(test "point-with-label describe" "Origin: 3,4" (pwl.describe))
(test "point-with-label magnitude" 5 (pwl.magnitude))

;; 11. Super call with different args than constructor
(define-class <HardcodedPoint> <Point>
  HardcodedPoint
  hardcoded-point?
  (fields)
  (constructor (scale)
    (super (* scale 3) (* scale 4)))  ;; Hardcoded pattern with computed args
  (methods))

(define hp (HardcodedPoint 2))
(test "hardcoded point x" 6 (point-x hp))
(test "hardcoded point y" 8 (point-y hp))
(test "hardcoded point magnitude" 10 (hp.magnitude))

;; 12. Super method call with nice syntax
(define-class <ExtendedPoint> <Point>
  ExtendedPoint
  extended-point?
  (fields)
  (constructor (x y)
    (super x y))
  (methods
    (magnitude ()
      (+ 100 (super.magnitude)))))  ;; Nice syntax - analyzer transforms to class-super-call

(define ep (ExtendedPoint 3 4))
(test "super method call" 105 (ep.magnitude))

) ;; end test-group
