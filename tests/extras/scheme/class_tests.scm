;; (scheme-js class-tests) library
;;
;; Tests for define-class and method calling.

(import (scheme base)
        (scheme-js interop))

(test-group "define-class-tests"

;; 1. Basic Class
(define-class <Point>
  (Point x y)
  point?
  (fields (x point-x point-x-set!)
          (y point-y point-y-set!))
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

(define-class <ColorPoint> <Point>
  (ColorPoint x y color)
  color-point?
  (fields (color color-point-color))
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
  (SelfAware)
  self-aware?
  (fields)
  (methods
    (get-self () this)))

(define sa (SelfAware))
(test "get-self" #t (eq? sa (sa.get-self)))

;; 4. Nested 'this'
(define-class <Nested>
  (Nested val)
  nested?
  (fields (val nested-val))
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
  (Greeter name)
  greeter?
  (fields (name greeter-name))
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

) ;; end test-group
