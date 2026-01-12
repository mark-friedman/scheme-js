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
;; Bind using js-obj procedure for context object
(define context (js-obj 'name "Bob"))
(define bound-greet (g1.greet.bind context))
(test "bind method from Scheme" "Hi Dave, I'm Bob" (bound-greet "Dave"))

;; 6. JS Object procedure verification
(test "js-obj literal" "Bob" (js-ref (js-obj 'name "Bob") "name"))
(test "js-obj nested" 123 (js-ref (js-ref (js-obj 'a (js-obj 'b 123)) "a") "b"))
(test "js-obj with multiple keys" 2 (js-ref (js-obj 'x 1 'y 2) "y"))

) ;; end test-group
