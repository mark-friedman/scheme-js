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
(test "p1.magnitude" (sqrt 25) (p1.magnitude))

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
(test "bind procedure from Scheme" 25.0 (add10 15))

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
(test "point-with-label magnitude" (sqrt 25) (pwl.magnitude))

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
(test "hardcoded point magnitude" (sqrt 100) (hp.magnitude))

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
(test "super method call" (+ 100 (sqrt 25)) (ep.magnitude))

) ;; end test-group

;; A JavaScript number carries no exactness, so a flonum that Scheme stores in
;; an object's property, or passes to a method, must still read back as a
;; flonum, while an integer JavaScript writes reads back exact.
(test-group "define-class values keep exactness"

  ;; /**
  ;;  * Class whose constructor and methods store and return values unchanged.
  ;;  * @field v
  ;;  */
  (define-class <Probe>
    Probe
    probe?
    (fields (v probe-v probe-v-set!))
    (constructor (v) (set! this.v v))
    (methods
      (get () this.v)
      (put! (new-v) (set! this.v new-v))
      (echo (x) x)
      (fill! (vec) (vector-set! vec 0 'filled))))

  (test "constructor body keeps a flonum" #t (eqv? 2.0 (probe-v (Probe 2.0))))
  (test "constructor body flonum through dot access" #t
        (let ((p (Probe 2.0))) (eqv? 2.0 p.v)))
  (test "constructor body keeps an exact integer" #t (eqv? 2 (probe-v (Probe 2))))
  (test "constructor body keeps negative zero" #t (eqv? -0.0 (probe-v (Probe -0.0))))

  (define p (Probe 1))
  (p.put! 3.0)
  (test "method assignment keeps a flonum" #t (eqv? 3.0 (probe-v p)))
  (test "method reads a flonum back" #t (eqv? 3.0 (p.get)))
  (test "method returns a flonum argument" #t (eqv? 4.0 (p.echo 4.0)))
  (test "method returns an exact argument" #t (eqv? 4 (p.echo 4)))
  (test "method passes a bignum" #t (= (expt 2 70) (p.echo (expt 2 70))))

  (define vec (vector 1 2 3))
  (test "method receives the caller's vector" #t (eq? vec (p.echo vec)))
  (p.fill! vec)
  (test "method mutates the caller's vector" 'filled (vector-ref vec 0))
  (define lst (list 1 2))
  (test "method receives the caller's list" #t (eq? lst (p.echo lst)))

  (set! p.v 5.0)
  (test "property assignment keeps a flonum" #t (eqv? 5.0 (probe-v p)))
  (test "property assignment flonum through dot access" #t (eqv? 5.0 p.v))
  (probe-v-set! p 6.0)
  (test "modifier flonum through dot access" #t (eqv? 6.0 p.v))
  (set! p.v 7)
  (test "property assignment of an exact integer" #t (eqv? 7 (probe-v p)))

  ;; /**
  ;;  * Class with the default constructor, which sets its fields itself.
  ;;  * @field a
  ;;  * @field b
  ;;  */
  (define-class <Pair2>
    Pair2
    pair2?
    (fields (a pair2-a) (b pair2-b))
    (methods))

  (test "default constructor keeps a flonum" #t (eqv? 2.0 (pair2-a (Pair2 2.0 3))))
  (test "default constructor keeps an exact integer" #t (eqv? 3 (pair2-b (Pair2 2.0 3))))

  ;; /**
  ;;  * Subclass that passes one argument to its parent's constructor.
  ;;  * @field w
  ;;  */
  (define-class <SubProbe> <Probe>
    SubProbe
    sub-probe?
    (fields (w sub-probe-w))
    (constructor (v w)
      (super v)
      (set! this.w w))
    (methods
      (base () (super.get))
      (base-echo (x) (super.echo x))))

  (define s (SubProbe 2.0 3.0))
  (test "parent constructor keeps a flonum" #t (eqv? 2.0 (probe-v s)))
  (test "subclass constructor keeps a flonum" #t (eqv? 3.0 (sub-probe-w s)))
  (test "super method call keeps a flonum" #t (eqv? 2.0 (s.base)))
  (test "super method call passes a flonum" #t (eqv? 2.0 (s.base-echo 2.0)))
  (test "super method call passes an exact integer" #t (eqv? 2 (s.base-echo 2)))
  (test "super method call receives the caller's vector" #t
        (let ((v2 (vector 1))) (eq? v2 (s.base-echo v2))))

  ;; /**
  ;;  * Subclass with the default constructor, which passes its arguments to
  ;;  * its parent's constructor as well as setting its own field.
  ;;  * @field c
  ;;  */
  (define-class <SubPair2> <Pair2>
    SubPair2
    sub-pair2?
    (fields (c sub-pair2-c))
    (methods))

  (define sp (SubPair2 2.0))
  (test "default constructor hands a flonum to its parent" #t (eqv? 2.0 (pair2-a sp)))
  (test "default subclass constructor keeps a flonum" #t (eqv? 2.0 (sub-pair2-c sp)))

  ;; /**
  ;;  * Class whose constructor constructs another object.
  ;;  * @field inner
  ;;  */
  (define-class <Holder>
    Holder
    holder?
    (fields (inner holder-inner))
    (constructor (x) (set! this.inner (Probe x)))
    (methods))

  (test "construction inside a constructor keeps a flonum" #t
        (eqv? 2.0 (probe-v (holder-inner (Holder 2.0)))))

  ;; A JavaScript parent class whose constructor builds a Scheme object with
  ;; `new`: that construction is JavaScript's, even though it happens inside
  ;; a Scheme one, so the integer it passes reads back exact.
  (js-set! (js-eval "globalThis") "__schemeProbeClass" Probe)
  (define JsBase
    (js-eval "(class { constructor() { this.made = new globalThis.__schemeProbeClass(3); } })"))

  ;; /**
  ;;  * Subclass of a JavaScript class.
  ;;  * @field k
  ;;  */
  (define-class <OnJs> JsBase
    OnJs
    on-js?
    (fields (k on-js-k))
    (constructor (k)
      (super)
      (set! this.k k))
    (methods))

  (define on-js (OnJs 2.0))
  (test "subclass of a JavaScript class keeps a flonum" #t (eqv? 2.0 (on-js-k on-js)))
  (test "JavaScript parent's own construction reads exact" #t
        (eqv? 3 (probe-v (js-ref on-js "made"))))

  (define obj (js-eval "({n: 3})"))
  (test "JavaScript-written integer reads exact" #t (eqv? 3 obj.n))
  (set! obj.n 4.0)
  (test "Scheme-written flonum on a plain object" #t (eqv? 4.0 obj.n))
  (set! obj.n 5)
  (test "Scheme-written exact integer on a plain object" #t (eqv? 5 obj.n)))
