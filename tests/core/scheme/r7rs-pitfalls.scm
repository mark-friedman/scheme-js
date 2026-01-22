;; r7rs-pitfalls.scm
;;
;; R7RS compliance tests based on the classic R5RS pitfalls collection.
;; Code was collected from public forums (comp.lang.scheme) and is in the public domain.
;; Converted to use the project test harness.
;;
;; Original authors: Al Petrofsky, Alan Bawden, Scott Miller, Eli Barzilay,
;; Brian M. Moore, Matthias Radestock, and others.

(define call/cc call-with-current-continuation)

;; =============================================================================
;; Section 1: Proper letrec implementation
;; =============================================================================

(test-group "letrec-implementation"

  ;; Credits to Al Petrofsky
  ;; In thread: defines in letrec body
  ;; http://groups.google.com/groups?selm=87bsoq0wfk.fsf%40app.dial.idiom.com
  (test "1.1: letrec with call/cc (all inits before assigns)" 0
    (let ((cont #f))
      (letrec ((x (call-with-current-continuation (lambda (c) (set! cont c) 0)))
               (y (call-with-current-continuation (lambda (c) (set! cont c) 0))))
        (if cont
            (let ((c cont))
              (set! cont #f)
              (set! x 1)
              (set! y 1)
              (c 0))
            (+ x y)))))

  ;; Credits to Al Petrofsky
  ;; In thread: Widespread bug in letrec when an initializer returns twice
  ;; http://groups.google.com/groups?selm=87d793aacz.fsf_-_%40app.dial.idiom.com
  (test "1.2: letrec initializer returns twice" #t
    (letrec ((x (call/cc list)) (y (call/cc list)))
      (cond ((procedure? x) (x (pair? y)))
            ((procedure? y) (y (pair? x))))
      (let ((x (car x)) (y (car y)))
        (and (call/cc x) (call/cc y) (call/cc x)))))

  ;; Credits to Alan Bawden
  ;; In thread: LETREC + CALL/CC = SET! even in a limited setting
  ;; http://groups.google.com/groups?selm=19890302162742.4.ALAN%40PIGPEN.AI.MIT.EDU
  (test "1.3: letrec with call/cc and list" #t
    (letrec ((x (call-with-current-continuation
                  (lambda (c)
                    (list #t c)))))
      (if (car x)
          ((cadr x) (list #f (lambda () x)))
          (eq? x ((cadr x)))))))

;; =============================================================================
;; Section 2: Proper call/cc and procedure application
;; =============================================================================

(test-group "call/cc-application"

  ;; Credits to Al Petrofsky (and a wink to Matthias Blume)
  ;; In thread: Widespread bug in handling (call/cc (lambda (c) (0 (c 1)))) => 1
  ;; http://groups.google.com/groups?selm=87g00y4b6l.fsf%40radish.petrofsky.org
  (test "2.1: call/cc in non-tail position of application" 1
    (call/cc (lambda (c) (0 (c 1))))))

;; =============================================================================
;; Section 3: Hygienic macros
;; =============================================================================

(test-group "hygienic-macros"

  ;; Eli Barzilay
  ;; In thread: R5RS macros...
  ;; http://groups.google.com/groups?selm=skitsdqjq3.fsf%40tulare.cs.cornell.edu
  (test "3.1: macro hygiene with shadowed +" 4
    (let-syntax ((foo
                   (syntax-rules ()
                     ((_ expr) (+ expr 1)))))
      (let ((+ *))
        (foo 3))))

  ;; Al Petrofsky
  ;; In thread: Buggy use of begin in r5rs cond and case macros.
  ;; http://groups.google.com/groups?selm=87bse3bznr.fsf%40radish.petrofsky.org
  (test "3.2: define in cond else clause" 2
    (let-syntax ((foo (syntax-rules ()
                        ((_ var) (define var 1)))))
      (let ((x 2))
        (begin (define foo +))
        (cond (else (foo x)))
        x)))

  ;; Al Petrofsky
  ;; In thread: An Advanced syntax-rules Primer for the Mildly Insane
  ;; http://groups.google.com/groups?selm=87it8db0um.fsf@radish.petrofsky.org
  (test "3.3: nested let-syntax hygiene" 1
    (let ((x 1))
      (let-syntax
          ((foo (syntax-rules ()
                  ((_ y) (let-syntax
                             ((bar (syntax-rules ()
                                     ((_) (let ((x 2)) y)))))
                           (bar))))))
        (foo x))))

  ;; Al Petrofsky - Contributed directly
  (test "3.4: empty syntax-rules" 1
    (let-syntax ((x (syntax-rules ()))) 1)))

;; =============================================================================
;; Section 4: No identifiers are reserved
;; =============================================================================

(test-group "no-reserved-identifiers"

  ;; Brian M. Moore
  ;; In thread: shadowing syntactic keywords, bug in MIT Scheme?
  ;; http://groups.google.com/groups?selm=6e6n88%248qf%241%40news.cc.ukans.edu
  (test "4.1: lambda as parameter name" '(x)
    ((lambda lambda lambda) 'x))

  (test "4.2: begin as parameter name" '(1 2 3)
    ((lambda (begin) (begin 1 2 3)) (lambda lambda lambda)))

  ;; Skipped: shadowing 'quote' requires quote to be a regular binding, not reader syntax
  (test-skip "quote is reader syntax, not a binding"
    (test "4.3: quote shadowed" #f
      (let ((quote -)) (eqv? '1 1)))))

;; =============================================================================
;; Section 5: #f/() distinctness
;; =============================================================================

(test-group "false-null-distinctness"

  ;; Scott Miller
  (test "5.1: eq? #f '()" #f
    (eq? #f '()))
  (test "5.2: eqv? #f '()" #f
    (eqv? #f '()))
  (test "5.3: equal? #f '()" #f
    (equal? #f '())))

;; =============================================================================
;; Section 6: string->symbol case sensitivity
;; =============================================================================

(test-group "symbol-case-sensitivity"

  ;; Jens Axel Søgaard
  ;; In thread: Symbols in DrScheme - bug?
  ;; http://groups.google.com/groups?selm=3be55b4f%240%24358%24edfadb0f%40dspool01.news.tele.dk
  (test "6.1: string->symbol case sensitivity" #f
    (eq? (string->symbol "f") (string->symbol "F"))))

;; =============================================================================
;; Section 7: First class continuations
;; =============================================================================

(test-group "first-class-continuations"

  ;; Scott Miller
  ;; Continuations should be unmodified by invocation of other continuations.
  (define r #f)
  (define a #f)
  (define b #f)
  (define c #f)
  (define i 0)
  (test "7.1: continuation independence (forward)" 28
    (let ()
      (set! r (+ 1 (+ 2 (+ 3 (call/cc (lambda (k) (set! a k) 4))))
                (+ 5 (+ 6 (call/cc (lambda (k) (set! b k) 7))))))
      (if (not c)
          (set! c a))
      (set! i (+ i 1))
      (case i
        ((1) (a 5))
        ((2) (b 8))
        ((3) (a 6))
        ((4) (c 4)))
      r))

  ;; Reset for next test
  (set! r #f)
  (set! a #f)
  (set! b #f)
  (set! c #f)
  (set! i 0)
  
  ;; Same test, but in reverse order
  (test "7.2: continuation independence (reverse)" 28
    (let ()
      (set! r (+ 1 (+ 2 (+ 3 (call/cc (lambda (k) (set! a k) 4))))
                (+ 5 (+ 6 (call/cc (lambda (k) (set! b k) 7))))))
      (if (not c)
          (set! c a))
      (set! i (+ i 1))
      (case i
        ((1) (b 8))
        ((2) (a 5))
        ((3) (b 7))
        ((4) (c 4)))
      r))

  ;; Credits to Matthias Radestock
  ;; Test for SISC's lazy CallFrame routines.
  (test "7.3: continuation with multiple captures" 
      '((-1 4 5 3)
        (4 -1 5 3)
        (-1 5 4 3)
        (5 -1 4 3)
        (4 5 -1 3)
        (5 4 -1 3))
    (let ((k1 #f)
          (k2 #f)
          (k3 #f)
          (state 0))
      (define (identity x) x)
      (define (fn)
        ((identity (if (= state 0)
                       (call/cc (lambda (k) (set! k1 k) +))
                       +))
         (identity (if (= state 0)
                       (call/cc (lambda (k) (set! k2 k) 1))
                       1))
         (identity (if (= state 0)
                       (call/cc (lambda (k) (set! k3 k) 2))
                       2))))
      (define (check states)
        (set! state 0)
        (let* ((res '())
               (r (fn)))
          (set! res (cons r res))
          (if (null? states)
              res
              (begin (set! state (car states))
                     (set! states (cdr states))
                     (case state
                       ((1) (k3 4))
                       ((2) (k2 2))
                       ((3) (k1 -)))))))
      (map check '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))))

  ;; Modification of the yin-yang puzzle (Scott G. Miller)
  (test "7.4: yin-yang puzzle" '(10 9 8 7 6 5 4 3 2 1 0)
    (let ((x '())
          (y 0))
      (call/cc
        (lambda (escape)
          (let* ((yin ((lambda (foo)
                         (set! x (cons y x))
                         (if (= y 10)
                             (escape x)
                             (begin
                               (set! y 0)
                               foo)))
                       (call/cc (lambda (bar) bar))))
                 (yang ((lambda (foo)
                          (set! y (+ y 1))
                          foo)
                        (call/cc (lambda (baz) baz)))))
            (yin yang)))))))

;; =============================================================================
;; Section 8: Miscellaneous
;; =============================================================================

(test-group "miscellaneous"

  ;; Al Petrofsky
  ;; In thread: R5RS Implementors Pitfalls
  ;; http://groups.google.com/groups?selm=871zemtmd4.fsf@app.dial.idiom.com
  (test "8.1: named let with - as name" -1
    (let - ((n (- 1))) n))

  (test "8.2: append shares structure" '(1 2 3 4 1 2 3 4 5)
    (let ((ls (list 1 2 3 4)))
      (append ls ls '(5))))

  ;; This illustrates a bug in R5RS. The general agreement is that 1 should
  ;; be returned (R7RS clarified this behavior).
  (test "8.3: let-syntax scope in definition context" 1
    (let ((x 1))
      (let-syntax ((foo (syntax-rules () ((_) 2))))
        (define x (foo))
        3)
      x)))

;; Report test results
(test-report)
