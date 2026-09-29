;; The compiler's driver and the tier's policies (src/compiler/driver.scm,
;; safety.scm, tier.scm)
;;
;; Runs in the compiler's own environment. Compiling real procedures, and the
;; tier compiling a program's as it runs, are checked end to end by the
;; functional tests (tests/functional/compiler_tests.js, tiering_tests.js,
;; capture_policy_tests.js); these check the decisions on their own, where a
;; wrong answer would only show up as a slower program.

;; The body of a procedure definition, analyzed: what the tier decides from.
(define (body-of definition) (ast-4 (analyze-lambda definition)))

(test-group "driver - what compiling a form is worth"
  (test "a procedure of straight-line code neither loops nor makes procedures"
        #f (makes-procedures-or-loops? (body-of '(define (square x) (* x x)))))
  (test "one that returns a procedure makes one"
        #t (makes-procedures-or-loops? (body-of '(define (adder n) (lambda (x) (+ x n))))))
  (test "one with a named let loops"
        #t (makes-procedures-or-loops?
             (body-of '(define (count n) (let loop ((i 0)) (if (= i n) i (loop (+ i 1))))))))
  (test "one with a do loops"
        #t (makes-procedures-or-loops? (body-of '(define (count n) (do ((i 0 (+ i 1))) ((= i n) i))))))
  (test "a procedure handed to another counts"
        #t (makes-procedures-or-loops? (body-of '(define (bump l) (map (lambda (x) (+ x 1)) l)))))
  (test "a lambda deep inside a conditional counts"
        #t (makes-procedures-or-loops? (body-of '(define (f x) (if x (begin 1 (lambda () x)) 2)))))
  (test "a quoted list is data, not code"
        #f (makes-procedures-or-loops? (body-of '(define (f) '(lambda (x) x)))))
  (test "a top-level loop contains a loop"
        #t (contains-loop? (analyze-form '(let loop ((i 0)) (if (< i 10) (loop (+ i 1)) i)))))
  (test "a top-level expression that only makes a procedure does not"
        #f (contains-loop? (analyze-form '(list (lambda () 1)))))
  (test "a loop inside a procedure it makes does"
        #t (contains-loop? (analyze-form '(list (lambda (n) (do ((i 0 (+ i 1))) ((= i n))))))))
  (test "straight-line code does not"
        #f (contains-loop? (analyze-form '(display (+ 1 2))))))

(test-group "driver - what defines at top level"
  (test "a definition does" #t (defines-at-top-level? (analyze-form '(define x 1))))
  (test "a begin holding one does" #t (defines-at-top-level? (analyze-form '(begin (display 1) (define x 1)))))
  (test "an expression does not" #f (defines-at-top-level? (analyze-form '(+ 1 2))))
  (test "a definition inside a let is internal, and does not"
        #f (defines-at-top-level? (analyze-form '(let () (define x 1) x)))))

(test-group "driver - a top-level expression as a procedure"
  (let ((thunk (expression-thunk (analyze-form '(+ 1 2)))))
    (test "is a lambda" 'lambda (car thunk))
    (test "of no parameters" '() (cadr thunk))
    (test "and no rest parameter" #f (caddr thunk))
    (test "named top-level" "top-level" (cadddr thunk))))

(test-group "driver - why a lowered procedure is declined"
  (test "a plain procedure is not" #f (lowering-decline (lower-lambda (analyze-lambda '(define (f x) (* x x)))) #f))
  (test "one naming a control global is, saying which"
        "references control global 'dynamic-wind'"
        (lowering-decline (lower-lambda (analyze-lambda '(define (f g) (dynamic-wind g g g)))) #f))
  (let ((captures (lower-lambda (analyze-lambda '(define (f) (call/cc (lambda (k) (k 1))))))))
    (test "one that captures is compiled by default" #f (lowering-decline captures #f))
    (test "and declined when captures are"
          "captures a continuation, and captures are declined" (lowering-decline captures #t))))

(test-group "driver - generated code too large to keep"
  (test "a source within the limit is kept" #f (source-too-large (make-string 10 #\a)))
  (test "one over it is declined, saying why"
        #t (string? (source-too-large (make-string (+ max-source 1) #\a)))))

;; Facts as the lowering reports them, written by hand: globals, whether the
;; procedure calls something it was handed, a control global it names, and
;; whether it captures.
(define (plain . globals) (make-facts globals #f #f #f))
(define none (lambda (name) #f))

(test-group "safety - declining what a capture could unwind through"
  (test "a procedure naming a control global is declined"
        '((f . "references control global 'dynamic-wind'"))
        (unsafe-from-facts (list (cons 'f (make-facts '(dynamic-wind) #f 'dynamic-wind #f))) none #f))
  (test "so is one that captures"
        '(f) (map car (unsafe-from-facts (list (cons 'f (make-facts '() #f #f #t))) none #f)))
  (test "a caller of a declined procedure in the unit is declined, with the path"
        "reaches g, which captures a continuation, which costs more compiled than interpreted"
        (cdr (assq 'f (unsafe-from-facts (list (cons 'f (plain 'g))
                                               (cons 'g (make-facts '() #f #f #t)))
                                         none #f))))
  (test "and so is its caller's caller"
        '(e f g)
        (map car (unsafe-from-facts (list (cons 'e (plain 'f)) (cons 'f (plain 'g))
                                          (cons 'g (make-facts '() #f #f #t)))
                                    none #f)))
  (test "a procedure reaching none is compiled" '() (unsafe-from-facts (list (cons 'f (plain 'car))) none #f))
  (test "calling something handed to it is declined only when strict"
        '(() (f))
        (list (map car (unsafe-from-facts (list (cons 'f (make-facts '() #t #f #f))) none #f))
              (map car (unsafe-from-facts (list (cons 'f (make-facts '() #t #f #f))) none #t))))
  (let ((library (lambda (name)
                   (case name
                     ((helper) (plain 'escape))
                     ((escape) (make-facts '() #f #f #t))
                     ((loop-a) (plain 'loop-b))
                     ((loop-b) (plain 'loop-a))
                     (else #f)))))
    (test "a procedure outside the unit is looked inside, through its callees"
          "reaches helper -> escape captures a continuation"
          (cdr (assq 'f (unsafe-from-facts (list (cons 'f (plain 'helper))) library #f))))
    (test "a cycle outside the unit is not itself a reason"
          '() (unsafe-from-facts (list (cons 'f (plain 'loop-a))) library #f))))

(test-group "tier - procedures whose frames are re-entered"
  (test "too few resumes to judge" #f (re-entered? 1 (- reentry-minimum 1)))
  (test "enough, at many resumes a save" #t (re-entered? 1 reentry-minimum))
  (test "enough, but about one resume a save, is an escape" #f (re-entered? reentry-minimum reentry-minimum))
  (test "the ratio is the boundary" #t (re-entered? (/ reentry-minimum reentry-ratio) reentry-minimum))
  (test "just under it is not" #f (re-entered? (+ (/ reentry-minimum reentry-ratio) 1) reentry-minimum))
  (test "resumes counted by JavaScript arrive as flonums" #t (re-entered? 1. (inexact reentry-minimum)))
  (test "first asked at the minimum" reentry-minimum (first-resume-to-ask))
  (test "not asked again before the minimum" reentry-minimum (next-resume-to-ask 1 5))
  (test "nor before resumes reach the ratio of the saves so far" (* reentry-ratio 300) (next-resume-to-ask 300 1100))
  (test "and otherwise at the next resume" 2001 (next-resume-to-ask 1 2000))
  (test "counted by JavaScript, the answer is a number JavaScript can compare"
        #t (real? (next-resume-to-ask 1. 5.))))
