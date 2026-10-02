;; tier_compiles_tests.scm -- when the tier compiles a program's procedures.
;;
;; A top-level procedure that loops or makes procedures is compiled when it is
;; bound, and any other on its second call (src/compiler/tier.scm). These check
;; that the rule holds whatever makes the call -- in particular compiled code,
;; which reaches an interpreted procedure through a run of the interpreter
;; nested beneath it -- and that the program computes the same answers in both
;; runs of this file, interpreted and with the tier attached.

;; /**
;;  * Whether a procedure is compiled.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {boolean}
;;  */
(define (compiled? procedure)
  (eq? #t (js-ref procedure "$compiled")))

(define (cube x) (* x x x))
(cube 2)
(cube 2)

(define (square x) (* x x))

;; Loops, so it is compiled when it is bound; it is `square`'s only caller, so
;; both of `square`'s first two calls come from compiled code.
(define (sum-of-squares n)
  (let loop ((i 0) (sum 0))
    (if (= i n) sum (loop (+ i 1) (+ sum (square i))))))

(sum-of-squares 10)

;; A loop the interpreter made, held in a list so that nothing binds it yet, is
;; assigned to a top-level name by `install-counter!`, which neither loops nor
;; makes procedures and is called once, from a compiled loop, so runs
;; interpreted: the loop is bound, and compiled if the rule holds, beneath
;; compiled code.
(define counters (list (lambda (n) (let loop ((i 0)) (if (= i n) i (loop (+ i 1)))))))

(define count-to #f)

(define (install-counter! counter) (set! count-to counter))

(define (install-once)
  (let loop ((i 0))
    (when (< i 1)
      (install-counter! (car counters))
      (loop (+ i 1)))))

(install-once)

;; Makes a procedure, so it is compiled when it is bound, and captures a
;; continuation to escape with.
(define (first-even numbers)
  (call-with-current-continuation
   (lambda (return)
     (for-each (lambda (n) (if (even? n) (return n))) numbers)
     #f)))

(define (depth n) (if (= n 0) 0 (+ 1 (depth (- n 1)))))
(depth 2)
(depth 2)

;; Loops, so it is compiled when it is bound, and walks a circular literal,
;; which R7RS 2.4 allows in a program: the compiler takes it as a constant
;; like any other, rather than following it.
(define (nth-of-cycle n)
  (let loop ((i 0) (xs '#0=(a b . #0#)))
    (if (= i n) (car xs) (loop (+ i 1) (cdr xs)))))

(test-group "When the tier compiles a procedure"
  (test "a procedure called twice from the top level is compiled on its second call"
        *tier-attached* (compiled? cube))
  (test "a procedure that loops is compiled when it is bound"
        *tier-attached* (compiled? sum-of-squares))
  (test "a procedure first called from compiled code is compiled on its second call"
        *tier-attached* (compiled? square))
  (test "a loop assigned to a top-level name beneath compiled code is compiled when it is bound"
        *tier-attached* (compiled? count-to))
  (test "and computes the same answer" 7 (count-to 7))
  (test "and the program's answer is the same in both runs" 285 (sum-of-squares 10))
  (test "after it, a continuation captured in compiled code escapes as it should"
        4 (first-even '(1 3 4 5 6)))
  (test "and a recursion 100,000 deep in compiled code finishes"
        100000 (depth 100000))
  (test "a procedure holding a circular literal is compiled"
        *tier-attached* (compiled? nth-of-cycle))
  (test "and finds in it what was written" 'b (nth-of-cycle 5)))
