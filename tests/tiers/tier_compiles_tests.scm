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

(test-group "When the tier compiles a procedure"
  (test "a procedure called twice from the top level is compiled on its second call"
        *tier-attached* (compiled? cube))
  (test "a procedure that loops is compiled when it is bound"
        *tier-attached* (compiled? sum-of-squares))
  (test-expect-fail
   (and *tier-attached*
        "compiling it beneath compiled code is abandoned: the compiler's own `guard` captures a continuation, which unwinds out through `call` in src/compiler/lowering.js")
   (test "a procedure first called from compiled code is compiled on its second call"
         *tier-attached* (compiled? square)))
  (test "and the program's answer is the same in both runs" 285 (sum-of-squares 10))
  (test "after it, a continuation captured in compiled code escapes as it should"
        4 (first-even '(1 3 4 5 6)))
  (test "and a recursion 100,000 deep in compiled code finishes"
        100000 (depth 100000)))
