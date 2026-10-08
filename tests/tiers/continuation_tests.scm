;; continuation_tests.scm -- continuations taken in compiled code, in either tier.
;;
;; A capture made by compiled code is finished where it can be by the driver
;; the interpreter calls compiled code through (`runSegment` in
;; src/core/interpreter/unwind.js): its saved frames kept as a list of their
;; own, the continuation called from compiled code running in that driver by a
;; jump the driver catches. Anywhere else -- from interpreted code, from inside
;; a run of the interpreter, after the driver has returned, across a
;; `dynamic-wind` -- the continuation is the interpreter's, built from the
;; frames when first needed. These check that every one of those ways gives
;; the interpreter's answers: escapes, re-entries within and after the
;; procedure that captured, several re-entries of one continuation, captures
;; beneath frames moved to the heap, and continuations crossing winds and
;; JavaScript.
;;
;; The file runs twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js). A procedure that loops or makes a
;; procedure is compiled when it is bound; any other is called twice before it
;; is tested, since the tier compiles a procedure on its second call. One that
;; names `dynamic-wind` is left interpreted.

;; /**
;;  * Whether a procedure is compiled.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {boolean}
;;  */
(define (compiled? procedure)
  (eq? #t (js-ref procedure "$compiled")))

;; ---------------------------------------------------------------------------
;; Escapes
;; ---------------------------------------------------------------------------

;; /**
;;  * The first element of a list that satisfies a predicate, found by escaping
;;  * from `for-each`.
;;  * @param {procedure} pred - The predicate.
;;  * @param {list} items - The list.
;;  * @returns {*} The element, or #f.
;;  */
(define (find-first pred items)
  (call/cc
   (lambda (return)
     (for-each (lambda (x) (if (pred x) (return x))) items)
     #f)))

;; /**
;;  * The sum of the even numbers below n, each found by an escape from a loop.
;;  * @param {integer} n - How many.
;;  * @returns {integer}
;;  */
(define (escapes n)
  (let loop ((i 0) (sum 0))
    (if (= i n)
        sum
        (loop (+ i 1) (+ sum (or (find-first even? (list 1 3 i 5)) 0))))))

;; Tak, every call through a continuation, from the canonical suite's ctak.
(define (ctak x y z)
  (call/cc (lambda (k) (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (call/cc
       (lambda (k)
         (ctak-aux k
                   (call/cc (lambda (k) (ctak-aux k (- x 1) y z)))
                   (call/cc (lambda (k) (ctak-aux k (- y 1) z x)))
                   (call/cc (lambda (k) (ctak-aux k (- z 1) x y))))))))

(define (tak x y z)
  (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))

;; ---------------------------------------------------------------------------
;; Re-entry
;; ---------------------------------------------------------------------------

(define saved-k #f)

;; /**
;;  * x, by way of a continuation saved for re-entry: re-entered with v, it
;;  * returns x + v again.
;;  * @param {number} x - The number.
;;  * @returns {number}
;;  */
(define (capture-and-return x)
  (+ x (call/cc (lambda (k) (set! saved-k k) 0))))

;; /**
;;  * Re-enters `capture-and-return`'s continuation, after it returned, twice
;;  * more, collecting what it returns each time.
;;  * @returns {list}
;;  */
(define (re-enter-test)
  (let ((count 0) (results '()))
    (let ((r (capture-and-return 10)))
      (set! results (cons r results))
      (set! count (+ count 1))
      (if (< count 3) (saved-k count) (reverse results)))))

;; /**
;;  * Counts to 3 by re-entering one continuation, taken inside the loop.
;;  * @returns {list}
;;  */
(define (count-by-reentry)
  (let ((acc '()) (again #f))
    (let ((v (call/cc (lambda (k) (set! again k) 0))))
      (set! acc (cons v acc))
      (if (< v 3) (again (+ v 1)) (reverse acc)))))

(define later-k #f)

;; /**
;;  * 1, saving its continuation for re-entry after it has returned.
;;  * @returns {integer}
;;  */
(define (capture-later)
  (call/cc (lambda (k) (set! later-k k) 1)))

(define wind-log '())

;; /**
;;  * Re-enters `capture-later` from interpreted code -- this procedure names
;;  * `dynamic-wind`, so stays interpreted -- after it has returned, within one
;;  * wind, logging the wind's entries and exits.
;;  * @returns {list} What the re-entries returned, and the wind's log.
;;  */
(define (reenter-from-interpreted)
  (set! wind-log '())
  (let ((results '()))
    (dynamic-wind
     (lambda () (set! wind-log (cons 'in wind-log)))
     (lambda ()
       (let ((v (capture-later)))
         (set! results (cons v results))
         (if (< v 3) (later-k (+ v 1)) #f)))
     (lambda () (set! wind-log (cons 'out wind-log))))
    (list (reverse results) (reverse wind-log))))

;; ---------------------------------------------------------------------------
;; Across winds and JavaScript
;; ---------------------------------------------------------------------------

;; /**
;;  * Calls a continuation, from compiled code.
;;  * @param {procedure} k - The continuation.
;;  * @param {*} v - The value.
;;  * @returns {*} Does not return.
;;  */
(define (call-k k v) (k v))

;; /**
;;  * Runs a thunk in a wind that logs into a box; interpreted, since it names
;;  * `dynamic-wind`.
;;  * @param {procedure} thunk - The thunk.
;;  * @param {pair} box - Where the log is kept, in its car.
;;  * @returns {*} What the thunk returns.
;;  */
(define (wind-around thunk box)
  (dynamic-wind (lambda () (set-car! box (cons 'before (car box))))
                thunk
                (lambda () (set-car! box (cons 'after (car box))))))

;; /**
;;  * Escapes, from compiled code inside a wind, to a continuation compiled code
;;  * took outside it: the wind's after-thunk runs on the way out.
;;  * @returns {list} The value escaped with, and the wind's log.
;;  */
(define (escape-through-wind)
  (let ((box (list '())))
    (let ((r (call/cc (lambda (k) (wind-around (lambda () (call-k k 'escaped)) box)))))
      (list r (reverse (car box))))))

;; /**
;;  * Escapes from a JavaScript callback -- `forEach` -- to a continuation taken
;;  * in compiled code around it.
;;  * @returns {*} The first element over 2.
;;  */
(define (escape-from-javascript)
  (call/cc
   (lambda (return)
     (js-invoke (js-eval "[1, 2, 3, 4]") "forEach" (lambda (x . rest) (if (> x 2) (return x))))
     #f)))

;; ---------------------------------------------------------------------------
;; Deep recursion
;; ---------------------------------------------------------------------------

(define (deep n) (if (= n 0) 0 (+ 1 (deep (- n 1)))))

;; /**
;;  * n, counted by a recursion n deep that escapes at its bottom.
;;  * @param {integer} n - How deep.
;;  * @returns {integer}
;;  */
(define (deep-capture n)
  (if (= n 0) (call/cc (lambda (k) (k 0))) (+ 1 (deep-capture (- n 1)))))

(define deep-k #f)

(define (deep-save n)
  (if (= n 0) (call/cc (lambda (k) (set! deep-k k) 0)) (+ 1 (deep-save (- n 1)))))

;; /**
;;  * Re-enters, twice, a continuation taken at the bottom of a recursion deep
;;  * enough that its frames had moved to the heap.
;;  * @returns {list} What the last re-entry returned, and how many runs.
;;  */
(define (deep-reenter)
  (let ((runs 0))
    (let ((r (deep-save 20000)))
      (set! runs (+ runs 1))
      (if (< runs 3) (deep-k runs) (list r runs)))))

;; ---------------------------------------------------------------------------
;; Several values
;; ---------------------------------------------------------------------------

(define (two-values) (call/cc (lambda (k) (k 1 2))))

(define (warm thunk) (thunk) (thunk))
(warm (lambda () (capture-and-return 1)))
(warm (lambda () (capture-later)))
(warm (lambda () (call-k (lambda (x) x) 1)))
(warm (lambda () (tak 3 2 1)))
(warm (lambda () (deep 3)))
(warm (lambda () (deep-capture 3)))
(warm (lambda () (deep-save 3)))
(warm (lambda () (ctak 3 2 1)))
(warm (lambda () (ctak-aux (lambda (x) x) 1 2 3)))
(warm (lambda () (two-values)))

(test-group "Continuations taken in compiled code"
  (test "the procedures that capture are compiled under the tier"
        (list *tier-attached* *tier-attached* *tier-attached* *tier-attached*)
        (map compiled? (list find-first ctak-aux capture-and-return deep-save)))
  (test "an escape from for-each" 4 (find-first even? '(1 3 4 5)))
  (test "and none" #f (find-first even? '(1 3 5)))
  (test "a thousand escapes, each from a loop" 249500 (escapes 1000))
  (test "a continuation taken at every call gives tak's answer" (tak 12 8 4) (ctak 12 8 4))
  (test "re-entered twice after the procedure that took it returned"
        '(10 11 12) (re-enter-test))
  (test "one continuation re-entered three times within its procedure"
        '(0 1 2 3) (count-by-reentry))
  (test "re-entered from interpreted code, within one wind, which is entered and left once"
        '((1 2 3) (in out)) (reenter-from-interpreted))
  (test "an escape through a wind runs its after-thunk"
        '(escaped (before after)) (escape-through-wind))
  (test "an escape from a JavaScript callback" 3 (escape-from-javascript))
  (test "a recursion 100,000 deep" 100000 (deep 100000))
  (test "an escape at the bottom of a recursion 50,000 deep" 50000 (deep-capture 50000))
  (test "a continuation taken 20,000 deep, re-entered twice" '(20002 3) (deep-reenter))
  (test "a continuation given two values"
        '(1 2) (call-with-values two-values list)))
