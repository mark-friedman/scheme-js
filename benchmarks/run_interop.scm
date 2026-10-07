;; run_interop.scm -- what crossing between Scheme and JavaScript costs.
;;
;; Calling JavaScript from Scheme, and being called by it, is the constraint
;; that sets this implementation apart (ROADMAP.md, constraint 1): a Scheme
;; procedure is a JavaScript function, which converts its arguments and result
;; and finishes its tail calls before returning, whichever tier runs it, and
;; JavaScript's functions, objects, properties and classes are reachable from
;; Scheme. The canonical suite measures none of it, since its programs are
;; portable Scheme. This does, crossing by crossing.
;;
;; Each crossing is timed in a loop, as nanoseconds a crossing, best of several
;; runs, with the same loop around the cheapest call on its side subtracted:
;;
;;   - JavaScript calling Scheme: a JavaScript loop calls a Scheme procedure,
;;     against the loop calling a JavaScript function that returns 1;
;;   - Scheme calling JavaScript: a Scheme loop makes the crossing, against
;;     the loop adding 1 with no call, as the plain JavaScript is against its
;;     loop adding 1.
;;
;; Each is measured with the Scheme interpreted and compiled -- the same
;; procedure, compiled by the compiler as the tier would (`compile-closure`) --
;; and beside the same work done in plain JavaScript, for scale. The two
;; tiers' totals are compared, so a figure cannot come from code that computes
;; something else.
;;
;;     node repl.js --no-compile benchmarks/run_interop.scm [--runs N]

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (srfi 1)
        (srfi 152)
        (scheme-js interop)
        (scheme-js compiler))

;; /**
;;  * How many times each loop is timed; the best is kept.
;;  */
(define runs
  (let ((given (member "--runs" (command-line))))
    (or (and given (pair? (cdr given)) (string->number (cadr given))) 5)))

;; /**
;;  * Crossings a timed loop makes in each tier: the interpreter is tens of
;;  * times slower, so it makes fewer, for about the same time.
;;  */
(define compiled-crossings 200000)
(define interpreted-crossings 20000)

;; /**
;;  * Milliseconds, as precisely as the host keeps them.
;;  * @returns {number}
;;  */
(define (now)
  (js-invoke (js-eval "performance") "now"))

;; ---------------------------------------------------------------------------
;; What the crossings cross with
;; ---------------------------------------------------------------------------

(define js-one (js-eval "(x) => 1"))
(define js-ident (js-eval "(x) => x"))
(define js-append (js-eval "(s) => s + '!'"))
(define point (js-eval "({ x: 3, y: 4, inc(n) { return n + 1; } })"))
(define point-class (js-eval "(class Point { constructor(x, y) { this.x = x; this.y = y; } })"))
(define ten (vector 1 2 3 4 5 6 7 8 9 10))

;; /**
;;  * A JavaScript loop calling a function `n` times with an argument, adding
;;  * up what it returns: how JavaScript calls Scheme here.
;;  */
(define call-each
  (js-eval "(f, n, a) => { let s = 0; for (let i = 0; i < n; i++) s += f(a); return s; }"))

;; /**
;;  * A JavaScript loop mapping a function over an array `n` times.
;;  */
(define map-each
  (js-eval "(f, n, a) => { let s = 0; for (let i = 0; i < n; i++) s += a.map(f).length; return s; }"))

;; ---------------------------------------------------------------------------
;; The crossings
;; ---------------------------------------------------------------------------

;; /**
;;  * A crossing, timed in each tier and in plain JavaScript.
;;  * @property {string} label - What it is.
;;  * @property {procedure} scheme - Called with a count: makes the crossing that
;;  *   many times and answers a total. Interpreted here; compiled for the
;;  *   compiled tier.
;;  * @property {procedure} baseline - The same loop around the cheapest call.
;;  * @property {procedure} javascript - The same work in plain JavaScript,
;;  *   called with a count; answers the milliseconds it took.
;;  */
(define-record-type crossing
  (make-crossing label scheme baseline javascript)
  crossing?
  (label crossing-label)
  (scheme crossing-scheme)
  (baseline crossing-baseline)
  (javascript crossing-javascript))

;; /**
;;  * Plain JavaScript doing some work in a loop, against the same loop doing
;;  * the cheapest, timed inside JavaScript.
;;  * @param {string} setup - Statements run first, binding what the work uses.
;;  * @param {string} work - An expression, the work, its value added up.
;;  * @param {string} cheapest - An expression, the baseline's.
;;  * @returns {procedure} From a count to the milliseconds the work took over
;;  *   the baseline.
;;  */
(define (plain-javascript setup work cheapest)
  (js-eval (string-append
            "(n) => { " setup
            " const time = (f) => { let s = 0; const t = performance.now();"
            " for (let i = 0; i < n; i++) s += f(i); return [performance.now() - t, s]; };"
            " const work = (i) => " work "; const cheapest = (i) => " cheapest ";"
            " let w = Infinity, c = Infinity;"
            " for (let r = 0; r < 3; r++) { w = Math.min(w, time(work)[0]); c = Math.min(c, time(cheapest)[0]); }"
            " return w - c; }")))

;; JavaScript calling Scheme. Each loop is JavaScript's; what it calls is the
;; Scheme procedure under test, interpreted or compiled.

(define (javascript-calls label callee argument driver js-work)
  (make-crossing label
                 (lambda (callee) (lambda (n) (driver callee n argument)))
                 (lambda (n) (driver js-one n argument))
                 js-work))

(define (ident x) x)
(define (string-size s) (string-length s))
(define (vector-sum v) (let loop ((i 0) (s 0)) (if (= i 10) s (loop (+ i 1) (+ s (vector-ref v i))))))
(define (point-x o) (js-ref o "x"))
(define (add-one x) (+ x 1))

(define javascript-calling-scheme
  (list
   (javascript-calls "a procedure of a number, returning it" ident 1 call-each
                     (plain-javascript "const f = (x) => x;" "f(1)" "1"))
   (javascript-calls "a procedure of a string, returning its length" string-size "hello" call-each
                     (plain-javascript "const f = (s) => s.length;" "f('hello')" "1"))
   (javascript-calls "a procedure of an array of ten, adding them up" vector-sum ten call-each
                     (plain-javascript "const a = [1,2,3,4,5,6,7,8,9,10]; const f = (v) => { let s = 0; for (let i = 0; i < 10; i++) s += v[i]; return s; };"
                                       "f(a)" "1"))
   (javascript-calls "a procedure of an object, reading a property" point-x point call-each
                     (plain-javascript "const o = { x: 3 }; const f = (o) => o.x;" "f(o)" "1"))
   (javascript-calls "Array.prototype.map over ten, with a procedure" add-one ten map-each
                     (plain-javascript "const a = [1,2,3,4,5,6,7,8,9,10]; const f = (x) => x + 1;"
                                       "a.map(f).length" "1"))))

;; Scheme calling JavaScript. Each loop is Scheme's, and is what the tiers
;; run interpreted or compiled.

(define (baseline-loop n) (let loop ((i 0) (s 0)) (if (= i n) s (loop (+ i 1) (+ s 1)))))
(define (call-function n) (let loop ((i 0) (s 0)) (if (= i n) s (loop (+ i 1) (+ s (js-ident 1))))))
(define (call-method n) (let loop ((i 0) (s 0)) (if (= i n) s (loop (+ i 1) (+ s (js-invoke point "inc" 1))))))
(define (read-property n) (let loop ((i 0) (s 0)) (if (= i n) s (loop (+ i 1) (+ s (js-ref point "x"))))))
(define (write-property n)
  (let loop ((i 0) (s 0)) (if (= i n) s (loop (+ i 1) (+ s (begin (js-set! point "y" i) 1))))))
(define (make-object n)
  (let loop ((i 0) (s 0)) (if (= i n) s (loop (+ i 1) (+ s (begin (js-obj "x" i) 1))))))
(define (make-instance n)
  (let loop ((i 0) (s 0)) (if (= i n) s (loop (+ i 1) (+ s (begin (js-new point-class i i) 1))))))
(define (pass-string n)
  (let loop ((i 0) (s 0)) (if (= i n) s (loop (+ i 1) (+ s (string-length (js-append "abc")))))))

(define (scheme-calls label loop js-work)
  (make-crossing label (lambda (loop) loop) baseline-loop js-work))

(define scheme-calling-javascript
  (list
   (scheme-calls "a function of a number, returning it" call-function
                 (plain-javascript "const f = (x) => x;" "f(1)" "1"))
   (scheme-calls "a method, through js-invoke" call-method
                 (plain-javascript "const o = { inc(n) { return n + 1; } };" "o.inc(1)" "1"))
   (scheme-calls "a property read, through js-ref" read-property
                 (plain-javascript "const o = { x: 3 };" "o.x" "1"))
   (scheme-calls "a property written, through js-set!" write-property
                 (plain-javascript "const o = { y: 0 };" "(o.y = i, 1)" "1"))
   (scheme-calls "an object made, through js-obj" make-object
                 (plain-javascript "" "({ x: i }, 1)" "1"))
   (scheme-calls "a class's instance made, through js-new" make-instance
                 (plain-javascript "class P { constructor(x, y) { this.x = x; this.y = y; } }" "(new P(i, i), 1)" "1"))
   (scheme-calls "a string passed and one returned" pass-string
                 (plain-javascript "const f = (s) => s + '!';" "f('abc').length" "1"))))

;; The procedure under test of each crossing, interpreted: the callee of a
;; JavaScript loop, or a Scheme loop.
(define under-test
  (list ident string-size vector-sum point-x add-one
        call-function call-method read-property write-property make-object make-instance pass-string))

;; ---------------------------------------------------------------------------
;; Timing
;; ---------------------------------------------------------------------------

;; /**
;;  * A procedure compiled, as the tier would compile it.
;;  * @param {procedure} proc - An interpreted procedure.
;;  * @param {string} name - Its name.
;;  * @returns {procedure}
;;  */
(define (compiled proc name)
  (let ((outcome (compile-closure proc name #f)))
    (if (compiled? outcome)
        (compiled-procedure outcome)
        (error "the compiler declined" name (declined-reason outcome)))))

;; /**
;;  * The best time of a call, over `runs`, and what the call answered.
;;  * @param {procedure} thunk - The call.
;;  * @returns {pair} `(milliseconds . answer)`.
;;  */
(define (best-time thunk)
  (let loop ((r 0) (best #f) (answer #f))
    (if (= r runs)
        (cons best answer)
        (let* ((start (now)) (value (thunk)) (took (- (now) start)))
          (loop (+ r 1) (if best (min best took) took) value)))))

;; /**
;;  * A crossing's cost in one tier, in nanoseconds a crossing, and its total.
;;  * @param {procedure} loop - The loop making the crossing, from a count.
;;  * @param {procedure} baseline - The loop around the cheapest call.
;;  * @param {integer} n - How many crossings.
;;  * @returns {pair} `(nanoseconds . total)`.
;;  */
(define (cost loop baseline n)
  (let ((timed (best-time (lambda () (loop n))))
        (base (best-time (lambda () (baseline n)))))
    (cons (/ (* 1e6 (- (car timed) (car base))) n) (cdr timed))))

;; /**
;;  * A number of nanoseconds, to one decimal place, in a column.
;;  * @param {number} ns - The number.
;;  * @returns {string}
;;  */
(define (ns-column ns)
  (string-pad (number->string (/ (round (* ns 10)) 10.0)) 12))

;; /**
;;  * Times a crossing in both tiers and in plain JavaScript, and writes its row.
;;  * @param {crossing} c - The crossing.
;;  * @param {procedure} interpreted - Its procedure under test, interpreted.
;;  * @param {procedure} compiled-proc - The same, compiled.
;;  */
(define (report-crossing c interpreted compiled-proc)
  (let* ((scheme (crossing-scheme c))
         ;; The baseline loop is compiled for the compiled tier too, where it
         ;; is Scheme's.
         (baseline (crossing-baseline c))
         (compiled-baseline (if (eq? baseline baseline-loop) compiled-baseline-loop baseline))
         (slow (cost (scheme interpreted) baseline interpreted-crossings))
         (fast (cost (scheme compiled-proc) compiled-baseline compiled-crossings))
         (plain (/ (* 1e6 ((crossing-javascript c) compiled-crossings)) compiled-crossings)))
    (if (not (= (/ (cdr slow) interpreted-crossings) (/ (cdr fast) compiled-crossings)))
        (error "the tiers answered differently" (crossing-label c) (cdr slow) (cdr fast)))
    (display (string-append "  " (string-pad-right (crossing-label c) 50)
                            (ns-column (car slow)) (ns-column (car fast)) (ns-column plain)))
    (newline)))

(define compiled-baseline-loop (compiled baseline-loop "baseline-loop"))

(define (heading title)
  (newline)
  (display (string-append (string-pad-right title 52) "  interpreted    compiled    plain JS"))
  (newline))

(display (string-append "Interop: nanoseconds a crossing, best of " (number->string runs)
                        ", the loop around the cheapest call subtracted"))
(newline)

(heading "JavaScript calling Scheme")
(for-each (lambda (c proc) (report-crossing c proc (compiled proc (crossing-label c))))
          javascript-calling-scheme (take under-test 5))
(heading "Scheme calling JavaScript")
(for-each (lambda (c proc) (report-crossing c proc (compiled proc (crossing-label c))))
          scheme-calling-javascript (drop under-test 5))
