;; double_loop_tests.scm -- loops over inexact reals, in either tier.
;;
;; A JavaScript number that is an integer is an exact integer, and an inexact
;; real whose value is an integer is boxed
;; (src/core/interpreter/number_representation.js). Compiled code runs a loop
;; whose variables stay inexact on raw doubles, boxing a value where it leaves
;; the loop, when its variables arrive inexact; otherwise, and in the
;; interpreter, the loop runs as written ("Loops on raw doubles" in
;; src/compiler/emit.scm). Either way the answers, and their exactness, are
;; the same: these check them where a raw double could be mistaken for an
;; exact integer -- a value that lands on an integer, -0.0, an exact entry
;; where an inexact one was expected -- and where the loop reads locals bound
;; outside it.
;;
;; The file runs twice, interpreted and with the tier attached
;; (tests/run_tiered_scheme_tests_lib.js), and each procedure is called twice
;; before it is tested, since the tier compiles a procedure on its second call.

;; /**
;;  * The sum of i from n down to 0, kept in a double.
;;  * @param {number} n - Where to start.
;;  * @returns {number}
;;  */
(define (sum-down n)
  (let loop ((i n) (acc 0.))
    (if (< i 0.) acc (loop (- i 1.) (+ i acc)))))

;; /**
;;  * The same sum, its own exactness whatever the entry's.
;;  * @param {number} n - Where to start.
;;  * @returns {number}
;;  */
(define (sum-down-from n)
  (let loop ((i n) (acc 0))
    (if (< i 0) acc (loop (- i 1) (+ i acc)))))

;; /**
;;  * x doubled `times` times, counting exactly.
;;  * @param {number} x - The start.
;;  * @param {integer} times - How many times.
;;  * @returns {number}
;;  */
(define (double-up x times)
  (let loop ((x x) (n 0))
    (if (= n times) x (loop (* x 2) (+ n 1)))))

;; /**
;;  * Ten steps of the Mandelbrot iteration from (cr, ci), or fewer if it
;;  * leaves the circle of radius 2: the point reached and the steps taken. The
;;  * loop reads `cr` and `ci`, bound outside it.
;;  * @param {number} cr - The real part.
;;  * @param {number} ci - The imaginary part.
;;  * @returns {list}
;;  */
(define (iterate cr ci)
  (let loop ((zr cr) (zi ci) (c 0))
    (if (= c 10)
        (list zr zi c)
        (let ((zr2 (* zr zr)) (zi2 (* zi zi)))
          (if (> (+ zr2 zi2) 4.)
              (list zr zi c)
              (loop (+ (- zr2 zi2) cr) (+ (* 2. (* zr zi)) ci) (+ c 1)))))))

(define (warm thunk) (thunk) (thunk))
(warm (lambda () (sum-down 3.)))
(warm (lambda () (sum-down-from 3)))
(warm (lambda () (double-up 1. 2)))
(warm (lambda () (iterate .25 .5)))

;; /**
;;  * Whether a procedure is compiled.
;;  * @param {procedure} procedure - The procedure.
;;  * @returns {boolean}
;;  */
(define (compiled? procedure)
  (eq? #t (js-ref procedure "$compiled")))

(test-group "loops over inexact reals"
  (test "the tier compiled them, and only in the run with it attached"
        *tier-attached*
        (and (compiled? sum-down) (compiled? double-up) (compiled? iterate)))
  (test "a sum of integral doubles" 55. (sum-down 10.))
  (test "is inexact, though every value it passes through is an integer" #f (exact? (sum-down 10.)))
  (test "entered with a box, an inexact integer, or with a fraction"
        '(55. .5)
        (list (sum-down (+ 5. 5.)) (sum-down .5)))
  (test "the same loop counting exactly stays exact" '(55 #t)
        (let ((sum (sum-down-from 10))) (list sum (exact? sum))))
  (test "an exact entry where an inexact one was looked for stays exact" '(8 #t)
        (let ((x (double-up 1 3))) (list x (exact? x))))
  (test "an inexact entry doubled exactly is inexact" '(8. #f)
        (let ((x (double-up 1. 3))) (list x (exact? x))))
  (test "-0.0 keeps its sign" "-0.0" (number->string (double-up -0. 2)))
  (test "a rational entry" 8 (double-up 1/2 4))
  (test "a complex entry" (make-rectangular 0. 8.) (double-up (make-rectangular 0. 1.) 3))
  (test "a loop reading locals bound outside it"
        '((-0.26433754502456985 0.574329881136845 10) (1. 3. 1) (0. 0. 10))
        (list (iterate .25 .5) (iterate 1. 1.) (iterate 0. 0.)))
  (test "and exact locals bound outside it, which the inexact constant makes inexact"
        '(0. 0. 10)
        (iterate 0 0)))
