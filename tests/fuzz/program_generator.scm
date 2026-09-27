;;; program_generator.scm -- random programs for the differential fuzzer.
;;;
;;; `generate-program` builds, from a seed, a program over what the compiler
;;; tier compiles, and says which of its procedures to compile. The fuzzer
;;; (`differential_fuzz_tests.js`) runs it with everything interpreted and with
;;; those procedures compiled, and compares the answers. The interpreter is the
;;; reference semantics, so any disagreement is a compiler bug.
;;;
;;; What is generated is what the tier's machinery has failed on before: loops,
;;; closures, assigned locals and globals, escapes, a continuation captured at a
;;; random site and re-entered twice, errors raised and caught, `dynamic-wind`,
;;; multiple values, higher-order calls through the library, and recursion deep
;;; enough to move frames to the heap, alternating between the tiers.
;;;
;;; Every program terminates and means the same every time. Loops count down
;;; from small literals; a procedure calls itself only on a smaller first
;;; argument and calls only procedures defined before it; the saved
;;; continuation is re-entered at most twice, by the driver. Types are tracked
;;; -- integers, lists, booleans, procedures from integers to integers -- so that
;;; most programs run to an answer rather than stopping at the first type error;
;;; errors are raised on purpose instead, and caught or not.

;; ---------------------------------------------------------------------------
;; Random numbers
;; ---------------------------------------------------------------------------

;; The generator's state: a linear congruential generator, which is all a
;; reproducible fuzzer needs.
(define *rng* 1)

;; /**
;;  * Starts the generator from a seed.
;;  * @param {integer} seed - Any exact integer.
;;  */
(define (seed-rng! seed)
  (set! *rng* (+ 1 (modulo seed 2147483647))))

;; /**
;;  * A random integer below n.
;;  * @param {integer} n - The bound, at most 32768.
;;  * @returns {integer} An integer in [0, n).
;;  */
(define (rand n)
  (set! *rng* (modulo (+ (* *rng* 1103515245) 12345) 2147483648))
  (modulo (quotient *rng* 65536) n))

;; /**
;;  * True with the given chance.
;;  * @param {integer} percent - The chance, out of 100.
;;  * @returns {boolean}
;;  */
(define (chance? percent) (< (rand 100) percent))

;; /**
;;  * A random element of a list.
;;  * @param {list} items - A non-empty list.
;;  * @returns {*} One of them.
;;  */
(define (pick items) (list-ref items (rand (length items))))

;; ---------------------------------------------------------------------------
;; Names
;; ---------------------------------------------------------------------------

;; Numbers fresh local names, so no binding is ever shadowed by accident.
(define *fresh* 0)

;; /**
;;  * A local name not used before in this program.
;;  * @param {string} stem - What the name starts with.
;;  * @returns {symbol} The name.
;;  */
(define (fresh stem)
  (set! *fresh* (+ *fresh* 1))
  (string->symbol (string-append stem (number->string *fresh*))))

;; ---------------------------------------------------------------------------
;; The scope an expression is generated in
;; ---------------------------------------------------------------------------
;;
;; A scope is a list of (name . type), innermost first; a type is one of int,
;; list and proc (an integer to an integer). `callable` is how many of the
;; program's procedures an expression may call: those defined before the one
;; being generated, so that calls never recur except through the argument that
;; counts down.

;; /**
;;  * The names in scope of a type.
;;  * @param {list} scope - The scope.
;;  * @param {symbol} type - The type.
;;  * @returns {list} The names.
;;  */
(define (names-of scope type)
  (let loop ((s scope) (acc '()))
    (cond ((null? s) acc)
          ((eq? (cdar s) type) (loop (cdr s) (cons (caar s) acc)))
          (else (loop (cdr s) acc)))))

;; /**
;;  * The name of the i-th procedure of the program.
;;  * @param {integer} i - Its index.
;;  * @returns {symbol} The name.
;;  */
(define (proc-name i) (string->symbol (string-append "p" (number->string i))))

;; ---------------------------------------------------------------------------
;; Expressions
;; ---------------------------------------------------------------------------

;; /**
;;  * A small integer literal.
;;  * @returns {integer}
;;  */
(define (small) (- (rand 12) 3))

;; /**
;;  * An integer-valued leaf: a literal, a variable, or a global.
;;  * @param {list} scope - The scope.
;;  * @returns {*} The expression.
;;  */
(define (int-leaf scope)
  (let ((vars (names-of scope 'int)))
    (cond ((and (pair? vars) (chance? 60)) (pick vars))
          ((chance? 15) (pick '(*g0* *g1*)))
          (else (small)))))

;; /**
;;  * An integer-valued expression.
;;  * @param {integer} depth - How much deeper it may nest.
;;  * @param {list} scope - The scope.
;;  * @param {integer} callable - How many procedures it may call.
;;  * @returns {*} The expression.
;;  */
(define (gen-int depth scope callable)
  (if (<= depth 0)
      (int-leaf scope)
      (let ((d (- depth 1)))
        (case (rand 27)
          ((0 1 2) (int-leaf scope))
          ((3 4) (list (pick '(+ - +)) (gen-int d scope callable) (gen-int d scope callable)))
          ((5) (list '* (gen-int d scope callable) (pick '(2 3 -1))))
          ((6) (list 'quotient (gen-int d scope callable) (pick '(2 3 -4))))
          ((7) (list 'if (gen-bool d scope callable) (gen-int d scope callable) (gen-int d scope callable)))
          ((8) (let ((v (fresh "v")))
                 (list 'let (list (list v (gen-int d scope callable)))
                       (gen-int d (cons (cons v 'int) scope) callable))))
          ;; A first argument of 0 or 1, since the procedure called counts it
          ;; down, and a chain of procedures each recurring on what it was
          ;; given would multiply.
          ((9) (if (> callable 0)
                   (list (proc-name (rand callable)) (rand 2) (gen-int d scope callable))
                   (int-leaf scope)))
          ((10) (let ((procs (names-of scope 'proc)))
                  (if (pair? procs)
                      (list (pick procs) (gen-int d scope callable))
                      (list (gen-proc d scope callable) (gen-int d scope callable)))))
          ((11) (let ((i (fresh "i")) (acc (fresh "acc")) (loop (fresh "loop")))
                  (list 'let loop (list (list i (rand 5)) (list acc (gen-int d scope callable)))
                        (list 'if (list '<= i 0) acc
                              (list loop (list '- i 1)
                                    (gen-int d (cons (cons i 'int) (cons (cons acc 'int) scope)) callable))))))
          ((12) (let ((vars (names-of scope 'int)))
                  (if (pair? vars)
                      (let ((v (pick vars)))
                        (list 'begin (list 'set! v (gen-int d scope callable))
                              (list '+ v (gen-int d scope callable))))
                      (int-leaf scope))))
          ((13) (let ((k (fresh "k")))
                  (list 'call/cc
                        (list 'lambda (list k)
                              (list '+ (gen-int d scope callable)
                                    (list 'if (gen-bool d scope callable)
                                          (list k (gen-int d scope callable))
                                          (gen-int d scope callable)))))))
          ((14) (let ((c (fresh "c")))
                  ;; The continuation the driver re-enters, captured here the
                  ;; first time this is reached.
                  (list 'call/cc
                        (list 'lambda (list c)
                              (list 'if '(not *k*) (list 'set! '*k* c) #f)
                              (gen-int d scope callable)))))
          ((15) (gen-guard d scope callable))
          ((16) (list 'dynamic-wind
                      '(lambda () (set! *log* (cons 'in *log*)))
                      (list 'lambda '() (gen-int d scope callable))
                      '(lambda () (set! *log* (cons 'out *log*)))))
          ((17) (let ((f (fresh "f")))
                  (list 'let (list (list f (gen-proc d scope callable)))
                        (list '+ (list f (gen-int d scope callable)) (list f (gen-int d scope callable))))))
          ((18) (pick (list (list 'length (gen-list d scope callable))
                            (list 'apply '+ (gen-list d scope callable))
                            (let ((l (fresh "l")))
                              (list 'let (list (list l (gen-list d scope callable)))
                                    (list 'if (list 'null? l) (gen-int d scope callable) (list 'car l)))))))
          ((19) (let ((vec (fresh "vec")))
                  (list 'let (list (list vec (list 'make-vector 3 (gen-int d scope callable))))
                        (list 'vector-set! vec (rand 3) (gen-int d scope callable))
                        (list '+ (list 'vector-ref vec 0) (list 'vector-ref vec 2)))))
          ((20) (let ((p (fresh "p")) (q (fresh "q")))
                  (list 'call-with-values
                        (list 'lambda '() (list 'values (gen-int d scope callable) (gen-int d scope callable)))
                        (list 'lambda (list p q) (list '- p q)))))
          ((21) (list 'case (list 'modulo (gen-int d scope callable) 3)
                      (list '(0) (gen-int d scope callable))
                      (list '(1) (gen-int d scope callable))
                      (list 'else (gen-int d scope callable))))
          ((22) (let ((i (fresh "i")) (s (fresh "s")))
                  (list 'do (list (list i 0 (list '+ i 1))
                                  (list s (gen-int d scope callable) (list '+ s i)))
                        (list (list '>= i (rand 5)) s))))
          ((23) (list 'begin (list 'set! '*g0* (list '+ '*g0* (gen-int d scope callable))) '*g0*))
          ((24) (let ((k (fresh "k")) (e (fresh "e")))
                  (list 'call/cc
                        (list 'lambda (list k)
                              (list 'with-exception-handler
                                    (list 'lambda (list e) (list k (gen-int d scope callable)))
                                    (list 'lambda '()
                                          (list '+ (gen-int d scope callable)
                                                (list 'if (gen-bool d scope callable)
                                                      '(raise 'oops)
                                                      (gen-int d scope callable)))))))))
          ((25) (let ((h (fresh "h")) (z (fresh "z")))
                  (list (list 'lambda '()
                              (list 'define (list h z) (list '+ z (gen-int d scope callable)))
                              (list h (gen-int d scope callable))))))
          (else (gen-higher-order d scope callable))))))

;; /**
;;  * An integer-valued `guard` around a body that may raise: a symbol, a
;;  * number, an error object, or a primitive's own error.
;;  * @param {integer} d - How much deeper it may nest.
;;  * @param {list} scope - The scope.
;;  * @param {integer} callable - How many procedures it may call.
;;  * @returns {list} The expression.
;;  */
(define (gen-guard d scope callable)
  (let ((e (fresh "e")))
    (list 'guard (list e
                       (list (list 'symbol? e) (gen-int d scope callable))
                       (list (list 'number? e) e)
                       (list (list 'error-object? e) (list 'length (list 'error-object-irritants e))))
          (list '+ (gen-int d scope callable)
                (list 'if (gen-bool d scope callable)
                      (pick (list ''boom-raise
                                  (list 'raise ''boom)
                                  (list 'raise (gen-int d scope callable))
                                  (list 'error "fuzz" (gen-int d scope callable) (gen-int d scope callable))
                                  '(vector-ref (vector 1 2) 5)
                                  '(car '())))
                      (gen-int d scope callable))))))

;; /**
;;  * An integer from a higher-order call through the standard library, which
;;  * the browser installs compiled: so an interpreted procedure passed to it
;;  * alternates the tiers.
;;  * @param {integer} d - How much deeper it may nest.
;;  * @param {list} scope - The scope.
;;  * @param {integer} callable - How many procedures it may call.
;;  * @returns {list} The expression.
;;  */
(define (gen-higher-order d scope callable)
  (let* ((z (fresh "z"))
         (inner (cons (cons z 'int) scope)))
    (case (rand 4)
      ((0) (list 'apply '+ (list 'map (list 'lambda (list z) (gen-int d inner callable))
                                 (gen-list d scope callable))))
      ((1) (let ((acc (fresh "acc")))
             (list 'let (list (list acc 0))
                   (list 'for-each (list 'lambda (list z) (list 'set! acc (list '+ acc (gen-int d inner callable))))
                         (gen-list d scope callable))
                   acc)))
      ((2) (list 'vector-ref (list 'vector-map (list 'lambda (list z) (gen-int d inner callable))
                                   (list 'vector (gen-int d scope callable) (gen-int d scope callable)))
                 (rand 2)))
      (else (if (> callable 0)
                (list 'apply (proc-name (rand callable)) (list 'list (rand 2) (gen-int d scope callable)))
                (gen-int d scope callable))))))

;; /**
;;  * A list-valued expression, of integers.
;;  * @param {integer} depth - How much deeper it may nest.
;;  * @param {list} scope - The scope.
;;  * @param {integer} callable - How many procedures it may call.
;;  * @returns {*} The expression.
;;  */
(define (gen-list depth scope callable)
  (let ((d (- depth 1))
        (lists (names-of scope 'list)))
    (if (or (<= depth 0) (chance? 20))
        (if (and (pair? lists) (chance? 50))
            (pick lists)
            (cons 'list (list (int-leaf scope) (int-leaf scope) (int-leaf scope))))
        (case (rand 7)
          ((0) (list 'cons (gen-int d scope callable) (gen-list d scope callable)))
          ((1) (let ((z (fresh "z")))
                 (list 'map (list 'lambda (list z) (gen-int d (cons (cons z 'int) scope) callable))
                       (gen-list d scope callable))))
          ((2) (list 'reverse (gen-list d scope callable)))
          ((3) (list 'append (gen-list d scope callable) (gen-list d scope callable)))
          ((4) (let ((i (fresh "i")) (acc (fresh "acc")) (loop (fresh "loop")))
                 (list 'let loop (list (list i (rand 4)) (list acc ''()))
                       (list 'if (list '= i 0) acc
                             (list loop (list '- i 1)
                                   (list 'cons (gen-int d (cons (cons i 'int) scope) callable) acc))))))
          ((5) (list 'vector->list (list 'make-vector (rand 3) (gen-int d scope callable))))
          (else (let ((l (fresh "l")))
                  (list 'let (list (list l (gen-list d scope callable)))
                        (list 'if (list 'null? l) l (list 'cdr l)))))))))

;; /**
;;  * A boolean-valued expression.
;;  * @param {integer} depth - How much deeper it may nest.
;;  * @param {list} scope - The scope.
;;  * @param {integer} callable - How many procedures it may call.
;;  * @returns {*} The expression.
;;  */
(define (gen-bool depth scope callable)
  (let ((d (- depth 1)))
    (if (<= depth 0)
        (pick (list #t #f (list '< (int-leaf scope) (int-leaf scope)) (list 'even? (int-leaf scope))))
        (case (rand 8)
          ((0) (list '< (gen-int d scope callable) (gen-int d scope callable)))
          ((1) (list '= (gen-int d scope callable) (gen-int d scope callable)))
          ((2) (list 'even? (gen-int d scope callable)))
          ((3) (list 'null? (gen-list d scope callable)))
          ((4) (list 'not (gen-bool d scope callable)))
          ((5) (list 'and (gen-bool d scope callable) (gen-bool d scope callable)))
          ((6) (list 'or (gen-bool d scope callable) (gen-bool d scope callable)))
          (else (list 'pair? (gen-list d scope callable)))))))

;; /**
;;  * A procedure from an integer to an integer: a lambda, closing over
;;  * whatever is in scope.
;;  * @param {integer} depth - How much deeper it may nest.
;;  * @param {list} scope - The scope.
;;  * @param {integer} callable - How many procedures it may call.
;;  * @returns {list} The expression.
;;  */
(define (gen-proc depth scope callable)
  (let ((z (fresh "z")))
    (list 'lambda (list z) (gen-int depth (cons (cons z 'int) scope) callable))))

;; ---------------------------------------------------------------------------
;; Programs
;; ---------------------------------------------------------------------------

;; /**
;;  * The i-th procedure: `(define (pi x y) ...)`, which may call itself on a
;;  * smaller `x` and may call the procedures before it.
;;  * @param {integer} i - Its index.
;;  * @returns {list} The definition.
;;  */
(define (gen-procedure i)
  (let* ((scope '((x . int) (y . int)))
         (body (gen-int (+ 2 (rand 3)) scope i)))
    (list 'define (list (proc-name i) 'x 'y)
          (if (chance? 45)
              (list 'if '(<= x 0) (gen-int 2 scope i)
                    (list (pick '(+ - +)) (list (proc-name i) '(- x 1) (gen-int 1 scope i)) body))
              body))))

;; The recursions that go deep enough to move frames to the heap: `hop` calls
;; what it is given, so with it and `deep` compiled or interpreted at random the
;; recursion alternates between the tiers; `grab-deep` captures the saved
;; continuation at the bottom; `depth` walks a deep tree through `map`.
(define deep-definitions
  '((define (hop f n) (f n))
    (define (deep n) (if (= n 0) 0 (+ 1 (hop deep (- n 1)))))
    (define (grab-deep n)
      (if (= n 0)
          (call/cc (lambda (c) (if (not *k*) (set! *k* c)) 0))
          (+ 1 (hop grab-deep (- n 1)))))
    (define (tree n) (if (= n 0) '() (list (tree (- n 1)))))
    (define (depth t) (if (pair? t) (+ 1 (apply max (map depth t))) 0))))

;; /**
;;  * A program, and the procedures to compile.
;;  *
;;  * The driver evaluates calls to the program's procedures, records their
;;  * values, and re-enters the saved continuation, if one was captured, twice;
;;  * its value is every record, the `dynamic-wind` trail, and the globals.
;;  *
;;  * @param {integer} seed - Chooses the program.
;;  * @returns {list} `(forms compiled)`: the top-level forms, in order, the
;;  *   last being the driver; and the names of the procedures to compile.
;;  */
(define (generate-program seed)
  (seed-rng! seed)
  (set! *fresh* 0)
  (let* ((count (+ 2 (rand 5)))
         (procedures (let loop ((i 0) (acc '()))
                       (if (= i count) (reverse acc) (loop (+ i 1) (cons (gen-procedure i) acc)))))
         (deep? (chance? 25))
         ;; An error nothing catches, raised in compiled code where a value is
         ;; wanted: the program's answer is its message.
         (uncaught? (chance? 10))
         (calls (let loop ((n (+ 2 (rand 4))) (acc '()))
                  (if (= n 0)
                      acc
                      (loop (- n 1)
                            (cons (list (proc-name (rand count)) (rand 7) (small)) acc)))))
         (calls (if uncaught?
                    (cons (list 'fail (rand 3) (small)) calls)
                    calls))
         (deep-calls (if deep?
                         (list (pick (list (list 'deep (+ 5000 (rand 20000)))
                                           (list 'grab-deep (+ 3000 (rand 10000)))
                                           (list 'depth (list 'tree (+ 1000 (rand 4000)))))))
                         '()))
         (names (append (let loop ((i (- count 1)) (acc '()))
                          (if (< i 0) acc (loop (- i 1) (cons (proc-name i) acc))))
                        (if deep? '(hop deep grab-deep tree depth) '())
                        (if uncaught? '(fail) '())))
         (compiled (let loop ((ns names) (acc '()))
                     (cond ((null? ns) (reverse acc))
                           ((chance? 60) (loop (cdr ns) (cons (car ns) acc)))
                           (else (loop (cdr ns) acc))))))
    (list (append '((define *k* #f) (define *count* 0) (define *results* '())
                    (define *log* '()) (define *g0* 0) (define *g1* 7))
                  procedures
                  (if deep? deep-definitions '())
                  (if uncaught?
                      '((define (fail x y) (+ y (vector-ref (vector x y) (+ x 5)))))
                      '())
                  ;; Half the drivers are a named `let` entered once, which the
                  ;; tier compiles as a top-level expression that loops.
                  (list (append (if (chance? 50) '(let driver) '(let))
                                (list (list (list 'r (cons 'list (append calls deep-calls))))
                                      '(set! *results* (cons r *results*))
                                      '(if (and *k* (< *count* 2))
                                           (begin (set! *count* (+ *count* 1)) (*k* (* 100 *count*)))
                                           (list (reverse *results*) *log* *g0* *g1*))))))
          compiled)))
