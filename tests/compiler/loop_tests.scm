;; Loops, as the lowering finds them (src/compiler/ir.scm)
;;
;; Runs in the compiler's own environment. A tail call to the procedure that
;; contains it is tagged `local` or `global` on its `call` node, and the emitter
;; compiles it as a jump rather than a trip through the trampoline; a `letrec`
;; group that is a loop entered once in tail position is marked to be emitted
;; inside the procedure that enters it. These check what the lowering decides,
;; from source run through the real analyzer (`analyze-lambda`, which the test
;; runner provides). What the emitter does with the tags, and that looped code
;; gives the interpreter's answers, is checked from JavaScript, where the
;; generated code can be run: tests/functional/loop_compilation_tests.js.

;; /**
;;  * The IR the lowering produces for a definition.
;;  * @param {list} definition - A `define` form, as data.
;;  * @returns {list} Its lambda's IR.
;;  * @throws If the lowering declined it, so a test cannot pass by accident on
;;  *   an empty result.
;;  */
(define (lowered-definition definition)
  (let ((result (lower-lambda (analyze-lambda definition))))
    (if (eq? (car result) 'ok)
        (cadr result)
        (error "the lowering declined the definition" (cadr result)))))

;; /**
;;  * The loop tag on every tagged call in a definition, in walk order.
;;  * @param {list} definition - A `define` form, as data.
;;  * @returns {list} Symbols, `local` or `global`.
;;  */
(define (loop-tags definition)
  (filter-map call-loop (ir-find (tagged? 'call) (lowered-definition definition))))

;; /**
;;  * How many `letrec` groups in a definition are marked to be emitted inside
;;  * the procedure that enters them.
;;  * @param {list} definition - A `define` form, as data.
;;  * @returns {integer} The count.
;;  */
(define (inlined-loops definition)
  (count letrec-inline? (ir-find (tagged? 'letrec) (lowered-definition definition))))

(test-group "loops - tail calls to the procedure itself"
  (test "a top-level procedure calling itself in tail position" '(global)
        (loop-tags '(define (f n) (if (= n 0) 'done (f (- n 1))))))
  (test "a named let" '(local)
        (loop-tags '(define (sum n)
                      (let loop ((i 0) (acc 0)) (if (> i n) acc (loop (+ i 1) (+ acc i)))))))
  (test "a do loop" '(local)
        (loop-tags '(define (f n) (do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i n) acc)))))
  ;; The call from `f` to `loop` is a tail call too, but not one from `loop` to
  ;; itself, so only the call inside `loop` is tagged.
  (test "an internally defined procedure calling itself" '(local)
        (loop-tags '(define (f n)
                      (define (loop i acc) (if (= i 0) acc (loop (- i 1) (+ acc i))))
                      (loop n 0)))))

(test-group "loops - calls that look like loops and are not"
  (test "a call that is not in tail position" '()
        (loop-tags '(define (f n) (if (= n 0) 0 (+ 1 (f (- n 1)))))))
  (test "a self-call with the wrong number of arguments" '()
        (loop-tags '(define (f n) (if (= n 0) 0 (f n n)))))
  (test "a procedure with a rest parameter" '()
        (loop-tags '(define (f . xs) (if (null? xs) 0 (f)))))
  (test "a call to a sibling in a letrec group" '()
        (loop-tags '(define (f n)
                      (letrec ((e (lambda (n) (if (= n 0) #t (o (- n 1)))))
                               (o (lambda (n) (if (= n 0) #f (e (- n 1))))))
                        (e n)))))
  ;; The call is in tail position in the inner lambda, which is a different
  ;; procedure from `loop`: jumping to the top of the inner one would be wrong.
  (test "a call to the enclosing loop from a nested procedure" '()
        (loop-tags '(define (f g)
                      (let loop ((i 0)) (if (< i 3) (g (lambda () (loop (+ i 1)))) i)))))
  (test "a call to the global from a nested procedure" '()
        (loop-tags '(define (f g) (g (lambda () (f g))))))
  ;; A loop name that is ever assigned may not name this procedure when the
  ;; call happens.
  (test "a loop whose name is assigned" '()
        (loop-tags '(define (f)
                      (let loop ((i 0))
                        (if (< i 3) (begin (set! loop loop) (loop (+ i 1))) i))))))

;; A loop's own iterations can jump, but entering it still made a closure and
;; returned a `TailCall` -- on every call of a procedure like `assq`, whose
;; lists are usually two long, that entry was nearly the whole cost. A `letrec`
;; whose name is only ever called, entered once in tail position and otherwise
;; only by its own looping calls, can be the enclosing procedure's own loop.
(test-group "loops - emitted inside the procedure that enters them"
  (test "a named let in tail position" 1
        (inlined-loops '(define (sum n)
                          (let loop ((i 0) (acc 0)) (if (> i n) acc (loop (+ i 1) (+ acc i)))))))
  (test "a do loop" 1
        (inlined-loops '(define (f n) (do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i n) acc)))))
  (test "the loop inside an assq" 1
        (inlined-loops '(define (my-assq x l)
                          (letrec ((loop (lambda (l)
                                           (cond ((null? l) #f)
                                                 ((eq? x (car (car l))) (car l))
                                                 (else (loop (cdr l)))))))
                            (loop l)))))
  (test "a loop entered from inside another inlined loop" 2
        (inlined-loops '(define (f n)
                          (let a ((i 0))
                            (if (< i n)
                                (a (+ i 1))
                                (let b ((j i)) (if (> j 0) (b (- j 1)) (list i j)))))))))

(test-group "loops - not emitted inside the procedure that enters them"
  (test "a loop whose value its caller wants" 0
        (inlined-loops '(define (f n) (+ 1 (let loop ((i 0)) (if (< i n) (loop (+ i 1)) i))))))
  (test "a loop that escapes as a value" 0
        (inlined-loops '(define (f) (let loop ((i 0)) (if (< i 3) (loop (+ i 1)) loop)))))
  (test "a loop that recurses rather than iterates" 0
        (inlined-loops '(define (f n) (let loop ((i n)) (if (= i 0) 0 (+ 1 (loop (- i 1))))))))
  (test "a loop called from a nested procedure" 0
        (inlined-loops '(define (f g)
                          (let loop ((i 0)) (if (< i 3) (g (lambda () (loop (+ i 1)))) i)))))
  (test "a mutually recursive group" 0
        (inlined-loops '(define (f n)
                          (letrec ((e (lambda (n) (if (= n 0) #t (o (- n 1)))))
                                   (o (lambda (n) (if (= n 0) #f (e (- n 1))))))
                            (e n))))))
