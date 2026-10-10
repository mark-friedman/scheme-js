;; run_evaluator.scm -- task 68's ceiling: the evaluator written in Scheme, to
;; the interpreter's own design, and compiled.
;;
;; Task 68 would write the evaluator -- the step loop, the frames, the nodes'
;; steps, the environments (src/core/interpreter/) -- in Scheme, once compiled
;; Scheme is close enough to hand-written JavaScript on hot code. This measures
;; how close, on the evaluator's own work: it is an evaluator in Scheme that
;; does what the JavaScript one does, step for step, for the forms ordinary
;; code is made of, and runs the programs of `benchmarks/evaluator/kernels.scm`,
;; which `benchmarks/evaluator/interpreted.scm` runs on the interpreter itself.
;; `benchmarks/run_evaluator.js` runs both and sets them side by side.
;;
;; The design is the JavaScript's (`interpreter.js`, `ast_nodes.js`,
;; `frames.js`, `environment.js`), not a better one, so that what differs is
;; the language and its compiler:
;;
;;   - The same core forms, from the expander, made the same nodes: a literal,
;;     a variable, `if`, a sequence, `lambda`, `letrec`, `set!`, `define` and an
;;     application, `let` being an application of a `lambda` (`assembler.js`).
;;   - The same machine: a control, an answer, an environment and a stack of
;;     frames, one step at a time, a node pushing a frame for what is to happen
;;     after a value it needs, and a value popping it.
;;   - The same short cuts: a literal or variable operand, or test, evaluated in
;;     place, as `continueApplication` and `IfNode` do, and the last of a
;;     sequence evaluated with no frame beneath it.
;;   - The same environments: a frame of names and values for each call, `let`
;;     and `letrec`, searched by name up the chain -- each a `Map` in
;;     JavaScript, here names and values side by side -- then the globals, a
;;     hash table (SRFI 125, which sits on a `Map`). A global is kept in the
;;     table once found, where JavaScript looks in the program's environment and
;;     then the system's each time: one lookup where it makes two.
;;   - A check on each step whether the program is being debugged, and the
;;     arity of each call, as JavaScript makes them.
;;
;; Left out, so that it is a ceiling: continuations, exception handlers,
;; `dynamic-wind`, `this`, a JavaScript function's argument conversion, the
;; tier's count of a procedure's calls, the debugger's records of frames, and
;; any form the kernels do not use.
;;
;;     node repl.js -I scripts/lib benchmarks/run_evaluator.scm [runs]

(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme file)
        (scheme eval)
        (scheme process-context)
        (srfi 125)
        (scheme-js interop)
        (scheme-js compiler build))

;; ---------------------------------------------------------------------------
;; Nodes, made from the expander's core forms
;; ---------------------------------------------------------------------------

(define-record-type lit-node (make-lit-node value) lit-node? (value lit-node-value))
(define-record-type var-node (make-var-node name) var-node? (name var-node-name))
(define-record-type if-node (make-if-node test then else) if-node?
  (test if-node-test) (then if-node-then) (else if-node-else))
;; Its expressions, a list.
(define-record-type seq-node (make-seq-node exprs) seq-node? (exprs seq-node-exprs))
;; Its parameters, a vector, and its rest parameter or #f.
(define-record-type lambda-node (make-lambda-node params rest name body) lambda-node?
  (params lambda-node-params) (rest lambda-node-rest) (name lambda-node-name) (body lambda-node-body))
;; Its names and lambdas, vectors.
(define-record-type letrec-node (make-letrec-node names lambdas body) letrec-node?
  (names letrec-node-names) (lambdas letrec-node-lambdas) (body letrec-node-body))
(define-record-type set-node (make-set-node name value) set-node? (name set-node-name) (value set-node-value))
(define-record-type define-node (make-define-node name value) define-node?
  (name define-node-name) (value define-node-value))
;; The operator and the operands, a vector.
(define-record-type app-node (make-app-node exprs) app-node? (exprs app-node-exprs))

;; /**
;;  * The node a core form is, as `build` in src/core/interpreter/assembler.js
;;  * makes one.
;;  * @param {list} form - A core form.
;;  * @returns {record}
;;  */
(define (analyze form)
  (case (car form)
    ((lit) (make-lit-node (cadr form)))
    ((var) (make-var-node (cadr form)))
    ((if) (make-if-node (analyze (cadr form)) (analyze (caddr form)) (analyze (cadddr form))))
    ((seq) (make-seq-node (map analyze (cadr form))))
    ((lambda) (make-lambda-node (list->vector (cadr form)) (caddr form) (cadddr form)
                                (analyze (car (cddddr form)))))
    ((letrec) (make-letrec-node (list->vector (cadr form)) (list->vector (map analyze (caddr form)))
                                (analyze (cadddr form))))
    ((set) (make-set-node (cadr form) (analyze (caddr form))))
    ((define) (make-define-node (cadr form) (analyze (caddr form))))
    ((app) (make-app-node (list->vector (cons (analyze (cadr form)) (map analyze (caddr form))))))
    (else (error "the evaluator ceiling does not run this form" (car form)))))

;; ---------------------------------------------------------------------------
;; Frames, the values the machine's runtime keeps
;; ---------------------------------------------------------------------------

(define-record-type if-frame (make-if-frame then else env) if-frame?
  (then if-frame-then) (else if-frame-else) (env if-frame-env))
;; The expressions left, a list.
(define-record-type seq-frame (make-seq-frame exprs env) seq-frame? (exprs seq-frame-exprs) (env seq-frame-env))
(define-record-type set-frame (make-set-frame name env) set-frame? (name set-frame-name) (env set-frame-env))
(define-record-type define-frame (make-define-frame name) define-frame? (name define-frame-name))
;; The values of the expressions before `index`, a vector as long as `exprs`.
(define-record-type app-frame (make-app-frame exprs index values env) app-frame?
  (exprs app-frame-exprs) (index app-frame-index) (values app-frame-values) (env app-frame-env))

;; An environment's frame: names and their values, vectors side by side.
(define-record-type frame (make-frame names values parent) frame?
  (names frame-names) (values frame-values) (parent frame-parent))

(define-record-type closure (make-closure params rest body env name) closure?
  (params closure-params) (rest closure-rest) (body closure-body) (env closure-env) (name closure-name))

;; The globals the programs define, and the system's they read
;; (`seed-globals!`).
(define globals (make-hash-table eq?))

;; Where the system's globals are found: (scheme base).
(define system (environment '(scheme base)))

;; Whether the program is being debugged, which every step asks.
(define debugging? #f)

;; /**
;;  * A global's value.
;;  * @param {symbol} name - The name.
;;  * @returns {*}
;;  */
(define (global-ref name)
  (let ((value (hash-table-ref/default globals name globals)))
    (if (eq? value globals) (error "unbound variable" name) value)))

;; /**
;;  * Puts in the table each global of the system's a core form reads, before
;;  * anything runs. Done apart from `global-ref`, which `eval` would have kept
;;  * from being compiled; a name the system does not bind -- a local, or one
;;  * the program defines -- is left.
;;  * @param {list} form - A core form.
;;  */
(define (seed-globals! form)
  (cond ((not (pair? form)) #f)
        ((eq? (car form) 'lit) #f)
        ((and (eq? (car form) 'var) (pair? (cdr form)) (symbol? (cadr form)))
         (let ((name (cadr form)))
           (if (not (hash-table-contains? globals name))
               (let ((value (guard (e (#t globals)) (eval name system))))
                 (if (not (eq? value globals)) (hash-table-set! globals name value))))))
        (else (for-each seed-globals! form))))

;; /**
;;  * A variable's value: searched by name in each frame up the chain, then the
;;  * globals.
;;  * @param {symbol} name - The name.
;;  * @param {frame|boolean} env - The innermost frame, or #f at top level.
;;  * @returns {*}
;;  */
(define (lookup name env)
  (let search ((f env) (i 0))
    (cond ((not f) (global-ref name))
          ((= i (vector-length (frame-names f))) (search (frame-parent f) 0))
          ((eq? (vector-ref (frame-names f) i) name) (vector-ref (frame-values f) i))
          (else (search f (+ i 1))))))

;; /**
;;  * Assigns a variable, where `lookup` finds it.
;;  * @param {symbol} name - The name.
;;  * @param {*} value - The value.
;;  * @param {frame|boolean} env - The innermost frame.
;;  */
(define (assign! name value env)
  (let search ((f env) (i 0))
    (cond ((not f) (hash-table-set! globals name value))
          ((= i (vector-length (frame-names f))) (search (frame-parent f) 0))
          ((eq? (vector-ref (frame-names f) i) name) (vector-set! (frame-values f) i value))
          (else (search f (+ i 1))))))

;; /**
;;  * Evaluates in place the literal and variable operands from `index` on,
;;  * storing their values, as `continueApplication` does.
;;  * @param {vector} exprs - The operator and operands.
;;  * @param {vector} values - Their values so far.
;;  * @param {integer} index - The first not yet evaluated.
;;  * @param {frame|boolean} env - The environment.
;;  * @returns {integer} The first that needs a step of its own, or the length.
;;  */
(define (evaluate-simple! exprs values index env)
  (let next ((i index))
    (if (= i (vector-length exprs))
        i
        (let ((e (vector-ref exprs i)))
          (cond ((lit-node? e) (vector-set! values i (lit-node-value e)) (next (+ i 1)))
                ((var-node? e) (vector-set! values i (lookup (var-node-name e) env)) (next (+ i 1)))
                (else i))))))

;; /**
;;  * A closure's frame for a call, its arity checked.
;;  * @param {closure} f - The closure.
;;  * @param {vector} values - The procedure and the arguments.
;;  * @returns {frame}
;;  */
(define (bind-arguments f values)
  (let ((params (closure-params f))
        (argc (- (vector-length values) 1)))
    (if (closure-rest f)
        (let ((required (vector-length params)))
          (if (< argc required) (error "wrong number of arguments" (closure-name f)))
          (make-frame (vector-append params (vector (closure-rest f)))
                      (vector-append (vector-copy values 1 (+ 1 required))
                                     (vector (vector->list values (+ 1 required))))
                      (closure-env f)))
        (begin
          (if (not (= argc (vector-length params))) (error "wrong number of arguments" (closure-name f)))
          (make-frame params (vector-copy values 1) (closure-env f))))))

;; /**
;;  * A procedure of the system's called with the arguments.
;;  * @param {procedure} f - The procedure.
;;  * @param {vector} values - The procedure and the arguments.
;;  * @returns {*}
;;  */
(define (call-procedure f values)
  (let ((n (vector-length values)))
    (cond ((= n 1) (f))
          ((= n 2) (f (vector-ref values 1)))
          ((= n 3) (f (vector-ref values 1) (vector-ref values 2)))
          ((= n 4) (f (vector-ref values 1) (vector-ref values 2) (vector-ref values 3)))
          (else (apply f (cdr (vector->list values)))))))

;; The machine's two moves, written into its loop: the value of a step going
;; to the frame on top of the stack, or out; and a call, once the procedure
;; and its arguments are values.
(define-syntax produce
  (syntax-rules ()
    ((_ loop env stack value)
     (let ((v value))
       (if (null? stack) v (loop (car stack) env (cdr stack) v))))))

(define-syntax apply-values
  (syntax-rules ()
    ((_ loop env stack values)
     (let ((f (vector-ref values 0)))
       (cond ((closure? f) (loop (closure-body f) (bind-arguments f values) stack #f))
             ((procedure? f) (produce loop env stack (call-procedure f values)))
             (else (error "application: not a procedure" f)))))))

;; /**
;;  * Runs a node to its value: the machine, a step at a time.
;;  * @param {record} node - The node.
;;  * @param {frame|boolean} env - The environment.
;;  * @returns {*}
;;  */
(define (execute node env)
  (let loop ((ctl node) (env env) (stack '()) (ans #f))
    (if debugging? (error "the evaluator ceiling does not debug"))
    (cond
      ((app-node? ctl)
       (let* ((exprs (app-node-exprs ctl))
              (values (make-vector (vector-length exprs) #f))
              (next (evaluate-simple! exprs values 0 env)))
         (if (< next (vector-length exprs))
             (loop (vector-ref exprs next) env (cons (make-app-frame exprs next values env) stack) ans)
             (apply-values loop env stack values))))
      ((app-frame? ctl)
       ;; A fresh vector of values, as `AppFrame` makes a fresh array: a frame
       ;; on the stack is never changed.
       (let* ((exprs (app-frame-exprs ctl))
              (index (app-frame-index ctl))
              (values (vector-copy (app-frame-values ctl)))
              (env (app-frame-env ctl)))
         (vector-set! values index ans)
         (let ((next (evaluate-simple! exprs values (+ index 1) env)))
           (if (< next (vector-length exprs))
               (loop (vector-ref exprs next) env (cons (make-app-frame exprs next values env) stack) ans)
               (apply-values loop env stack values)))))
      ((if-node? ctl)
       (let ((test (if-node-test ctl)))
         (cond ((lit-node? test)
                (loop (if (lit-node-value test) (if-node-then ctl) (if-node-else ctl)) env stack ans))
               ((var-node? test)
                (loop (if (lookup (var-node-name test) env) (if-node-then ctl) (if-node-else ctl)) env stack ans))
               (else
                (loop test env (cons (make-if-frame (if-node-then ctl) (if-node-else ctl) env) stack) ans)))))
      ((var-node? ctl) (produce loop env stack (lookup (var-node-name ctl) env)))
      ((lit-node? ctl) (produce loop env stack (lit-node-value ctl)))
      ((if-frame? ctl)
       (loop (if ans (if-frame-then ctl) (if-frame-else ctl)) (if-frame-env ctl) stack ans))
      ((seq-node? ctl)
       (let ((exprs (seq-node-exprs ctl)))
         (if (null? (cdr exprs))
             (loop (car exprs) env stack ans)
             (loop (car exprs) env (cons (make-seq-frame (cdr exprs) env) stack) ans))))
      ((seq-frame? ctl)
       (let ((exprs (seq-frame-exprs ctl))
             (env (seq-frame-env ctl)))
         (if (null? (cdr exprs))
             (loop (car exprs) env stack ans)
             (loop (car exprs) env (cons (make-seq-frame (cdr exprs) env) stack) ans))))
      ((lambda-node? ctl)
       (produce loop env stack (make-closure (lambda-node-params ctl) (lambda-node-rest ctl)
                                             (lambda-node-body ctl) env (lambda-node-name ctl))))
      ((letrec-node? ctl)
       ;; Every closure is made over the one new frame, as `LetRecNode` makes
       ;; them; no frame is pushed, since making a closure takes no step.
       (let* ((names (letrec-node-names ctl))
              (lambdas (letrec-node-lambdas ctl))
              (inner (make-frame names (make-vector (vector-length names) #f) env)))
         (do ((i 0 (+ i 1))) ((= i (vector-length names)))
           (let ((l (vector-ref lambdas i)))
             (vector-set! (frame-values inner) i
                          (make-closure (lambda-node-params l) (lambda-node-rest l) (lambda-node-body l)
                                        inner (lambda-node-name l)))))
         (loop (letrec-node-body ctl) inner stack ans)))
      ((set-node? ctl)
       (loop (set-node-value ctl) env (cons (make-set-frame (set-node-name ctl) env) stack) ans))
      ((set-frame? ctl)
       (assign! (set-frame-name ctl) ans (set-frame-env ctl))
       (produce loop env stack (if #f #f)))
      ((define-node? ctl)
       (loop (define-node-value ctl) env (cons (make-define-frame (define-node-name ctl)) stack) ans))
      ((define-frame? ctl)
       (hash-table-set! globals (define-frame-name ctl) ans)
       (produce loop env stack (define-frame-name ctl)))
      (else (error "not a node or a frame" ctl)))))

;; ---------------------------------------------------------------------------
;; Running the kernels
;; ---------------------------------------------------------------------------

;; How many times each kernel is run; the best is kept.
(define runs
  (or (string->number (car (reverse (command-line)))) 5))

;; /**
;;  * Milliseconds, as precisely as the host keeps them.
;;  * @returns {number}
;;  */
(define (now) (js-invoke (js-eval "performance") "now"))

;; /**
;;  * A file's forms, as data.
;;  * @param {string} path - The file.
;;  * @returns {list}
;;  */
(define (read-forms path)
  (call-with-input-file path
    (lambda (port)
      (let more ((forms '()))
        (let ((form (read port)))
          (if (eof-object? form) (reverse forms) (more (cons form forms))))))))

;; /**
;;  * A thunk the evaluator made, called by it: its best time over the runs, in
;;  * milliseconds, and its answer.
;;  * @param {closure} thunk - The kernel.
;;  * @returns {pair} (ms . answer)
;;  */
(define (best thunk)
  (let ((call (make-app-node (vector (make-lit-node thunk)))))
    (let more ((i 0) (best-ms #f) (answer #f))
      (if (= i runs)
          (cons best-ms answer)
          (let* ((start (now))
                 (value (execute call #f))
                 (ms (- (now) start)))
            (more (+ i 1) (if (or (not best-ms) (< ms best-ms)) ms best-ms) value))))))

(for-each (lambda (form)
            (let ((core (expand form)))
              (seed-globals! core)
              (execute (analyze core) #f)))
          (read-forms "benchmarks/evaluator/kernels.scm"))

(for-each (lambda (kernel)
            (let ((timed (best (cadr kernel))))
              (display (car kernel)) (display "\t") (display (car timed)) (display "\t")
              (write (equal? (cdr timed) (caddr kernel))) (newline)))
          (global-ref 'kernels))
