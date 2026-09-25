;;; liveness.scm -- which locals are live where a suspended frame will resume.
;;;
;;; When a continuation is captured beneath a compiled procedure, the procedure
;;; saves its locals into a frame and its resumable form restores them later.
;;; Saving every local at every suspension point is quadratic -- a procedure
;;; with n locals and n call sites writes n^2 names -- and measured, frame
;;; literals were once 57% of all generated code in the benchmark corpus. A
;;; frame only needs what can still be read after it resumes, and this works
;;; that out per block of the resumable form, with ordinary backward liveness.
;;;
;;; It runs over the twin's statements rather than over the IR because what a
;;; frame must save includes values the IR has no name for: in
;;; `(list (one) (capturer))` the result of `(one)` sits in a temporary while
;;; `(capturer)` runs, and is live across it. The statements are data
;;; (`emit.scm`), with every local an expression reads marked as a symbol, so
;;; reads and writes are read off them directly.
;;;
;;; Getting it wrong one way loses a value on resume; the other way saves a
;;; name unnecessarily. So where a statement is only conditionally a write --
;;; an assignment inside a guarded jump -- it counts as a read and not a write.

;; /**
;;  * The locals live on entry to each block of a resumable form.
;;  *
;;  * Every symbol a statement names is followed, not only the frame's slots: a
;;  * nested procedure's statements also name its factory's parameters, which
;;  * are never written here, so they are live everywhere and harmless, and the
;;  * caller keeps only the slots when it builds each frame. Testing each read
;;  * against the slot list instead was most of the emitter's time.
;;  *
;;  * @param {list} blocks - Each block's statements, in block order; a block's
;;  *   position is the `$pc` that selects it.
;;  * @returns {vector} For each block, the list of symbols live on entry.
;;  */
(define (live-in blocks)
  (let* ((code (list->vector blocks))
         (n (vector-length code))
         (successors (vector-map (lambda (stmts i)
                                   (filter (lambda (s) (< s n)) (block-successors stmts i)))
                                 code (list->vector (iota n))))
         (live (make-vector n '())))
    ;; Standard backward dataflow. Each set only grows, so comparing sizes
    ;; finds the fixed point, and visiting the blocks last-first makes one pass
    ;; nearly always enough, because the twin mostly jumps forward.
    (let sweep ()
      (let ((changed
             (fold (lambda (i changed)
                     (let ((entry (block-entry (vector-ref code i)
                                               (fold (lambda (s acc) (union (vector-ref live s) acc))
                                                     '() (vector-ref successors i))
                                               live)))
                       (if (> (length entry) (length (vector-ref live i)))
                           (begin (vector-set! live i entry) #t)
                           changed)))
                   #f
                   (reverse (iota n)))))
        (if changed (sweep) live)))))

;; /**
;;  * The union of two sets of symbols: SRFI 1's `lset-union`, specialised to
;;  * `eq?`. This is the innermost operation of the dataflow, run for every
;;  * statement of every block on every sweep, and the general version tests
;;  * membership through a call to its equality per element where `memq` does
;;  * it in one primitive.
;;  * @param {list} a - A set.
;;  * @param {list} b - A set.
;;  * @returns {list} Their union.
;;  */
(define (union a b)
  (fold (lambda (x set) (if (memq x set) set (cons x set))) b a))

;; /**
;;  * The locals live on entry to a block, given those live on its exit.
;;  * @param {list} stmts - The block's statements.
;;  * @param {list} exit - Locals live on exit.
;;  * @param {vector} live - Live-in sets so far, for what a spill reads.
;;  * @returns {list} Locals live on entry.
;;  */
(define (block-entry stmts exit live)
  (fold (lambda (st current)
          (let* ((def (statement-def st))
                 (after (if def (remove (lambda (s) (eq? s def)) current) current))
                 (reads (statement-reads st))
                 (spilled (let ((n (statement-spill st))) (if n (vector-ref live n) '()))))
            (union spilled (union reads after))))
        exit
        (reverse stmts)))

;; /**
;;  * The local a statement certainly writes: the target of an assignment that
;;  * is a local on its own. A write through a box reads the box.
;;  * @param {list} st - A statement.
;;  * @returns {symbol|boolean} The local, or #f.
;;  */
(define (statement-def st)
  (and (eq? (car st) 'assign)
       (let ((target (cadr st)))
         (and (null? (cdr target)) (symbol? (car target)) (car target)))))

;; /**
;;  * The locals a statement reads.
;;  * @param {list} st - A statement.
;;  * @returns {list} The locals, possibly with repeats.
;;  */
(define (statement-reads st)
  (case (car st)
    ((assign) (append (if (statement-def st) '() (expr-locals (cadr st)))
                      (expr-locals (caddr st))))
    ((eval return raw) (expr-locals (cadr st)))
    ((branch) (expr-locals (cadr st)))
    ((spill) (if (cadr st) (expr-locals (cadr st)) '()))
    ;; Conditional, so a write inside it is not certain: everything it names
    ;; counts as read.
    ((guarded) (append (expr-locals (cadr st))
                       (append-map statement-mentions (caddr st))))
    ((tail) (append (expr-locals (cadr st)) (append-map expr-locals (caddr st))))
    (else '())))

;; /**
;;  * Every local a statement names, written or read.
;;  * @param {list} st - A statement.
;;  * @returns {list} The locals.
;;  */
(define (statement-mentions st)
  (case (car st)
    ((assign) (append (expr-locals (cadr st)) (expr-locals (caddr st))))
    (else (statement-reads st))))

;; /**
;;  * The block a statement spills a frame for, whose live-in set it reads:
;;  * the frame is the only way those values reach the code after a capture.
;;  * @param {list} st - A statement.
;;  * @returns {integer|boolean} The resume block, or #f.
;;  */
(define (statement-spill st)
  (and (eq? (car st) 'spill) (caddr st)))

;; /**
;;  * The blocks control can reach after a block: its jumps, and the next block
;;  * if it may fall through -- which it may unless it ends in a return, a tail
;;  * call, which returns whichever way it is made, or an unconditional jump.
;;  * @param {list} stmts - The block's statements.
;;  * @param {integer} index - The block's own number.
;;  * @returns {list} Successor block numbers.
;;  */
(define (block-successors stmts index)
  (let ((jumps (append-map statement-jumps stmts))
        (ends (and (pair? stmts)
                   (memq (car (list-ref stmts (- (length stmts) 1))) '(return goto branch tail)))))
    (delete-duplicates (if ends jumps (append jumps (list (+ index 1)))) eqv?)))

;; /**
;;  * The blocks a statement can jump to.
;;  * @param {list} st - A statement.
;;  * @returns {list} Block numbers.
;;  */
(define (statement-jumps st)
  (case (car st)
    ((goto) (list (cadr st)))
    ((branch) (list (caddr st) (cadddr st)))
    ((guarded) (append-map statement-jumps (caddr st)))
    (else '())))
