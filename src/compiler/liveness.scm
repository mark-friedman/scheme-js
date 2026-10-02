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

;; ---------------------------------------------------------------------------
;; Sets of locals, as bits
;; ---------------------------------------------------------------------------

;; Each set is an exact integer with a bit for each local in it (SRFI 151), so
;; that a union, or what is left once a local is written, is one operation
;; whatever the sets' sizes. As lists, those were most of the analysis: a large
;; procedure has hundreds of locals live across its call sites, and a list's
;; union tests each member of one set against the whole of the other.

;; /**
;;  * The bit each local of one analysis has, numbered as they are met.
;;  * @property {weak-table} bits - Each local's bit.
;;  * @property {integer} count - How many locals are numbered.
;;  * @property {list} locals - The locals, the last numbered first.
;;  */
(define-record-type numbering
  (make-numbering bits count locals)
  numbering?
  (bits numbering-bits)
  (count numbering-count set-numbering-count!)
  (locals numbering-locals set-numbering-locals!))

;; /**
;;  * A local's bit, numbering it if it has none yet.
;;  * @param {numbering} numbering - The analysis's numbering.
;;  * @param {symbol} local - The local.
;;  * @returns {integer} Its bit's index.
;;  */
(define (local-bit numbering local)
  (or (weak-table-ref (numbering-bits numbering) local)
      (let ((k (numbering-count numbering)))
        (weak-table-set! (numbering-bits numbering) local k)
        (set-numbering-count! numbering (+ k 1))
        (set-numbering-locals! numbering (cons local (numbering-locals numbering)))
        k)))

;; /**
;;  * The set of some locals.
;;  * @param {numbering} numbering - The analysis's numbering.
;;  * @param {list} locals - The locals, possibly with repeats.
;;  * @returns {integer} The set.
;;  */
(define (set-of numbering locals)
  (fold (lambda (local set) (bitwise-ior set (arithmetic-shift 1 (local-bit numbering local))))
        0 locals))

;; /**
;;  * What a statement does to the set of live locals, going backwards -- its
;;  * transfer function: the
;;  * locals still live above it are those live below it that it does not
;;  * write, with those it reads, and with those live where it spills a frame.
;;  * @property {integer} keeps - Every local but the one it writes.
;;  * @property {integer} reads - The locals it reads.
;;  * @property {integer|boolean} spill - The block it spills a frame for, or #f.
;;  */
(define-record-type transfer
  (make-transfer keeps reads spill)
  transfer?
  (keeps transfer-keeps)
  (reads transfer-reads)
  (spill transfer-spill))

;; /**
;;  * A statement's transfer.
;;  * @param {list} st - The statement.
;;  * @param {numbering} numbering - The analysis's numbering.
;;  * @returns {transfer}
;;  */
(define (statement-transfer st numbering)
  (let ((def (statement-def st)))
    (make-transfer (if def (bitwise-not (arithmetic-shift 1 (local-bit numbering def))) -1)
                   (set-of numbering (statement-reads st))
                   (statement-spill st))))

;; ---------------------------------------------------------------------------
;; The analysis
;; ---------------------------------------------------------------------------

;; /**
;;  * What the analysis found: the locals live on entry to each block.
;;  * @property {numbering} numbering - Each local's bit.
;;  * @property {vector} live - For each block, the set live on entry.
;;  */
(define-record-type liveness
  (make-liveness numbering live)
  liveness?
  (numbering liveness-numbering)
  (live liveness-live))

;; /**
;;  * The locals live on entry to each block of a resumable form.
;;  *
;;  * Every symbol a statement names is followed, not only the frame's slots: a
;;  * nested procedure's statements also name its factory's parameters, which
;;  * are never written here, so they are live everywhere and harmless, and the
;;  * caller asks only about the slots when it builds each frame (`live-among`).
;;  *
;;  * @param {list} blocks - Each block's statements, in block order; a block's
;;  *   position is the `$pc` that selects it.
;;  * @returns {liveness} What is live on entry to each block.
;;  */
(define (live-in blocks)
  (let* ((numbering (make-numbering (make-weak-table) 0 '()))
         ;; Each block's transfers, last statement first, for going backwards.
         (code (list->vector
                 (map (lambda (stmts)
                        (reverse (map (lambda (st) (statement-transfer st numbering)) stmts)))
                      blocks)))
         (n (vector-length code))
         (successors (list->vector
                       (map (lambda (stmts i)
                              (filter (lambda (s) (< s n)) (block-successors stmts i)))
                            blocks (iota n))))
         (live (make-vector n 0)))
    ;; Standard backward dataflow. Each set only grows, so one that has not
    ;; changed is at its fixed point, and visiting the blocks last-first makes
    ;; one pass nearly always enough, because the twin mostly jumps forward.
    (let sweep ()
      (let ((changed
             (fold (lambda (i changed)
                     (let ((entry (block-entry (vector-ref code i)
                                               (fold (lambda (s set) (bitwise-ior (vector-ref live s) set))
                                                     0 (vector-ref successors i))
                                               live)))
                       (if (= entry (vector-ref live i))
                           changed
                           (begin (vector-set! live i entry) #t))))
                   #f
                   (reverse (iota n)))))
        (if changed (sweep) (make-liveness numbering live))))))

;; /**
;;  * The locals live on entry to a block, given those live on its exit.
;;  * @param {list} transfers - The block's transfers, last statement first.
;;  * @param {integer} exit - Locals live on exit.
;;  * @param {vector} live - Live-in sets so far, for what a spill reads.
;;  * @returns {integer} Locals live on entry.
;;  */
(define (block-entry transfers exit live)
  (fold (lambda (transfer current)
          (bitwise-ior (bitwise-and current (transfer-keeps transfer))
                       (transfer-reads transfer)
                       (let ((n (transfer-spill transfer))) (if n (vector-ref live n) 0))))
        exit
        transfers))

;; /**
;;  * Those of some locals that are live on entry to a block, in their order.
;;  * @param {liveness} liveness - The analysis.
;;  * @param {integer} block - The block.
;;  * @param {list} locals - The locals asked about.
;;  * @returns {list} The live ones.
;;  */
(define (live-among liveness block locals)
  (let ((set (vector-ref (liveness-live liveness) block))
        (bits (numbering-bits (liveness-numbering liveness))))
    (filter (lambda (local)
              (let ((bit (weak-table-ref bits local)))
                (and bit (bit-set? bit set))))
            locals)))

;; /**
;;  * Every local live on entry to a block, in the order they were numbered.
;;  * @param {liveness} liveness - The analysis.
;;  * @param {integer} block - The block.
;;  * @returns {list} The live locals.
;;  */
(define (live-locals liveness block)
  (live-among liveness block (reverse (numbering-locals (liveness-numbering liveness)))))

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
