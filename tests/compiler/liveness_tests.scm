;; Liveness for frame spills (src/compiler/liveness.scm)
;;
;; Runs in the compiler's own environment, where `live-in` is defined. Blocks
;; are lists of statements in the emitter's data form (see emit.scm): locals
;; are symbols, and everything else in an expression is text.

;; /**
;;  * The names live on entry to a block, sorted, as one string.
;;  * @param {list} blocks - The blocks.
;;  * @param {integer} i - Which block.
;;  * @returns {string} The names, comma-separated.
;;  */
(define (live-at blocks i)
  (define (insert s sorted)
    (cond ((null? sorted) (list s))
          ((string<? s (car sorted)) (cons s sorted))
          (else (cons (car sorted) (insert s (cdr sorted))))))
  (string-join (fold insert '() (map symbol->string (vector-ref (live-in blocks) i))) ","))

(test-group "liveness - within a block"
  (test "a read makes a variable live" "a"
        (live-at '(((return (a)))) 0))
  (test "an assignment ends liveness above it" ""
        (live-at '(((assign (x) ("1")) (return (x)))) 0))
  (test "the value assigned is read before the assignment happens" "b"
        (live-at '(((assign (x) ("f(" b ")")) (return (x)))) 0))
  (test "reading and assigning in one statement keeps it live" "x"
        (live-at '(((assign (x) ("g(" x ")")) (return (x)))) 0))
  ;; An assigned Scheme local is held in a one-element array, and `x[0] = v`
  ;; needs `x` -- treating it as a definition would drop the box from a frame.
  (test "writing through a box is a read of the box" "x"
        (live-at '(((assign (x "[0]") ("5")) (return ("1")))) 0))
  (test "text in an expression is not a local" ""
        (live-at '(((return ("R.step(K[0], E)")))) 0))
  ;; Locals are marked in the data, so the name of one inside a string literal
  ;; is only text -- where scanning the emitted text used to count it as read.
  (test "a local's name inside a string literal is not a read" ""
        (live-at '(((return ("\"a\"")))) 0)))

(test-group "liveness - across blocks"
  (test "both branches of a jump contribute" "a,b,c"
        (live-at '(((branch (c) 1 2)) ((return (a))) ((return (b)))) 0))
  (test "a return ends the block" "a"
        (live-at '(((return (a))) ((return (b)))) 0))
  (test "a block without a jump falls through" "z"
        (live-at '(((assign (y) ("h()"))) ((return (y " + " z)))) 0))
  (test "a statement that may return does not end the block" "a,b"
        (live-at '(((spill (a) 1)) ((return (b)))) 0))
  ;; A variable defined on one branch and not the other is still live above
  ;; the branch, because the other path may reach the read.
  (test "a definition on only one branch does not kill" "c,x"
        (live-at '(((branch (c) 1 2))
                   ((assign (x) ("1")) (goto 3))
                   ((goto 3))
                   ((return (x))))
                 0))
  (test "a cycle reaches a fixed point" "c,y"
        (live-at '(((goto 1))
                   ((assign (x) (y)) (branch (c) 0 2))
                   ((return (x))))
                 0))
  (test "an assignment inside a guarded jump is not a definition" "c,x"
        (live-at '(((guarded (c) ((assign (x) ("1")) (goto 1))) (return (x))) ((return ("0")))) 0))
  (test "a tail call reads its callee and its arguments" "a,b,f"
        (live-at '(((tail (f) ((a) (b " + 1"))))) 0))
  (test "and ends the block, since it returns either way" "f"
        (live-at '(((tail (f) ())) ((return (z)))) 0)))

;; The frame saved at a suspension point is exactly what is live at the block
;; it resumes at. A capture has no ordinary edge to that block -- it spills and
;; returns -- so the spill *reads* the block's live set. Get this wrong and a
;; variable read only after a capture is judged dead before it: resume at an
;; earlier call site, run forward into the capture, and it spills `undefined`.
(test-group "liveness - the spill"
  (let ((assigned-first '(((assign (x) ("1")) (assign (y) ("2")) (spill #f 1) (return ("R.UNWIND")))
                          ((assign ($t1) ("$r")) (return (x)))))
        (not-assigned '(((assign (y) ("2")) (spill #f 1) (return ("R.UNWIND")))
                        ((assign ($t1) ("$r")) (return (x))))))
    (test "a spill needs what its resume block reads" "x" (live-at assigned-first 1))
    (test "a variable set before the spill is not live above it" "" (live-at assigned-first 0))
    (test "a variable read only after a capture is live before it" "x" (live-at not-assigned 0))))
