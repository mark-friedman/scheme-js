;; messages.scm -- a page's model, updated by a stream of messages.
;;
;; Synthetic, written for `benchmarks/run_tier.js` to stand for the shape of a
;; page's code that the canonical benchmarks lack: the state of a task board
;; changed by messages, each handled by a procedure found in a dispatch
;; table; views of the state computed by selectors that a setup procedure
;; makes once, each remembering its last answer; and an undo history. The
;; messages are made by a pseudo-random generator, the same every run.

(import (scheme base) (scheme write))

;; ---------------------------------------------------------------------------
;; The state
;; ---------------------------------------------------------------------------

(define-record-type task
  (make-task id title column owner points done?)
  task?
  (id task-id)
  (title task-title)
  (column task-column)
  (owner task-owner)
  (points task-points)
  (done? task-done?))

(define-record-type board
  (make-board tasks next-id history version)
  board?
  (tasks board-tasks)
  (next-id board-next-id)
  (history board-history)
  (version board-version))

;; /**
;;  * The board with its tasks replaced, the old board kept for undo.
;;  * @param {board} b - The board.
;;  * @param {list} tasks - The new tasks.
;;  * @param {integer} next-id - The next task's id.
;;  * @returns {board}
;;  */
(define (with-tasks b tasks next-id)
  (make-board tasks next-id (cons b (take-at-most (board-history b) 30)) (+ 1 (board-version b))))

;; /**
;;  * The first n elements of a list, or all of them.
;;  * @param {list} xs - The list.
;;  * @param {integer} n - How many.
;;  * @returns {list}
;;  */
(define (take-at-most xs n)
  (if (or (null? xs) (zero? n)) '() (cons (car xs) (take-at-most (cdr xs) (- n 1)))))

;; /**
;;  * A task with some fields changed.
;;  * @param {task} t - The task.
;;  * @param {symbol} field - Which field.
;;  * @param {*} value - Its new value.
;;  * @returns {task}
;;  */
(define (task-with t field value)
  (make-task (task-id t)
             (if (eq? field 'title) value (task-title t))
             (if (eq? field 'column) value (task-column t))
             (if (eq? field 'owner) value (task-owner t))
             (if (eq? field 'points) value (task-points t))
             (if (eq? field 'done?) value (task-done? t))))

;; /**
;;  * The board with one task changed, if it has that task.
;;  * @param {board} b - The board.
;;  * @param {integer} id - The task.
;;  * @param {procedure} change - From the task to its replacement.
;;  * @returns {board}
;;  */
(define (update-task b id change)
  (with-tasks b
              (map (lambda (t) (if (= (task-id t) id) (change t) t)) (board-tasks b))
              (board-next-id b)))

;; ---------------------------------------------------------------------------
;; The messages, and what each does
;; ---------------------------------------------------------------------------

;; /**
;;  * The board's columns, and the people tasks are assigned to.
;;  */
(define columns #(backlog doing review done))
(define owners #(ana ben chen dana))

;; /**
;;  * The id of one of the board's tasks, picked by a number, or #f.
;;  * @param {board} b - The board.
;;  * @param {integer} k - The number.
;;  * @returns {integer|boolean}
;;  */
(define (some-task-id b k)
  (let ((tasks (board-tasks b)))
    (and (pair? tasks) (task-id (list-ref tasks (modulo k (length tasks)))))))

;; /**
;;  * A new task in the backlog.
;;  * @param {board} b - The board.
;;  * @param {integer} k - The message's argument.
;;  * @returns {board}
;;  */
(define (handle-add b k)
  (let ((id (board-next-id b)))
    (with-tasks b
                (cons (make-task id (string-append "task " (number->string id)) 'backlog
                                 (vector-ref owners (modulo k 4)) (+ 1 (modulo k 8)) #f)
                      (board-tasks b))
                (+ id 1))))

;; /**
;;  * A task moved to another column.
;;  * @param {board} b - The board.
;;  * @param {integer} k - The message's argument.
;;  * @returns {board}
;;  */
(define (handle-move b k)
  (let ((id (some-task-id b k)))
    (if id (update-task b id (lambda (t) (task-with t 'column (vector-ref columns (modulo k 4))))) b)))

;; /**
;;  * A task given to someone else.
;;  * @param {board} b - The board.
;;  * @param {integer} k - The message's argument.
;;  * @returns {board}
;;  */
(define (handle-assign b k)
  (let ((id (some-task-id b k)))
    (if id (update-task b id (lambda (t) (task-with t 'owner (vector-ref owners (modulo (quotient k 4) 4))))) b)))

;; /**
;;  * A task's points changed.
;;  * @param {board} b - The board.
;;  * @param {integer} k - The message's argument.
;;  * @returns {board}
;;  */
(define (handle-estimate b k)
  (let ((id (some-task-id b k)))
    (if id (update-task b id (lambda (t) (task-with t 'points (+ 1 (modulo k 13))))) b)))

;; /**
;;  * A task done.
;;  * @param {board} b - The board.
;;  * @param {integer} k - The message's argument.
;;  * @returns {board}
;;  */
(define (handle-complete b k)
  (let ((id (some-task-id b k)))
    (if id (update-task b id (lambda (t) (task-with (task-with t 'done? #t) 'column 'done))) b)))

;; /**
;;  * A task renamed.
;;  * @param {board} b - The board.
;;  * @param {integer} k - The message's argument.
;;  * @returns {board}
;;  */
(define (handle-rename b k)
  (let ((id (some-task-id b k)))
    (if id (update-task b id (lambda (t) (task-with t 'title (string-append (task-title t) "!")))) b)))

;; /**
;;  * A task deleted, while the board has more than ten.
;;  * @param {board} b - The board.
;;  * @param {integer} k - The message's argument.
;;  * @returns {board}
;;  */
(define (handle-delete b k)
  (let ((id (some-task-id b k)))
    (if (and id (> (length (board-tasks b)) 10))
        (with-tasks b (filter-tasks (lambda (t) (not (= (task-id t) id))) (board-tasks b)) (board-next-id b))
        b)))

;; /**
;;  * The board as it was before the last change.
;;  * @param {board} b - The board.
;;  * @param {integer} k - The message's argument.
;;  * @returns {board}
;;  */
(define (handle-undo b k)
  (if (pair? (board-history b)) (car (board-history b)) b))

;; /**
;;  * The tasks that pass a test.
;;  * @param {procedure} keep? - The test.
;;  * @param {list} tasks - The tasks.
;;  * @returns {list}
;;  */
(define (filter-tasks keep? tasks)
  (let loop ((ts tasks) (acc '()))
    (cond ((null? ts) (reverse acc))
          ((keep? (car ts)) (loop (cdr ts) (cons (car ts) acc)))
          (else (loop (cdr ts) acc)))))

;; /**
;;  * Each message's handler.
;;  */
(define handlers
  (list (cons 'add handle-add) (cons 'move handle-move) (cons 'assign handle-assign)
        (cons 'estimate handle-estimate) (cons 'complete handle-complete)
        (cons 'rename handle-rename) (cons 'delete handle-delete) (cons 'undo handle-undo)))

;; /**
;;  * The board after one message.
;;  * @param {board} b - The board.
;;  * @param {symbol} kind - The message.
;;  * @param {integer} k - Its argument.
;;  * @returns {board}
;;  */
(define (dispatch b kind k)
  ((cdr (assq kind handlers)) b k))

;; ---------------------------------------------------------------------------
;; Views of the state: selectors, made once
;; ---------------------------------------------------------------------------

;; /**
;;  * A selector: a view computed from the board, computed again only when the
;;  * board has changed since it last was.
;;  * @param {procedure} compute - From a board to the view.
;;  * @returns {procedure} From a board to the view.
;;  */
(define (make-selector compute)
  (let ((version -1) (last #f))
    (lambda (b)
      (if (not (= version (board-version b)))
          (begin (set! last (compute b)) (set! version (board-version b))))
      last)))

;; /**
;;  * The page's views, made as it starts.
;;  * @returns {list} The selectors, by name.
;;  */
(define (make-views)
  (list
    (cons 'per-column
          (make-selector
            (lambda (b)
              (map (lambda (c) (length (filter-tasks (lambda (t) (eq? (task-column t) c)) (board-tasks b))))
                   (vector->list columns)))))
    (cons 'points-per-owner
          (make-selector
            (lambda (b)
              (map (lambda (o)
                     (let loop ((ts (board-tasks b)) (sum 0))
                       (cond ((null? ts) sum)
                             ((eq? (task-owner (car ts)) o) (loop (cdr ts) (+ sum (task-points (car ts)))))
                             (else (loop (cdr ts) sum)))))
                   (vector->list owners)))))
    (cons 'done (make-selector (lambda (b) (length (filter-tasks task-done? (board-tasks b))))))))

;; ---------------------------------------------------------------------------
;; The run
;; ---------------------------------------------------------------------------

;; /**
;;  * The next number of a linear congruential generator.
;;  * @param {integer} seed - The last number.
;;  * @returns {integer}
;;  */
(define (next-random seed) (modulo (+ (* seed 1103515245) 12345) 2147483648))

;; /**
;;  * The messages, as often as each comes: as many deletions as additions, so
;;  * that the board stays the size of one a person keeps.
;;  */
(define message-kinds #(add add move move assign estimate complete rename delete delete undo))

;; /**
;;  * Runs the messages through the board, reading a view after each as a page
;;  * repainting would.
;;  * @param {integer} n - How many messages.
;;  * @param {list} views - The selectors.
;;  * @returns {board} The last board.
;;  */
(define (run-messages n views)
  (let loop ((i 0) (seed 7) (b (make-board '() 0 '() 0)))
    (if (= i n)
        b
        (let* ((seed (next-random seed))
               (kind (vector-ref message-kinds (modulo (quotient seed 65536) (vector-length message-kinds))))
               (b (dispatch b kind (quotient seed 256))))
          ((cdr (list-ref views (modulo i 3))) b)
          (loop (+ i 1) seed b)))))

(define views (make-views))
(define final (run-messages 4000 views))
(display (list 'messages (length (board-tasks final))
               (map (lambda (v) ((cdr v) final)) views)))
(newline)
