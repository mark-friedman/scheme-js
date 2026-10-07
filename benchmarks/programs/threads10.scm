;; threads10.scm -- a process scheduler built on call/cc, from Thivierge & Feeley.
;;
;; CONTINUATION BENCHMARK. The program is `threads10` from Thivierge & Feeley,
;; *Efficient Compilation of Tail Calls and Continuations to JavaScript*
;; (SFP 2012), their Figure 15, transcribed from the paper by hand. Ten threads
;; each yield `bench-size` times; every yield captures the running thread's
;; continuation, queues the thread at the back of a ready queue, and resumes the
;; thread at the front. At the paper's size, 100000, that is a million context
;; switches, each one capture and one resume, so the figure is the steady-state
;; cost of a switch. A captured continuation is only a few frames deep (`wait`,
;; the thread's body, `boot`), so the depth of the stack plays no part.
;;
;; Each yield's continuation is resumed once, but `graft`, captured once by
;; `boot`, is re-entered for every thread started and once more at the end, so
;; the program needs continuations that can be invoked more than once.
;;
;; `benchmarks/programs/threads.scm` was written for this suite from the paper's
;; description before this source was found. This one differs from it in scale
;; (a million switches against four thousand) and in its queue: a doubly-linked
;; ring of vectors updated in place, where `threads.scm` appends to a list.
;;
;; The paper's procedures are unchanged except where the suite's portability
;; requires it:
;;
;;  - one-armed `if` is written `when`, which Racket requires;
;;  - `threads` takes the number of yields per thread, the paper's constant
;;    100000, so that `bench-size` can scale the run;
;;  - `threads` returns how many threads ran to their end, which the paper's
;;    does not, so that a scheduler that loses or repeats a thread gives a
;;    wrong answer rather than a slow one;
;;  - the driver is `bench-run`, as the suite's other programs have, in place of
;;    the paper's `run-benchmark`.

;; Queues.
;;
;; A queue is a ring of vectors, doubly linked through slot 0 (next) and slot 1
;; (prev), with the queue itself as the ring's header. A node linked to itself
;; is an empty queue, or an element in no queue.

;; /**
;;  * The node after `q` in its ring.
;;  * @param {vector} q - A node.
;;  * @returns {vector}
;;  */
(define (next q) (vector-ref q 0))

;; /**
;;  * The node before `q` in its ring.
;;  * @param {vector} q - A node.
;;  * @returns {vector}
;;  */
(define (prev q) (vector-ref q 1))

;; /**
;;  * Sets the node after `q`.
;;  * @param {vector} q - A node.
;;  * @param {vector} x - Its new successor.
;;  */
(define (next-set! q x) (vector-set! q 0 x))

;; /**
;;  * Sets the node before `q`.
;;  * @param {vector} q - A node.
;;  * @param {vector} x - Its new predecessor.
;;  */
(define (prev-set! q x) (vector-set! q 1 x))

;; /**
;;  * Whether a queue holds nothing: its header is linked to itself.
;;  * @param {vector} q - A queue.
;;  * @returns {boolean}
;;  */
(define (empty? q) (eq? q (next q)))

;; /**
;;  * A new, empty queue.
;;  * @returns {vector}
;;  */
(define (queue) (init (vector #f #f)))

;; /**
;;  * Links a node to itself, making it an empty queue or an element in none.
;;  * @param {vector} q - A node.
;;  * @returns {vector} The node.
;;  */
(define (init q)
  (next-set! q q)
  (prev-set! q q)
  q)

;; /**
;;  * Unlinks a node from its queue.
;;  * @param {vector} x - A node in a queue.
;;  * @returns {vector} The node, now linked to itself.
;;  */
(define (deq x)
  (let ((n (next x)) (p (prev x)))
    (next-set! p n)
    (prev-set! n p)
    (init x)))

;; /**
;;  * Links a node in at the back of a queue.
;;  * @param {vector} q - The queue.
;;  * @param {vector} x - A node in no queue.
;;  * @returns {vector} The node.
;;  */
(define (enq q x)
  (let ((p (prev q)))
    (next-set! p x)
    (next-set! x q)
    (prev-set! q x)
    (prev-set! x p)
    x))

;; Process scheduler.
;;
;; A process is a queue node with a third slot, the continuation that resumes
;; it. Every thread runs on top of `graft`, the continuation `boot` captures:
;; calling `(graft thunk)` abandons the stack above `boot` and runs `thunk` in
;; its place, which is how a thread starts on a fresh stack and how the
;; scheduler, once the queue is empty, returns from `boot`.

;; /**
;;  * Runs the ready threads until none are left, starting each above the
;;  * continuation it captures as `graft`.
;;  * @returns {*} The value of the last thunk grafted, #f.
;;  */
(define (boot)
  ((call/cc
     (lambda (k)
       (set! graft k)
       (schedule)))))

(define graft #f)
(define current #f)
(define readyq (queue))

;; /**
;;  * A new process, in no queue.
;;  * @param {procedure} cont - What resumes it, given one argument it ignores.
;;  * @returns {vector}
;;  */
(define (process cont)
  (init (vector #f #f cont)))

;; /**
;;  * What resumes a process.
;;  * @param {vector} p - A process.
;;  * @returns {procedure}
;;  */
(define (cont p) (vector-ref p 2))

;; /**
;;  * Sets what resumes a process.
;;  * @param {vector} p - A process.
;;  * @param {procedure} x - A continuation, or a procedure of one argument.
;;  */
(define (cont-set! p x) (vector-set! p 2 x))

;; /**
;;  * Queues a thread that runs `thunk` and then ends; it starts by grafting the
;;  * thunk onto `boot`'s stack.
;;  * @param {procedure} thunk - The thread's body.
;;  * @returns {vector} The thread's process.
;;  */
(define (spawn thunk)
  (enq readyq
    (process (lambda (r)
               (graft (lambda ()
                        (end (thunk))))))))

;; /**
;;  * Resumes the thread at the front of the ready queue, or, if there is none,
;;  * returns #f from `boot`.
;;  * @returns {*} Does not return.
;;  */
(define (schedule)
  (if (empty? readyq)
    (graft (lambda () #f))
    (let ((p (deq (next readyq))))
      (set! current p)
      ((cont p) #f))))

;; /**
;;  * Ends the running thread and resumes the next.
;;  * @param {*} result - The thread's value, which is dropped.
;;  * @returns {*} Does not return.
;;  */
(define (end result) (schedule))

;; /**
;;  * The context switch: saves the running thread's continuation, queues the
;;  * thread at the back, and resumes the thread at the front.
;;  * @returns {*} Returns when the thread is next resumed.
;;  */
(define (yield)
  (call/cc
    (lambda (k)
      (cont-set! current k)
      (enq readyq current)
      (schedule))))

;; /**
;;  * Yields `x` times.
;;  * @param {exact-integer} x - How many times.
;;  */
(define (wait x)
  (when (> x 0)
    (yield)
    (wait (- x 1))))

;; The number of threads that have run to their end.
(define finished 0)

;; /**
;;  * Runs `n` threads, each yielding `rounds` times, to completion.
;;  * @param {exact-integer} n - How many threads.
;;  * @param {exact-integer} rounds - How many times each yields.
;;  * @returns {exact-integer} How many threads ran to their end: `n`.
;;  */
(define (threads n rounds)
  (set! finished 0)
  (let loop ((n n))
    (when (> n 0)
      (spawn (lambda ()
               (wait rounds)
               (set! finished (+ finished 1))))
      (loop (- n 1))))
  (boot)
  finished)

(define (bench-run) (threads 10 bench-size))
