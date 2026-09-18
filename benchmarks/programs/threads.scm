;; threads.scm -- round-robin coroutine scheduler built on call/cc.
;;
;; CONTINUATION BENCHMARK. Each context switch captures the current continuation,
;; queues it, and invokes a previously queued one -- so continuations are both
;; stored for later and resumed out of order. This is the closest thing in the
;; suite to how continuations are used in real programs (generators, green
;; threads, async), and it is the case a one-shot-only design cannot support.
;;
;; `bench-size` is the number of threads; each yields `rounds-per-thread` times.

(define ready-queue '())
(define scheduler-exit #f)

;; /**
;;  * Adds a thunk to the back of the ready queue.
;;  * @param {procedure} thunk - The suspended computation to enqueue.
;;  * @returns {unspecified}
;;  */
(define (enqueue! thunk)
  (set! ready-queue (append ready-queue (list thunk))))

;; /**
;;  * Runs the next ready thunk, or exits the scheduler if none remain.
;;  * @returns {*} Does not return normally.
;;  */
(define (dispatch)
  (if (null? ready-queue)
      (scheduler-exit 'done)
      (let ((next (car ready-queue)))
        (set! ready-queue (cdr ready-queue))
        (next))))

;; /**
;;  * Suspends the current thread, re-queuing it, and dispatches another.
;;  * @returns {unspecified} Returns when the thread is later resumed.
;;  */
(define (yield)
  (call/cc
    (lambda (k)
      (enqueue! (lambda () (k 'resumed)))
      (dispatch))))

;; /**
;;  * Spawns a thread running `thunk`, which dispatches on completion.
;;  * @param {procedure} thunk - The thread body.
;;  * @returns {unspecified}
;;  */
(define (spawn thunk)
  (enqueue! (lambda () (thunk) (dispatch))))

;; /**
;;  * Runs `n` threads to completion, each yielding `rounds` times.
;;  * @param {exact-integer} n - Number of threads.
;;  * @param {exact-integer} rounds - Yields per thread.
;;  * @returns {exact-integer} Total number of yields performed.
;;  */
(define (run-threads n rounds)
  (let ((counter 0))
    (set! ready-queue '())
    (call/cc
      (lambda (exit)
        (set! scheduler-exit exit)
        (let spawn-loop ((i 0))
          ;; One-armed `if` is legal R7RS but rejected by Racket, so the
          ;; portable spelling `when` is used throughout this suite.
          (when (< i n)
            (spawn (lambda ()
                     (let yield-loop ((j 0))
                       (when (< j rounds)
                         (set! counter (+ counter 1))
                         (yield)
                         (yield-loop (+ j 1))))))
            (spawn-loop (+ i 1))))
        (dispatch)))
    counter))

(define (bench-run) (run-threads bench-size 10))
