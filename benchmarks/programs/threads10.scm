;; Queues.

(define (next q) (vector-ref q 0))
(define (prev q) (vector-ref q 1))
(define (next-set! q x) (vector-set! q 0 x))
(define (prev-set! q x) (vector-set! q 1 x))

(define (empty? q) (eq? q (next q)))

(define (queue) (init (vector #f #f)))

(define (init q)
  (next-set! q q)
  (prev-set! q q)
  q)

(define (deq x)
  (let ((n (next x)) (p (prev x)))
    (next-set! p n)
    (prev-set! n p)
    (init x)))

(define (enq q x)
  (let ((p (prev q)))
    (next-set! p x)
    (next-set! x q)
    (prev-set! q x)
    (prev-set! x p)
    x))

;; Process scheduler.

(define (boot)
  ((call/cc
     (lambda (k)
       (set! graft k)
       (schedule)))))

(define graft #f)
(define current #f)
(define readyq (queue))

(define (process cont)
  (init (vector #f #f cont)))

(define (cont p) (vector-ref p 2))
(define (cont-set! p x) (vector-set! p 2 x))

(define (spawn thunk)
  (enq readyq
    (process (lambda (r)
               (graft (lambda ()
                        (end (thunk))))))))

(define (schedule)
  (if (empty? readyq)
    (graft (lambda () #f))
    (let ((p (deq (next readyq))))
      (set! current p)
      ((cont p) #f))))

(define (end result) (schedule))

(define (yield)
  (call/cc
    (lambda (k)
      (cont-set! current k)
      (enq readyq current)
      (schedule))))

(define (wait x)
  (if (> x 0)
    (begin
      (yield)
      (wait (- x 1)))))

(define (threads n)
  (let loop ((n n))
    (if (> n 0)
      (begin
        (spawn (lambda () (wait 100000)))
        (loop (- n 1)))))
  (boot))

(run-benchmark
  "threads10"
  (lambda () (threads 10)))
