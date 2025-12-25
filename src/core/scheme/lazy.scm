;; (scheme lazy) library
;;
;; R7RS lazy evaluation (delay/force) implementation.
;;
;; A promise is a pair: (forced? . value-or-thunk)
;; - When not forced: (#f . thunk)
;; - When forced: (#t . value)

;; /**
;;  * promise? - Check if an object is a promise.
;;  * @param {*} obj - Object to check.
;;  * @returns {boolean} True if obj is a promise.
;;  */
(define (promise? obj)
  (and (pair? obj)
       (pair? (car obj))
       (eq? (caar obj) 'promise-tag)))

;; Internal promise constructor
(define (make-promise-internal thunk)
  (cons (cons 'promise-tag #f) thunk))

;; /**
;;  * make-promise - Wrap a value in an already-forced promise.
;;  * @param {*} obj - Value to wrap.
;;  * @returns {promise} A promise that yields obj.
;;  */
(define (make-promise obj)
  (if (promise? obj)
      obj
      (cons (cons 'promise-tag #t) obj)))

;; /**
;;  * force - Force a promise and return its value.
;;  * @param {promise} promise - The promise to force.
;;  * @returns {*} The forced value.
;;  */
(define (force promise)
  (if (not (promise? promise))
      (error "force: expected promise" promise))
  (if (cdar promise)  ; already forced?
      (cdr promise)   ; return cached value
      (let ((result ((cdr promise))))  ; call thunk
        (if (cdar promise)  ; check again (thunk may have forced recursively)
            (cdr promise)
            (begin
              (set-cdr! (car promise) #t)  ; mark as forced
              (set-cdr! promise result)     ; cache value
              result)))))
