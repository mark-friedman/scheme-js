;; Equality Procedures
;; Structural equality testing

;; /**
;;  * Deep equality check.
;;  * Recursively compares pairs and vectors. Uses eqv? for other types.
;;  *
;;  * @param {*} a - First object.
;;  * @param {*} b - Second object.
;;  * @returns {boolean} #t if objects are structurally equal, #f otherwise.
;;  */
(define (equal? a b)
  (cond
    ((eqv? a b) #t)
    ((and (pair? a) (pair? b))
     (and (equal? (car a) (car b))
          (equal? (cdr a) (cdr b))))
    ((and (vector? a) (vector? b))
     (let ((len-a (vector-length a))
           (len-b (vector-length b)))
       (if (= len-a len-b)
           (let loop ((i 0))
             (if (= i len-a)
                 #t
                 (if (equal? (vector-ref a i) (vector-ref b i))
                     (loop (+ i 1))
                     #f)))
           #f)))
    ((and (bytevector? a) (bytevector? b))
     (let ((len-a (bytevector-length a))
           (len-b (bytevector-length b)))
       (if (= len-a len-b)
           (let loop ((i 0))
             (if (= i len-a)
                 #t
                 (if (= (bytevector-u8-ref a i) (bytevector-u8-ref b i))
                     (loop (+ i 1))
                     #f)))
           #f)))
    ((and (string? a) (string? b))
     (eqv? a b)) ; Strings are primitives in JS, so eqv? (Object.is) works. 
    (else #f)))
