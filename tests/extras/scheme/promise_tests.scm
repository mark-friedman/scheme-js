;; Promise Tests
;;
;; Tests for the (scheme-js promise) library.

;; Note: Library is pre-loaded by test runner, but import for documentation
;; (import (scheme-js promise))

;; ============================================================================
;; js-promise? Predicate Tests
;; ============================================================================

(test-group "js-promise? predicate"
  (test "js-promise? returns #f for numbers" #f (js-promise? 42))
  (test "js-promise? returns #f for strings" #f (js-promise? "hello"))
  (test "js-promise? returns #f for lists" #f (js-promise? '(1 2 3)))
  (test "js-promise? returns #t for resolved promise" #t (js-promise? (js-promise-resolve 42)))
  ;; Create rejected promise but attach a catch handler to avoid unhandled rejection
  (let ((p (js-promise-reject "error")))
    (js-promise-catch p (lambda (e) #f))  ;; Handle the rejection
    (test "js-promise? returns #t for rejected promise" #t (js-promise? p))))

;; ============================================================================
;; js-promise-resolve Tests  
;; ============================================================================

(test-group "js-promise-resolve"
  (test "js-promise-resolve returns a Promise" #t (js-promise? (js-promise-resolve 42)))
  (test "js-promise-resolve works with strings" #t (js-promise? (js-promise-resolve "hello")))
  (test "js-promise-resolve works with lists" #t (js-promise? (js-promise-resolve '(1 2 3)))))

;; ============================================================================
;; js-promise-reject Tests
;; ============================================================================

(test-group "js-promise-reject"
  (let ((p (js-promise-reject "error")))
    (js-promise-catch p (lambda (e) #f))  ;; Handle the rejection  
    (test "js-promise-reject returns a Promise" #t (js-promise? p))))

;; ============================================================================
;; make-js-promise Tests
;; ============================================================================

(test-group "make-js-promise"
  ;; Create a promise with a Scheme executor
  (test "make-js-promise returns a Promise" #t
    (js-promise? (make-js-promise (lambda (resolve reject) (resolve 42)))))
  
  ;; Test with an executor that calls resolve
  (let ((p (make-js-promise 
             (lambda (resolve reject)
               (resolve (* 6 7))))))
    (test "make-js-promise with resolve executor" #t (js-promise? p))))

;; ============================================================================
;; js-promise-then Tests
;; ============================================================================

(test-group "js-promise-then"
  ;; js-promise-then returns a new promise
  (let ((p (js-promise-resolve 42)))
    (test "js-promise-then returns a Promise" #t (js-promise? (js-promise-then p (lambda (x) (+ x 1)))))))

;; ============================================================================
;; js-promise-all Tests  
;; ============================================================================

(test-group "js-promise-all"
  (let ((p1 (js-promise-resolve 1))
        (p2 (js-promise-resolve 2))
        (p3 (js-promise-resolve 3)))
    (test "js-promise-all returns a Promise" #t (js-promise? (js-promise-all (list p1 p2 p3))))))

;; ============================================================================
;; js-promise-race Tests
;; ============================================================================

(test-group "js-promise-race"
  (let ((p1 (js-promise-resolve 1))
        (p2 (js-promise-resolve 2)))
    (test "js-promise-race returns a Promise" #t (js-promise? (js-promise-race (list p1 p2))))))

;; ============================================================================
;; js-promise-map Utility Tests
;; ============================================================================

(test-group "js-promise-map"
  (let ((p (js-promise-resolve 5)))
    (test "js-promise-map returns a Promise" #t (js-promise? (js-promise-map (lambda (x) (* x 2)) p)))))

;; ============================================================================
;; js-promise-chain Utility Tests
;; ============================================================================

(test-group "js-promise-chain"
  (let ((p (js-promise-resolve 1)))
    (test "js-promise-chain returns a Promise" #t
      (js-promise? 
        (js-promise-chain p
          (lambda (x) (js-promise-resolve (+ x 1)))
          (lambda (x) (js-promise-resolve (* x 2))))))))

;; ============================================================================
;; Type Error Tests
;; ============================================================================

(test-group "type errors"
  (test-error "js-promise-then rejects non-promise first arg" "must be a Promise"
    (js-promise-then 42 (lambda (x) x)))
  (test-error "js-promise-catch rejects non-promise first arg" "must be a Promise"
    (js-promise-catch 42 (lambda (x) x))))
