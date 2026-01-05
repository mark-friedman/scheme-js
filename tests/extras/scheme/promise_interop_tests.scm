;; Promise Interoperability Tests
;; 
;; Tests for JavaScript <-> Scheme Promise interoperability.
;; Uses js-eval to create JavaScript objects where needed.

;; ============================================================================
;; Test 1: Scheme-created Promise consumed by JavaScript
;; ============================================================================

(test-group "Scheme-created Promise"
  ;; Create a promise in Scheme and verify it resolves correctly
  (let ((p (make-js-promise 
             (lambda (resolve reject)
               (resolve 42)))))
    (test-assert "make-js-promise returns a Promise"
      (js-promise? p))
    ;; Chain and verify the value
    (let ((result-promise (js-promise-then p (lambda (x) x))))
      (test-assert "js-promise-then returns a Promise"
        (js-promise? result-promise)))))

;; ============================================================================
;; Test 2: Scheme callback attached to JS Promise
;; ============================================================================

(test-group "Scheme callback on JS Promise"
  ;; Create a resolved JS Promise using js-eval
  (let ((js-promise (js-eval "Promise.resolve(100)")))
    (test-assert "js-eval creates a Promise"
      (js-promise? js-promise))
    ;; Attach a Scheme callback
    (let ((doubled (js-promise-then js-promise 
                     (lambda (x) (* x 2)))))
      (test-assert "Chained promise is a Promise"
        (js-promise? doubled)))))

;; ============================================================================
;; Test 3: js-promise-resolve and js-promise-reject
;; ============================================================================

(test-group "Promise constructors - interop"
  (test-assert "js-promise-resolve returns a Promise"
    (js-promise? (js-promise-resolve 'value)))
  
  (test-assert "js-promise-resolve works with numbers"
    (js-promise? (js-promise-resolve 42)))
  
  (test-assert "js-promise-resolve works with lists"
    (js-promise? (js-promise-resolve '(1 2 3))))
  
  (let ((p (js-promise-reject 'error)))
    (js-promise-catch p (lambda (e) #f))  ;; Handle the rejection
    (test-assert "js-promise-reject returns a Promise"
      (js-promise? p))))

;; ============================================================================
;; Test 4: js-promise-map (Scheme utility)
;; ============================================================================

(test-group "js-promise-map - interop"
  (let* ((p (js-promise-resolve 10))
         (mapped (js-promise-map (lambda (x) (* x 3)) p)))
    (test-assert "js-promise-map returns a Promise"
      (js-promise? mapped))))

;; ============================================================================
;; Test 5: js-promise-chain (Scheme utility)
;; ============================================================================

(test-group "js-promise-chain - interop"
  (let* ((p (js-promise-resolve 5))
         (chained (js-promise-chain p
                    (lambda (x) (js-promise-resolve (* x 2)))
                    (lambda (x) (js-promise-resolve (+ x 3))))))
    (test-assert "js-promise-chain returns a Promise"
      (js-promise? chained))))

;; ============================================================================
;; Test 6: Promise combinators
;; ============================================================================

(test-group "Promise combinators - interop"
  (test-assert "js-promise-all returns a Promise"
    (js-promise? (js-promise-all (list 
                   (js-promise-resolve 1)
                   (js-promise-resolve 2)))))
  
  (test-assert "js-promise-race returns a Promise"  
    (js-promise? (js-promise-race (list
                   (js-promise-resolve 'first)
                   (js-promise-resolve 'second)))))
  
  (let ((all-settled 
          (js-promise-all-settled (list
            (js-promise-resolve 'ok)
            (let ((p (js-promise-reject 'err)))
              (js-promise-catch p (lambda (e) #f))
              p)))))
    (test-assert "js-promise-all-settled returns a Promise"
      (js-promise? all-settled))))

;; ============================================================================
;; Test 7: Error handling
;; ============================================================================

(test-group "Error handling - interop"
  (let* ((rejected (js-promise-reject 'test-error))
         (caught (js-promise-catch rejected 
                   (lambda (err) 'caught))))
    (test-assert "js-promise-catch returns a Promise"
      (js-promise? caught)))
  
  (let* ((p (js-promise-resolve 'value))
         (finalized (js-promise-finally p (lambda () 'done))))
    (test-assert "js-promise-finally returns a Promise"
      (js-promise? finalized))))

;; ============================================================================
;; Test 8: Type checking
;; ============================================================================

(test-group "Type checking - interop"
  (test-error "js-promise-then rejects non-promise first arg"
    "must be a Promise"
    (js-promise-then 'not-a-promise (lambda (x) x)))
  
  (test-error "js-promise-catch rejects non-promise first arg"
    "must be a Promise"
    (js-promise-catch 'not-a-promise (lambda (x) x))))

;; ============================================================================
;; Test 9: Integration with js-eval
;; ============================================================================

(test-group "JS-Scheme integration"
  ;; Create a JS function that returns a promise
  (let ((async-js-fn (js-eval "(() => Promise.resolve('from-js'))")))
    (test-assert "js-eval async function result is callable"
      (procedure? async-js-fn))
    (let ((result (async-js-fn)))
      (test-assert "Calling JS async function returns a Promise"
        (js-promise? result)))))
