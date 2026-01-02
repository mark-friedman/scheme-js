;; JavaScript Promise Interoperability - Scheme Implementation
;;
;; This file provides the async-lambda macro which transforms
;; code with await forms into CPS (Continuation-Passing Style).
;;
;; Example:
;;   (async-lambda (url)
;;     (let ((response (await (fetch url))))
;;       (await (parse-json response))))
;;
;; Expands to:
;;   (lambda (url)
;;     (js-promise-then (fetch url)
;;       (lambda (response)
;;         (js-promise-then (parse-json response)
;;           (lambda (v) v)))))

;; ============================================================================
;; async-lambda Macro
;; ============================================================================

;; /**
;;  * async-lambda - Define a lambda that can use await to suspend on promises.
;;  * 
;;  * Each (await expr) form becomes a promise-then boundary, transforming
;;  * the remainder of the body into a continuation callback.
;;  *
;;  * @syntax (async-lambda (args ...) body ...)
;;  * @returns {procedure} A procedure that returns a Promise
;;  *
;;  * LIMITATIONS:
;;  * - call/cc captured BEFORE an await can escape the promise chain when invoked
;;  * - call/cc captured AFTER an await only captures the current callback scope
;;  * - TCO works within each callback segment, but not across await boundaries
;;  */
(define-syntax async-lambda
  (syntax-rules ()
    ;; Base case: body with no await - just return the value
    ((async-lambda (args ...) body)
     (lambda (args ...) (js-promise-resolve body)))
    ;; Entry point for multiple expressions  
    ((async-lambda (args ...) expr exprs ...)
     (lambda (args ...)
       (async-body expr exprs ...)))))

;; /**
;;  * async-body - Helper macro to process async body expressions.
;;  * Transforms await forms to promise-then chains.
;;  */
(define-syntax async-body
  (syntax-rules (await let)
    ;; Single expression - wrap in resolved promise
    ((async-body expr)
     (js-promise-resolve expr))
    ;; Let with await in binding - transform to promise-then
    ((async-body (let ((var (await promise-expr))) body ...) rest ...)
     (js-promise-then promise-expr
       (lambda (var)
         (async-body body ... rest ...))))
    ;; Let without await - keep as is, continue processing
    ((async-body (let ((var val)) body ...) rest ...)
     (let ((var val))
       (async-body body ... rest ...)))
    ;; Non-let expression followed by more - sequence them
    ((async-body expr rest ...)
     (js-promise-then (js-promise-resolve expr)
       (lambda (_)
         (async-body rest ...))))))

;; ============================================================================
;; Promise Composition Utilities (Scheme-level)
;; ============================================================================

;; /**
;;  * js-promise-map - Apply a function to the resolved value of a promise.
;;  * @param {procedure} f - Function to apply
;;  * @param {promise} p - Promise to map over
;;  * @returns {promise} A new promise with the mapped value
;;  */
(define (js-promise-map f p)
  (js-promise-then p f))

;; /**
;;  * js-promise-chain - Chain multiple promise-returning functions.
;;  * @param {promise} p - Initial promise
;;  * @param {procedure...} fs - Functions to chain (each takes a value, returns a promise)
;;  * @returns {promise} Final promise in the chain
;;  */
(define (js-promise-chain p . fs)
  (if (null? fs)
      p
      (apply js-promise-chain 
             (js-promise-then p (car fs))
             (cdr fs))))
