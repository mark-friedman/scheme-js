;; (scheme-js js-conversion) library
;;
;; Provides deep and shallow conversion procedures between Scheme and JavaScript values,
;; for a program to convert a value itself. The boundary's own conversions -- a JavaScript
;; function's arguments, a Scheme procedure's result returned to JavaScript -- are fixed
;; for each way of calling, and no setting changes them (docs/Interoperability.md).
;;
;; Exports:
;;   scheme->js        - Shallow Scheme to JS conversion
;;   scheme->js-deep   - Deep recursive Scheme to JS conversion
;;   js->scheme        - Shallow JS to Scheme conversion
;;   js->scheme-deep   - Deep recursive JS to Scheme conversion
;;   make-js-object    - Create a new js-object record
;;   js-object?        - Predicate for js-object records
;;   js-ref            - Access a property on a JS object
;;   js-set!           - Set a property on a JS object

(define-library (scheme-js js-conversion)
  (import (scheme base))
  (import (scheme primitives))
  
  (export 
    scheme->js
    scheme->js-deep
    js->scheme
    js->scheme-deep
    
    make-js-object
    js-object?
    js-ref
    js-set!
  )

  (begin
    ;; ------------------------------------------------------------------------
    ;; Record Type: js-object
    ;; ------------------------------------------------------------------------
    ;; A transparent wrapper for JavaScript objects.
    ;; Instances are standard JS objects with a special constructor.
    ;; Fields are dynamic - use js-ref/js-set! to access them.
    
    (define-record-type js-object
      (make-js-object-internal)
      js-object?)
    
    ;; Register the record type with the JS interop system for deep conversion
    (register-js-object-record js-object)
    
    ;; Convenience constructor (currently just wraps the internal one)
    (define (make-js-object)
      (make-js-object-internal))
  )
)
