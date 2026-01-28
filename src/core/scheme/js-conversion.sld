;; (scheme-js js-conversion) library
;;
;; Provides deep and shallow conversion procedures between Scheme and JavaScript values,
;; and the 'js-auto-convert' parameter for controlling boundary conversion.
;;
;; Exports:
;;   scheme->js        - Shallow Scheme to JS conversion
;;   scheme->js-deep   - Deep recursive Scheme to JS conversion
;;   js->scheme        - Shallow JS to Scheme conversion
;;   js->scheme-deep   - Deep recursive JS to Scheme conversion
;;   js-auto-convert   - Parameter controlling automatic deep conversion
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
    js-auto-convert
    
    make-js-object
    js-object?
    js-ref
    js-set!
  )

  (begin
    ;; ------------------------------------------------------------------------
    ;; Parameter: js-auto-convert
    ;; ------------------------------------------------------------------------
    ;; Controls whether automatic deep conversion happens at JS boundaries.
    ;; Possible values: 'deep, 'shallow, 'raw.
    ;; Default is 'deep.
    (define js-auto-convert (make-parameter 'deep))
    
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
