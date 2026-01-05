
;; (scheme-js js-conversion) library
;;
;; Provides deep and shallow conversion procedures between Scheme and JavaScript values,
;; and the 'js-auto-convert' parameter for controlling boundary conversion.
;;
;; Exports:
;;   scheme->js
;;   scheme->js-deep
;;   js->scheme
;;   js->scheme-deep
;;   js-auto-convert
;;   make-js-object
;;   js-object?
;;   js-ref
;;   js-set!

(define-library (scheme-js js-conversion)
  (import (scheme base))
  (import (scheme write)) ;; for warnings if needed

  ;; Import the JS interop primitives from the runtime
  ;; These are implemented in src/core/interpreter/js_interop.js
  ;; and exposed via library_loader.js as 'scheme-js primitives'
  ;; Wait... we need to expose them as primitives first!
  ;; OR we implement the wrappers here and use %js-interop primitives.
  
  ;; STRATEGY: 
  ;; 1. The functions in js_interop.js are JS functions. 
  ;; 2. We need to register them in the primitive table so Scheme can see them.
  ;; 3. OR we define them here if they are mostly Scheme... but deep conversion is best fast in JS.
  ;; 4. We will assume 'scheme-js-primitives' works similar to 'scheme-primitives'.

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
    ;; Default is #t.
    (define js-auto-convert (make-parameter #t))
    
    ;; ------------------------------------------------------------------------
    ;; Record Type: js-object
    ;; ------------------------------------------------------------------------
    ;; A transparent wrapper for JavaScript objects.
    ;; Instances of this record ARE standard JS objects (with a special constructor).
    ;; Fields are dynamic.
    
    ;; Since we want dynamic fields, we define it with NO fields in the record-type.
    ;; The underlying JS implementation of records allows this.
    ;; But we need to register the constructor with our JS interop logic!
    
    (define-record-type js-object
      (make-js-object-internal) ;; internal constructor
      js-object?)
      
    ;; Helper to create generic js-object with properties from an alist?
    ;; Or just usage: (define obj (make-js-object)) then (js-set! obj "key" val)
    ;; User might want convenience.
    
    ;; We'll export a convenience constructor if needed, but 'make-js-object' 
    ;; effectively just makes an empty one here.
    ;; Let's rename the internal one to expose a better one?
    (define (make-js-object . args)
        (make-js-object-internal))

    ;; ------------------------------------------------------------------------
    ;; Generic Accessors (implemented as Primitives usually, but we can use JS calls)
    ;; ------------------------------------------------------------------------
    
    ;; We use the JS generic object access primitives (or we need to add them).
    ;; Assuming we have basic JS interop or we define them here via %js-eval?
    ;; Actually, specialized primitives are better.
    
    ;; For now, let's assume `js-ref` and `js-set!` are provided by the runtime
    ;; or we define them using `vector-ref` / `vector-set!` hacking? No.
    ;; We likely need to add `js-ref` / `js-set!` to `src/core/primitives/js_interop_primitives.js`?
    ;; Currently `src/core/interpreter/js_interop.js` is pure LOGIC, not Primitives.
  )
)
