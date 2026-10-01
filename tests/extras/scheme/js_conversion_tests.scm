;;; Tests for (scheme-js js-conversion) library
;;;
;;; Tests the conversion procedures and js-object record type.

(test-group "JS Conversion Library"
  
  (test-group "Shallow Conversions"
    ;; scheme->js converts BigInt to Number for safe integers
    (test "scheme->js converts exact integer" 
          (= (scheme->js 42) 42) 
          #t)
    
    ;; js->scheme converts integer Numbers to BigInt
    (test "js->scheme preserves number" 
          (js->scheme 42) 
          42)
  )

  (test-group "Deep Conversions"
    ;; scheme->js-deep converts vectors to arrays
    (test "scheme->js-deep converts vector"
          (let ((arr (scheme->js-deep #(1 2 3))))
            (and (= (js-ref arr "length") 3)
                 (= (js-ref arr "0") 1)))
          #t)
    
    ;; js->scheme-deep converts arrays to vectors
    (test "js->scheme-deep converts array"
          (js->scheme-deep (js-eval "[1, 2, 3]"))
          #(1 2 3))
    
    ;; Nested structures
    (test "js->scheme-deep handles nested arrays"
          (js->scheme-deep (js-eval "[[1, 2], [3, 4]]"))
          #(#(1 2) #(3 4)))
  )

  ;; What crosses the boundary is converted as the call says -- by these
  ;; procedures, and by which entry JavaScript calls a Scheme procedure
  ;; through -- and never by dynamic state, so the library has no parameter
  ;; for it. It once exported one, `js-auto-convert`, that nothing read, so
  ;; that `parameterize` of it silently changed nothing.
  (test-group "No conversion parameter"
    (test "the library exports no js-auto-convert"
          #f
          (guard (e (#t #f))
            js-auto-convert
            #t))
  )

  (test-group "js-object Record Type"
    (let ((obj (make-js-object)))
      (test "make-js-object creates object"
            #t
            (js-object? obj))
      
      (test "js-object? returns false for other types"
            (js-object? '())
            #f)
      
      (test "js-object? returns false for vectors"
            (js-object? #(1 2 3))
            #f)
      
      ;; Property access
      (js-set! obj "x" 42)
      (test "js-ref after js-set!"
            (js-ref obj "x")
            42)
      
      (js-set! obj "name" "test")
      (test "js-ref for string property"
            (js-ref obj "name")
            "test")
    )
  )

  (test-group "js-ref and js-set!"
    (let ((obj (js-eval "({a: 1, b: 2})")))
      (test "js-ref reads existing property"
            (js-ref obj "a")
            1)
      
      (js-set! obj "a" 100)
      (test "js-set! modifies property"
            (js-ref obj "a")
            100)
      
      (js-set! obj "c" 3)
      (test "js-set! creates new property"
            (js-ref obj "c")
            3)
      
      (js-set! obj "big" 100)
      (test "js-set! preserves exactness"
            (integer? (js-ref obj "big"))
            #t)
    )
  )

  (test-group "js-obj exactness"
    (let ((obj (js-obj "val" 42)))
      (test "js-obj preserves exactness"
            (integer? (js-ref obj "val"))
            #t))
  )

  (test-group "Boundary Conversion (js-invoke)"
    (test "js-invoke converts to Number for native call"
          (js-invoke (js-eval "({check: (v) => typeof v})") "check" 42)
          "number")
  )
)
