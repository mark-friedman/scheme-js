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

  (test-group "js-auto-convert Parameter"
    (test "default value is 'deep" 
          (js-auto-convert) 
          'deep)
    
    (test "parameterize can change value to 'shallow"
          (parameterize ((js-auto-convert 'shallow))
            (js-auto-convert))
          'shallow)

    (test "parameterize can change value to 'raw"
          (parameterize ((js-auto-convert 'raw))
            (js-auto-convert))
          'raw)
    
    (test "value restored after parameterize"
          (begin
            (parameterize ((js-auto-convert 'shallow))
              'ignored)
            (js-auto-convert))
          'deep)
  )

  (test-group "js-object Record Type"
    (let ((obj (make-js-object)))
      (test "make-js-object creates object"
            (js-object? obj)
            #t)
      
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
      
      (js-set! (js-eval "globalThis") "tmpObj" obj)
      (js-set! obj "big" 100)
      (test "js-set! performs conversion (stored as number)"
            (js-eval "typeof globalThis.tmpObj.big")
            "number")
    )
  )

  (test-group "js-obj conversion"
    (begin
      (js-set! (js-eval "globalThis") "tmpObj2" (js-obj "val" 42))
      (test "js-obj performs conversion"
            (js-eval "typeof globalThis.tmpObj2.val")
            "number"))
  )
)
