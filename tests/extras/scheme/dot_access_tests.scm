
(import (scheme base)
        (scheme-js interop))

(test-group "extended-dot-notation"

  ;; 1. String literal property access
  (test "string propery" 3 "abc".length)
  
  ;; 3. Vector property access
  (test "vector property" 3 #(1 2 3).length)

  ;; 2. Expression property access
  ;; (vector 1 2) returns a JS Array, so .length is 2.
  (test "expression property" 2 (vector 1 2).length)
  
  ;; 4. JS Object Literal property access (#{})
  (test "#{...}.prop" "val" #{("a" "val")}.a)
  
  ;; 5. Nested access
  (test "nested access" 123 #{(a #{(b 123)})}.a.b)
  
  ;; 6. WHITESPACE SENSITIVITY CHECK
  ;; "abc" .length (with space) should NOT be property access.
  ;; It should read as "abc" followed by symbol .length
  ;; To test this, we define .length as a symbol and see if we can read/eval it separately.
  
  (define .len 'my-symbol)
  (test "separate .symbol" 'my-symbol (begin "ignore" .len))
  
  ;; (list 1 .len) should be (1 .len) list of 2 items
  (test "list with separate .symbol" '(1 my-symbol) (list 1 .len))
  
  ;; 7. Chained access with whitespace break
  ;; #{...}.a .b -> returns property a, then symbol .b
  (define .b 'sym-b)
  (test "chained break" 'sym-b (begin #{("a" 1)}.a .b))
  
  ;; 9. Mixed with other syntax
  (define abc "abc")
  (test "variable prop" 3 abc.length)

  ;; Quoted symbol with dot -> expands to list structure
  (test "quoted symbol prop" '(js-ref abc "length") 'abc.length)

)
