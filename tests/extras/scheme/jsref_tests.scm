;;; Tests for JS Property Access Syntax
;;;
;;; Tests the reader's dot notation transformation and the js-ref/js-set! primitives.

;; ============================================================================
;; js-ref Basic Tests
;; ============================================================================

(test-group "js-ref primitive"
  (let ((obj (js-eval "({foo: 42, bar: 'hello'})")))
    (test "js-ref accesses numeric property" 42 (js-ref obj "foo"))
    (test "js-ref accesses string property" "hello" (js-ref obj "bar"))))

;; ============================================================================
;; Dot Notation Reader Syntax Tests
;; ============================================================================

(test-group "dot notation reader syntax"
  (let ((obj (js-eval "({x: 100})")))
    (test "dot notation reads property" 100 obj.x))
  
  (let ((obj (js-eval "({a: {b: {c: 'deep'}}})")))
    (test "chained property access" "deep" obj.a.b.c)))

;; ============================================================================
;; js-set! Basic Tests
;; ============================================================================

(test-group "js-set! primitive"
  (let ((obj (js-eval "({x: 0})")))
    (js-set! obj "x" 42)
    (test "js-set! modifies property" 42 (js-ref obj "x"))))

;; ============================================================================
;; set! with Dot Notation Tests
;; ============================================================================

(test-group "set! with dot notation"
  (let ((obj (js-eval "({y: 0})")))
    (set! obj.y 99)
    (test "set! with dot notation modifies property" 99 obj.y))
  
  (let ((obj (js-eval "({a: {b: 0}})")))
    (set! obj.a.b 55)
    (test "set! with chained dot notation" 55 obj.a.b)))

;; ============================================================================
;; Array Property Access Tests
;; ============================================================================

(test-group "array property access"
  (let ((arr (js-eval "[1, 2, 3, 4, 5]")))
    (test "array length property" 5 arr.length)))

;; ============================================================================
;; Number Preservation Tests
;; ============================================================================

(test-group "numbers with dots remain numbers"
  (test "3.14 is a number" #t (number? 3.14))
  (test "1.5 parsed as number" 1.5 1.5)
  (test "0.5 parsed as number" 0.5 0.5))

;; ============================================================================
;; js-typeof Tests
;; ============================================================================

(test-group "js-typeof primitive"
  ;; Exact integers are BigInt in Scheme-JS
  (test "js-typeof exact integer" "bigint" (js-typeof 42))
  ;; Inexact numbers are JS Number
  (test "js-typeof inexact number" "number" (js-typeof 3.14))
  (test "js-typeof string" "string" (js-typeof "hello"))
  (test "js-typeof boolean" "boolean" (js-typeof #t))
  ;; Note: In this Scheme implementation, lists/vectors might be objects or arrays
  ;; Vectors are arrays -> "object"
  (test "js-typeof vector" "object" (js-typeof #(1 2 3)))
  ;; Functions (Scheme procedures) are callable -> "function"
  (test "js-typeof lambda" "function" (js-typeof (lambda (x) x)))
  ;; Undefined
  (test "js-typeof undefined" "undefined" (js-typeof (js-eval "undefined")))
)

;; ============================================================================
;; js-undefined Tests
;; ============================================================================

(test-group "js-undefined primitive"
  (test "js-undefined is undefined" "undefined" (js-typeof js-undefined))
  (test "compare with raw undefined" #t (eq? js-undefined (js-eval "undefined")))
  (test "compare with null" #f (eq? js-undefined (js-eval "null")))
  ;; Check that it is NOT equal to false
  (test "undefined is not false" #f (eq? js-undefined #f))
)

;; ============================================================================
;; js-undefined? Tests
;; ============================================================================

(test-group "js-undefined? predicate"
  (test "js-undefined? with undefined" #t (js-undefined? js-undefined))
  (test "js-undefined? with raw undefined" #t (js-undefined? (js-eval "undefined")))
  (test "js-undefined? with null" #f (js-undefined? (js-eval "null")))
  (test "js-undefined? with number" #f (js-undefined? 0))
  (test "js-undefined? with false" #f (js-undefined? #f))
)

;; ============================================================================
;; js-null Tests
;; ============================================================================

(test-group "js-null primitive"
  (test "js-null is null" "object" (js-typeof js-null))
  (test "compare with raw null" #t (eq? js-null (js-eval "null")))
  (test "compare with undefined" #f (eq? js-null (js-eval "undefined")))
  ;; Check that it is NOT equal to false
  (test "null is not false" #f (eq? js-null #f))
)

;; ============================================================================
;; js-null? Tests
;; ============================================================================

(test-group "js-null? predicate"
  (test "js-null? with null" #t (js-null? js-null))
  (test "js-null? with raw null" #t (js-null? (js-eval "null")))
  (test "js-null? with undefined" #f (js-null? (js-eval "undefined")))
  (test "js-null? with number" #f (js-null? 0))
  (test "js-null? with false" #f (js-null? #f))
)

;; ============================================================================
;; js-new Tests
;; ============================================================================

(test-group "js-new primitive"
  ;; Test with Date constructor
  (define my-date (js-new Date 2024 0 15))
  (test "js-new Date creates object" "object" (js-typeof my-date))
  (test "js-new Date year" 2024 (my-date.getFullYear))
  (test "js-new Date month" 0 (my-date.getMonth))
  (test "js-new Date day" 15 (my-date.getDate))
  
  ;; Test with Map constructor
  (define my-map (js-new Map))
  (test "js-new Map creates object" "object" (js-typeof my-map))
  (my-map.set "key" "value")
  (test "js-new Map functionality" "value" (my-map.get "key"))
  
  ;; Test with Array constructor
  (define my-array (js-new Array 5))
  (test "js-new Array creates object" "object" (js-typeof my-array))
  (test "js-new Array length" 5 my-array.length)
)
