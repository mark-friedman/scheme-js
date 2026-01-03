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
