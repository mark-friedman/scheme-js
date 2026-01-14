;; Macro Hygiene Tests
;; Tests for R7RS macro hygiene features including free-identifier=? literal matching

(test-group "Macro Hygiene"

  (test-group "Normal Literal Matching"
    ;; Basic cond with else should work
    (test "cond else basic" 'equal
          (cond ((> 3 3) 'greater)
                ((< 3 3) 'less)
                (else 'equal)))

    ;; cond with => should work  
    (test "cond => basic" 2
          (cond ((assv 'b '((a 1) (b 2))) => cadr)
                (else #f)))

    ;; case with else should work
    (test "case else basic" 'consonant
          (case 'x
            ((a e i o u) 'vowel)
            (else 'consonant))))

  ;; R7RS free-identifier=? semantics tests
  (test-group "Free-Identifier Literal Matching"
    ;; When => is locally bound, cond should NOT recognize it as special syntax
    (test "cond with rebound =>" 'ok
          (let ((=> #f)) 
            (cond (#t => 'ok)))))

  (test-group "Macro-Introduced Definitions"
    ;; Test that macro-introduced definitions are visible to nested macros
    ;; This is the jabberwocky/mad-hatter test
    (define-syntax jabberwocky
      (syntax-rules ()
        ((_ hatter)
         (begin
           (define march-hare 42)
           (define-syntax hatter
             (syntax-rules ()
               ((_) march-hare)))))))
    
    (jabberwocky mad-hatter)
    (test "mad-hatter sees march-hare" 42 (mad-hatter)))

  ;; ===========================================================================
  ;; KNOWN FAILING TESTS - These document specific macro hygiene issues
  ;; ===========================================================================

  ;; Test group for nested macro pattern variable capture (bar 1 test)
  ;; Expected to fail until issue is fixed
  ;; 
  ;; Issue: Pattern variable from outer macro is being shadowed by inner macro's
  ;; pattern variable with the same name.
  ;;
  (let ()
    (define-syntax foo
      (syntax-rules ()
        ((foo bar y)
         (define-syntax bar
           (syntax-rules ()
             ((bar x) 'y))))))
    (foo bar x)
    (test "nested pattern capture (bar 1)" 'x (bar 1)))
  ;;
  ;; The outer macro `foo` has pattern variable `y` bound to `x`.
  ;; The inner macro `bar` has pattern variable `x` which shadows the ability
  ;; to reference the outer `y` in the template 'y.
  
  ;; Test group for bound-identifier=? semantics (m k test)
  ;; Expected to fail until issue is fixed
  ;;
  ;; Issue: When a pattern variable from outer macro is used as a literal in
  ;; an inner syntax-rules, the bound-identifier=? comparison should match
  ;; identifiers that are bound identically.
  ;;
  ;; (let-syntax
  ;;     ((m (syntax-rules ()
  ;;           ((m x) (let-syntax
  ;;                      ((n (syntax-rules (k)
  ;;                            ((n x) 'bound-identifier=?)
  ;;                            ((n y) 'free-identifier=?))))
  ;;                    (n z))))))
  ;;   (test 'bound-identifier=? (m k)))  ; Should return 'bound-identifier=?
  ;;
  ;; When (m k) is called:
  ;; - x is bound to k (as pattern variable)
  ;; - n has k as a literal in its syntax-rules
  ;; - In pattern (n x), x refers to the pattern variable k
  ;; - (n z) is called with identifier z
  ;; - Should match (n x) pattern because x == k == literal k
  ;; - But we get free-identifier=?, meaning z didn't match the x pattern

  ;; ===========================================================================
  ;; PURE MARKS HYGIENE TESTS
  ;; Tests validating the pure marks/scopes approach (no gensyms)
  ;; ===========================================================================

  (test-group "Pure Marks: Scope Isolation"
    ;; Test 1: User variable with same name as introduced binding
    ;; The macro's 'tmp' should be distinct from user's 'tmp'
    (define-syntax swap-test!
      (syntax-rules ()
        ((swap-test! a b)
         (let ((tmp a))
           (set! a b)
           (set! b tmp)))))
    
    (let ((x 1) (y 2) (tmp 100))
      (swap-test! x y)
      ;; After swap: x=2, y=1, tmp should still be 100
      (test "user tmp not captured by swap" 100 tmp)
      (test "swap-test x value" 2 x)
      (test "swap-test y value" 1 y))

    ;; Test 2: Multiple expansions of same macro get different scopes
    (define-syntax inc-temp
      (syntax-rules ()
        ((inc-temp val)
         (let ((temp val))
           (+ temp 1)))))
    
    (test "multiple expansions independent 1" 11 (inc-temp 10))
    (test "multiple expansions independent 2" 21 (inc-temp 20))
    (test "nested expansions" 23 (+ (inc-temp 10) (inc-temp 11))))

  (test-group "Pure Marks: Nested Macro Scope Isolation"
    ;; Test 3: Hygiene prevents macro bindings from leaking into user code
    ;; The macro introduces 'x', but user's 'body' shouldn't see it
    (define-syntax with-x
      (syntax-rules ()
        ((with-x val body)
         (let ((x val))
           body))))
    
    ;; User's 'x' in body should NOT see macro's 'x' binding
    ;; This verifies hygiene is working correctly
    (define x 100)
    (test "macro binding doesn't leak to user body"
          100  ; User's x, not macro's x
          (with-x 42 x))
    
    ;; But macro can reference its own x internally
    (define-syntax with-x-squared
      (syntax-rules ()
        ((with-x-squared val)
         (let ((x val))
           (* x x)))))
    
    (test "macro can use its own introduced binding"
          16
          (with-x-squared 4)))


  (test-group "Pure Marks: Referential Transparency"
    ;; Test 4: Free variables resolve at definition site, not use site
    (define-syntax double
      (syntax-rules ()
        ((double x) (+ x x))))
    
    ;; Even with + shadowed, macro's + should refer to global +
    (test "referential transparency with shadowed +"
          10
          (let ((+ *))
            (double 5)))

    ;; Test 5: Macro using another procedure that's later shadowed
    (define (helper x) (* x 10))
    (define-syntax use-helper
      (syntax-rules ()
        ((use-helper v) (helper v))))
    
    (test "referential transparency with helper"
          50
          (let ((helper (lambda (x) x)))
            (use-helper 5))))
)


