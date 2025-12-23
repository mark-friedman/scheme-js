;; Hygiene Tests
;; Tests for macro hygiene fixes including:
;; - Referential transparency (macros resolving internal bindings)
;; - let-syntax and letrec-syntax scoping

;; ============================================================================
;; Referential Transparency Tests
;; These test that macros can reference bindings from their definition site,
;; even if those bindings aren't exported from the library.
;; ============================================================================

;; The parameterize macro references param-dynamic-bind internally.
;; This should work without param-dynamic-bind being exported.
(test "parameterize internal binding" 
  (let ((p (make-parameter 10)))
    (parameterize ((p 42))
      (p)))
  42)

(test "parameterize restores after exit"
  (let ((p (make-parameter 5)))
    (parameterize ((p 100))
      #f)
    (p))
  5)

(test "nested parameterize"
  (let ((p (make-parameter 1)))
    (parameterize ((p 10))
      (parameterize ((p 100))
        (p))))
  100)

;; ============================================================================
;; let-syntax Tests
;; These test that let-syntax properly propagates the syntactic environment
;; and that nested let-syntax can see macros from outer scopes.
;; ============================================================================

(test "let-syntax basic"
  (let-syntax ((double (syntax-rules ()
                         ((double x) (* 2 x)))))
    (double 21))
  42)

(test "let-syntax shadowing"
  (let-syntax ((foo (syntax-rules ()
                      ((foo) 'outer))))
    (let-syntax ((foo (syntax-rules ()
                        ((foo) 'inner))))
      (foo)))
  'inner)

(test "let-syntax nested with different macros"
  (let-syntax ((add1 (syntax-rules ()
                       ((add1 x) (+ x 1)))))
    (let-syntax ((add2 (syntax-rules ()
                         ((add2 x) (+ x 2)))))
      (+ (add1 10) (add2 20))))
  33)

(test "let-syntax outer macro visible in inner scope"
  (let-syntax ((outer (syntax-rules ()
                        ((outer) 100))))
    (let-syntax ((inner (syntax-rules ()
                          ((inner) 200))))
      (+ (outer) (inner))))
  300)

;; ============================================================================
;; letrec-syntax Tests  
;; These test that letrec-syntax works for scoped macro definitions.
;; ============================================================================

(test "letrec-syntax basic"
  (letrec-syntax ((double (syntax-rules ()
                            ((double x) (* 2 x)))))
    (double 5))
  10)

(test "letrec-syntax count elements"
  (letrec-syntax ((count (syntax-rules ()
                           ((count) 0)
                           ((count x) 1)
                           ((count x y) 2)
                           ((count x y z) 3))))
    (list (count) (count a) (count a b) (count a b c)))
  '(0 1 2 3))

;; ============================================================================
;; Macro Hygiene Edge Cases
;; ============================================================================

;; Test that locally-defined macro doesn't capture bindings from use site
(test "macro doesn't capture local binding"
  (let ()
    (define-syntax square-it
      (syntax-rules ()
        ((square-it x)
         (let ((tmp x))
           (* tmp tmp)))))
    (let ((tmp 10))
      (square-it 5)))
  25)  ;; tmp inside macro doesn't conflict with outer tmp

