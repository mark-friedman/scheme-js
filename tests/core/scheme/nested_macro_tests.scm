;; Tests for macro hygiene: nested macro definitions
;; Note: test-protect wraps expressions in guard which creates an expression
;; context. Macro-introduced defines need a body context, so we use let().

(test-group "nested macro hygiene"

  ;; Simplest case: macro introduces a value via pattern variable define
  ;; Use let() to create body context where macro-expanded define is valid
  (let ()
    (define-syntax def-test
      (syntax-rules ()
        ((_ name val)
         (define name val))))
    (def-test my-val 42)
    (test 42 my-val))
  
  ;; Test macro-introduced hidden binding (should be renamed for hygiene)
  (let ()
    (define-syntax hidden-def
      (syntax-rules ()
        ((_)
         (begin
           (define hidden 99)
           hidden))))
    (test 99 (hidden-def)))
  
  ;; Mad-hatter pattern: macro defines both a variable and nested macro
  (let ()
    (define-syntax jabberwocky
      (syntax-rules ()
        ((_ hatter)
         (begin
           (define march-hare 42)
           (define-syntax hatter
             (syntax-rules ()
               ((_) march-hare)))))))
    (jabberwocky mad-hatter)
    (test 42 (mad-hatter)))
)
