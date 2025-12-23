
(import (scheme base) (scheme write))

(define-syntax test-group
  (syntax-rules ()
    ((_ name body ...)
     (begin 
       (display "Running Group: ") (display name) (newline)
       body ...))))

(define-syntax test
  (syntax-rules ()
    ((_ expected expr)
     (let ((res expr))
       (if (equal? res expected)
           (begin (display "PASS: ") (write res) (newline))
           (begin (display "FAIL: expected ") (write expected) (display ", got ") (write res) (newline)))))))

(test-group "Hygiene: Macro-introduced Bindings"
  ;; This test exercises the case where a macro attempts to bind a variable
  ;; (in this case 'e') that is NOT present in the macro call, but generated
  ;; from the macro template.
  
  (define-syntax test-binding-visibilty
    (syntax-rules ()
      ((_ val)
       (let ((e val))
         e))))

  (test 42 (test-binding-visibilty 42))

  ;; A slightly more complex case involving 'guard' pattern 
  ;; (Simplified guard macro usage simulation if guard is not fully supported or to isolate)
  ;; Actually utilizing (scheme base) 'guard' should work now.
  
  (define-syntax test-guard-binding
    (syntax-rules ()
      ((_ expr)
       (guard (e (else 
                  (begin 
                    (display "Caught e: ") (write e) (newline)
                    (if (string? e) e "not-string"))))
         expr))))

  (test "error" (test-guard-binding (raise "error")))

  ;; Test Hypothesis: Global definition triggers the bug
  (define e "global-e")
  
  (define-syntax test-shadowing-bug
    (syntax-rules ()
      ((_ expr)
       (let ((e expr)) e)))) ;; Should return expr, not "global-e"

  (test "local" (test-shadowing-bug "local"))
)
